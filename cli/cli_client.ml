
open Lwt

open LTerm_style
open LTerm_text
open LTerm_geom
open CamomileLibraryDyn.Camomile
open React

open Cli_state

let rec take x l acc =
  match x, l with
  | 0, _       -> List.rev acc
  | n, x :: xs -> take (pred n) xs (x :: acc)
  | _, _       -> assert false

let rec drop x l =
  match x, l with
  | 0, xs      -> xs
  | n, _ :: xs -> drop (pred n) xs
  | _, []      -> []

let pad_l_rev neutral x l =
  let rec doit xs =
      match x - (List.length xs) with
      | 0            -> xs
      | d when d > 0 -> doit (neutral :: xs)
      | _            -> drop (List.length xs - x) xs
  in
  doit l

let pad_l neutral len xs =
  List.rev (pad_l_rev neutral len (List.rev xs))

let pad x s =
  match x - (Zed_utf8.length s) with
  | 0                  -> s
  | d when d > 0       -> s ^ (String.make d ' ')
  | _ (* when d < 0 *) -> Zed_utf8.sub s 0 x

let buddy_to_color = function
  | `Default -> default
  | `Good -> green
  | `Bad -> red

let line_wrap_with_tags ?raw ~(tags : 'a list) ~max_length entries : ('a * Zed_utf8.t) list =
  let pre = match raw with
    | None   -> "  "
    | Some _ -> ""
  in
  let rec worker (acc : ('a * Zed_utf8.t) list) = function
    | (tag, entry) :: remaining when String.contains entry '\n' ->
       let remaining = match Astring.String.cut ~sep:"\n" entry with
         | None -> assert false (* this can never happen *)
         | Some (part1, part2) ->
            let part2 = pre ^ part2 in
            match raw with
            | Some _ -> (tag, part2) :: (tag, part1) :: remaining
            | None   ->
               match String.trim part1 = "", String.trim part2 = "" with
               | true , true  -> remaining
               | false, true  -> (tag, part1) :: remaining
               | true , false -> (tag, part2) :: remaining
               | false, false -> (tag, part2) :: (tag, part1) :: remaining
       in
       worker acc remaining
    | (tag, entry) :: remaining when (Zed_utf8.length entry) > max_length ->
      let part1, part2 =
        let p1, p2 = Zed_utf8.break entry max_length in
        match raw with
        | None ->
          let n = 10 in
          let last_10 = Zed_utf8.sub p1 (max_length - n) n in
          let rec find_space idx =
            match Zed_utf8.get last_10 idx with
            | x when x = UChar.of_int 0x20 -> Some idx
            | _ when idx = 0               -> None
            | _                            -> find_space (pred idx)
          in
          (match find_space 10 with
           | None   -> (p1, pre ^ String.trim p2)
           | Some x ->
              let p1, p2 = Zed_utf8.break entry (x + (max_length - n)) in
              (p1, pre ^ String.trim p2))
        | Some _ -> (p1, p2)
      in
      worker acc ((tag, part2) :: (tag, part1) :: remaining)
    | (tag, entry) :: remaining ->
      worker ((tag, entry) :: acc) remaining
    | [] -> acc
  in
  worker [] (List.combine tags entries)

let line_wrap ?raw ~max_length entries : Zed_utf8.t list =
  let data =
    let tags = List.map (fun _ -> `Default) entries in
    List.split (line_wrap_with_tags ?raw ~tags ~max_length entries)
  in
  snd data

let print_time ~now ~tz_offset_s timestamp =
  let daydiff, _ = Ptime.Span.to_d_ps (Ptime.diff now timestamp) in
  let (_, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ~tz_offset_s timestamp in
  if daydiff = 0 then (* less than a day ago *)
    Printf.sprintf "%02d:%02d:%02d " hh mm ss
  else
    Printf.sprintf "%02d-%02d %02d:%02d " m d hh mm

let format_log tz_offset_s now log =
  let print_log { User.direction ; timestamp ; message ; _ } =
    let time = print_time ~now ~tz_offset_s timestamp in
    let from = match direction with
      | `From jid -> Xjid.jid_to_string jid ^ ":"
      | `Local (_, x) when x = "" -> "***"
      | `Local (_, x) -> "*** " ^ x ^ " ***"
      | `To _ -> ">>>"
    in
    time ^ from ^ " " ^ message
  in
  List.map print_log log

let format_buddies state buddies width =
  let env jid =
    if isactive state jid then
      ([ B_reverse true ], [ E_reverse ])
    else if isnotified state jid then
      ([ B_blink true ], [ E_blink ])
    else
      ([], [])
  and notify_char buddy jid =
    match isnotified state jid, Contact.expanded buddy with
    | true, true -> "*"
    | false, false ->
       if potentially_visible_resource state buddy then "+" else " "
    | true, false -> Zed_utf8.singleton (UChar.of_int 0x2600)
    | false, true -> " "
  and color buddy resource =
    buddy_to_color (Contact.color buddy resource)
  in

  let draw (print : string) (b : Contact.contact) (r : Contact.resource option) =
    let jid = Contact.jid b r in
    let pr, po = env jid
    and fst = notify_char b jid
    and color = color b r
    in
    let data = pad width (fst ^ print) in
    pr @ [ B_fg color ; S data ; E_fg ] @ po
  in

  List.fold_right
    (fun (c, res) acc ->
     let r = if Contact.expanded c then None else Contact.active c in
     draw (Contact.oneline c None) c r ::
       List.map (fun r -> draw (Contact.oneline c (Some r)) c (Some r)) res @
       acc)
    buddies []

let format_messages tz_offset_s now buddy resource jid msgs =
  let printmsg { User.direction ; encrypted ; received ; timestamp ; message ; _ } =
    let time = print_time ~now ~tz_offset_s timestamp in
    let msg_color, pre =
      match buddy with
      | `Room _ ->
         (match direction with
          | `From (`Full (_, nick)) -> (`Highlight, nick ^ ": ")
          | `From (`Bare _) -> (`Highlight, " ")
          | `Local (_, x) -> (`Default, "***" ^ x ^ " ")
          | `To _ -> (`Default, if received then "-> " else "?> ")
         )
      | `User _ ->
         let en = if encrypted then "O" else "-" in
         let msg_color, pre = match direction with
           | `From _ -> (`Highlight, "<" ^ en ^ "- ")
           | `To _   -> let f = if received then "-" else "?" in
                        (`Default, f ^ en ^ "> ")
           | `Local (_, x) when x = "" -> (`Default, "*** ")
           | `Local (_, x) -> (`Default, "***" ^ x ^ "*** ")
         in
         let r =
           let show_res =
             let other = User.jid_of_direction direction in
             let other_resource s = match Xjid.resource other with
               | None -> None
               | Some x when x = s.User.resource -> None
               | Some x -> Some x
             in
             match resource with
             | Some (`Session s) -> other_resource s
             | _ -> Xjid.resource other
           in
           Utils.option "" (fun x -> "(" ^ x ^ ") ") show_res
         in
         (msg_color, r ^ pre)
    in
    (msg_color, time ^ pre ^ message)
  in
  let tst o =
    if Contact.expanded buddy then
      match buddy, jid with
      | `Room _, _ -> true
      | `User _, `Bare _ -> true
      | `User _, `Full _ -> Xjid.jid_matches o jid
    else
      true
  in
  List.map printmsg
    (List.filter (fun m -> tst (User.jid_of_direction m.User.direction)) msgs)

let buddy_list state length width =
  let buddies = active_contacts_resources state in
  let formatted_buddies = format_buddies state buddies width in

  let flattened = show_resources buddies in
  let bs = List.length flattened
  and up, down = (length / 2, (length + 1) / 2)
  and active_idx = Utils.find_index state.active_contact 0 flattened
  in
  match length >= bs with
  | true  -> let pad = [ S (String.make width ' ') ] in
             pad_l pad length formatted_buddies
  | false ->
    let from =
      match
        active_idx - up >= 0,
        active_idx + down > bs
      with
      | true , true  -> bs - length
      | true , false -> active_idx - up
      | false, _     -> 0
    in
    take length (drop from formatted_buddies) []

let maybe_trim str left =
  if left > 0 then
    if Zed_utf8.length str < left then
      ([ S str ], left - Zed_utf8.length str)
    else
      ([ S (Zed_utf8.sub str 0 left) ], 0)
  else
    ([], 0)

let horizontal_line buddy resource fg_color buddy_width scrollback show_buddy_list width =
  let otrcolor, otr =
    match buddy, resource with
     | `User user, Some (`Session s) ->
        Utils.option
          (`Bad, " - no OTR - ")
          (fun fp ->
           let vs = User.verified_fp user fp in
           (User.verification_status_to_color vs,
            " - " ^ User.verification_status_to_string vs ^ " - "))
          (User.otr_fingerprint s.User.otr)
    | _ -> (Contact.color buddy resource, " ")
  in
  let pre =
    if show_buddy_list then
      (Zed_utf8.make buddy_width (UChar.of_int 0x2500)) ^
      (Zed_utf8.singleton (UChar.of_int 0x2534))
    else
      (Zed_utf8.singleton (UChar.of_int 0x2500))
  in

  let presence, status =
    let tr (p, s) =
      let st x = Utils.option (" - " ^ x) (fun (a, _) -> " - " ^ a) (Astring.String.cut ~sep:"\n" x) in
      (User.presence_to_string p, Utils.option "" st s)
    in
    Utils.option
      ("", "")
      (function
        | `Session s -> tr (s.User.presence, s.User.status)
        | `Member m -> tr (m.Muc.presence, m.Muc.status))
      resource
  in
  let txt =
    let jid =
      let id = Contact.jid buddy resource in
      Xjid.jid_to_string id
    in
    match scrollback, buddy with
    | 0, `User _ -> " buddy: " ^ jid
    | 0, `Room _ -> " room: " ^ jid
    | _, _ -> "*scroll*" ^ jid
  in
  (* now we have the building blocks and might need to cut some of them down *)
  let buddy, left = maybe_trim txt width in
  let otr, left = maybe_trim otr left in
  let presence, left = maybe_trim presence left in
  let status, left = maybe_trim status left in
  let pre, left = maybe_trim pre left in
  let post = if left > 0 then [S (Zed_utf8.make left (UChar.of_int 0x2500))] else [] in
  B_fg fg_color :: pre @ buddy @ [ E_fg ; B_fg (buddy_to_color otrcolor) ] @ otr @ [ E_fg ; B_fg fg_color ] @ presence @ status @ post @ [ E_fg ]

let status_line self mysession notify log redraw fg_color width =
  let status = User.presence_to_string mysession.User.presence in
  let jid = User.userid self mysession in

  let status_color =
    if mysession.User.presence = `Offline then
      lred
    else
      lgreen
  in

  let jid, left = maybe_trim jid (width - 2) in
  let jid_pre, left = maybe_trim "< " left in
  let jid_post, left = maybe_trim " >─" left in

  let styled_jid = if log then B_reverse true :: jid @ [ E_reverse ] else jid in

  let status, left = maybe_trim status left in
  let status_pre, left = maybe_trim "[ " left in
  let status_post, left = maybe_trim " ]─" left in

  let redraw, left = maybe_trim (Printf.sprintf "%02x" redraw) left in

  let fill = if left > 0 then [S (Zed_utf8.make left (UChar.of_int 0x2500))] else [] in

  let first =
    let dash = S (Zed_utf8.make 1 (UChar.of_int 0x2500)) in
    if notify then
      [ B_bold true ; B_blink true ; B_fg cyan ; S "#" ] @ redraw @ [ dash ; E_fg ; E_blink ]
    else
      [ B_bold true ; B_fg fg_color ; dash ] @ redraw @ [ dash ; E_fg ]
  in

  first @
  [ B_fg fg_color ] @ jid_pre @ styled_jid @ jid_post @
  fill @
  status_pre @ [ E_fg ; B_fg status_color ] @ status @ [ E_fg ; B_fg fg_color ] @ status_post @
  [ E_fg ; E_bold ]

let tz_offset_s () =
  match Ptime_clock.current_tz_offset_s () with
  | None -> 0 (* XXX: report error *)
  | Some x -> x

let make_prompt size network state redraw =
  (* network should be an event, then I wouldn't need a check here *)
  (if state.last_status <> network then
     (add_status state (fst network) (snd network) ;
      state.last_status <- network)) ;

  (* we might have gotten a connection termination - if so mark everything offline *)
  (match fst network with
   | `Local (_, err) ->
     let err_prefix = try String.sub err 0 11 with Invalid_argument _ -> "" in
     (match err_prefix, !xmpp_session with
      | (x, None) when x = "async error" || x = "session err" ->
         let buddies = Contact.fold (fun _ b acc -> Contact.reset b :: acc) state.contacts [] in
         List.iter (Contact.replace_contact state.contacts) buddies ;
         Lwt.async (fun () -> Lwt_mvar.put state.state_mvar Disconnected)
      | _ -> ())
   | _ -> ()) ;

  let log_size =
    if state.log_height + 10 > size.rows then
      0
    else
      state.log_height
  in
  let main_size =
    if log_size = 0 then
      size.rows - 2
    else
      size.rows - log_size - 3 (* status + hline + readline *)
  in
  let buddy_width = state.buddy_width in
  let chat_width =
    match state.window_mode with
    | BuddyList        -> size.cols - buddy_width - 1
    | FullScreen | Raw -> size.cols
  in

  if main_size <= 4 || chat_width <= 20 then
    eval [S "need more space"]
  else
    let now = Ptime_clock.now ()
    and tz_offset_s = tz_offset_s ()
    and self = self state
    and mysession = selfsession state
    in
    let isself = Xjid.jid_matches (`Bare (fst state.config.Xconfig.jid)) state.active_contact in
    let statusses = self.User.message_history in
    let logs =
      let entries =
        if List.length statusses > log_size then
          take log_size statusses []
        else
          statusses
      in
      let entries =
        let entries = format_log tz_offset_s now entries in
        line_wrap ~max_length:size.cols entries
      in
      let data = pad_l_rev "" log_size entries in
      String.concat "\n" data
    in

    let active = active state in
    let resource = resource state in

    let fg_color = buddy_to_color (Contact.color active resource) in

    let main_window =
      let msg_colors, data =
        let msgs =
          if isself then
            List.map (fun s -> `Default, s) (format_log tz_offset_s now statusses)
          else
            format_messages tz_offset_s now active resource state.active_contact (Contact.messages active)
        in
        List.split msgs
      in
      let scroll default lines =
        (* data is already in right order -- but we need to strip scrollback *)
        let elements = drop (state.scrollback * main_size) (List.rev lines) in
        pad_l_rev default main_size (List.rev elements)
      in
      let render_msg (color, line) =
        let data = S (line ^ "\n") in
        match color with
        | `Default   -> [ data ]
        | `Highlight -> [ B_bold true ; data ; E_bold ]
      in
      match state.window_mode with
      | BuddyList ->
         let chat = line_wrap_with_tags ~max_length:chat_width ~tags:msg_colors data in
         let chat = scroll (`Default, "") chat in
         let buddies = buddy_list state main_size buddy_width in
              let comb = List.combine buddies chat in
              let pipe = S (Zed_utf8.singleton (UChar.of_int 0x2502)) in
              List.map
                (fun (buddy, chat) ->
                 buddy @ [ B_fg fg_color ; pipe ; E_fg ] @ (render_msg chat))
                comb

           | FullScreen ->
              let chat = line_wrap_with_tags ~max_length:chat_width ~tags:msg_colors data in
              let chat = scroll (`Default, "") chat in
              List.map render_msg chat

           | Raw ->
              let data =
                List.map
                  (fun x -> x.User.message)
                  (List.filter
                     (fun x -> match x.User.direction with
                               | `Local _ -> false
                               | `From _ -> true
                               | `To _ -> false)
                     (Contact.messages active))
              in
              let wrapped = line_wrap ~raw:() ~max_length:chat_width data in
              let chat = scroll "" wrapped in
              List.map (fun x -> [ S x ; S "\n" ]) chat
         in

         let showing_buddies = match state.window_mode with
           | BuddyList -> true
           | FullScreen | Raw -> false
         in
         let hline =
           horizontal_line
             active resource fg_color buddy_width
             state.scrollback showing_buddies size.cols
         in

         let notify = List.length state.notifications > 0 in
         let log = Contact.preserve_messages active in
         let status = status_line self mysession notify log redraw fg_color size.cols in
         let main = List.flatten main_window in

         try
           if log_size = 0 then
             eval ( main @ status @ [ S "\n" ] )
           else
             eval ( main @ hline @ [ S "\n" ; S logs ; S "\n" ] @ status @ [ S "\n" ] )
         with
           _ -> eval ([ S "error during evaluating layout"])

let up = UChar.of_int 0x2500
let down = UChar.of_int 0x2501
let f5 = UChar.of_int 0x2502
let f12 = UChar.of_int 0x2503
let ctrlq = UChar.of_int 0x2504
let ctrlx = UChar.of_int 0x2505
let ctrlup = UChar.of_int 0x2506
let ctrldown = UChar.of_int 0x2507
let f11 = UChar.of_int 0x2508
let shift_f11 = UChar.of_int 0x2509
let f10 = UChar.of_int 0x2510
let shift_f10 = UChar.of_int 0x2511


let redraw, force_redraw =
  (* this is just an ugly hack which should be removed *)
  let a, b = S.create 0 in
  (a, fun () -> b (Random.int 256) )

type direction = Up | Down

let navigate_message_buffer state direction =
  match
    direction,
    state.scrollback
  with
  | Down, 0 -> ()
  | Down, n ->
    state.scrollback <- n - 1 ;
    if state.scrollback = 0 then notified state ;
    force_redraw ()
  | Up, n -> state.scrollback <- n + 1; force_redraw ()

let navigate_buddy_list state direction =
  let userlist = all_jids state in
  let set_active idx =
    let user = List.nth userlist idx in
    activate_contact state user ;
    force_redraw ()
  and active_idx = Utils.find_index state.active_contact 0 userlist
  in
  match
    direction,
    List.length userlist > succ active_idx,
    pred active_idx >= 0
  with
  | Down, true , _     -> set_active (succ active_idx)
  | Down, false, _     -> set_active 0
  | Up  , _    , true  -> set_active (pred active_idx)
  | Up  , _    , false -> set_active (pred (List.length userlist))

class read_line ~term ~network ~history ~state ~input_buffer = object(self)
  inherit LTerm_read_line.read_line ~history () as super
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method! completion =
    let prefix  = Zed_rope.to_string self#input_prev in
    let completions = Cli_commands.completion prefix in
    self#set_completion 0 completions

  method! complete =
    try super#complete with
    | _ -> ()

  method! show_box = false

  method set_input_buffer s =
    Zed_edit.goto_bot self#context ;
    Zed_edit.delete_next_line self#context ;
    Zed_edit.insert_no_erase self#context s

  method save_input_buffer =
    let saved_input_buffer = self#eval
    and u = active state
    in
    Contact.replace_contact state.contacts (Contact.set_saved_input_buffer u saved_input_buffer)

  method! send_action = function
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = down ->
      self#save_input_buffer ;
      navigate_buddy_list state Down ;
      super#send_action LTerm_read_line.Break
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = up ->
      self#save_input_buffer ;
      navigate_buddy_list state Up ;
      super#send_action LTerm_read_line.Break
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = ctrldown ->
      navigate_message_buffer state Down
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = ctrlup ->
      navigate_message_buffer state Up
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = f5 ->
      state.show_offline <- not state.show_offline ;
      force_redraw ()
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = f12 ->
      state.window_mode <- next_display_mode state.window_mode ;
      force_redraw ()
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = f11 ->
      state.buddy_width <- succ state.buddy_width ;
      force_redraw ()
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = shift_f11 ->
      state.buddy_width <- max (pred state.buddy_width) 0 ;
      force_redraw ()
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = f10 ->
      state.log_height <- succ state.log_height ;
      force_redraw ()
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = shift_f10 ->
      state.log_height <- max (pred state.log_height) 0 ;
      force_redraw ()
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = ctrlq ->
      if List.length state.notifications > 0 then
        (self#save_input_buffer ;
         activate_contact state (List.hd (List.rev state.notifications)) ;
         force_redraw () ;
         super#send_action LTerm_read_line.Break )
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = ctrlx ->
      self#save_input_buffer ;
      activate_contact state state.last_active_contact ;
      force_redraw () ;
      super#send_action LTerm_read_line.Break
    | action ->
      super#send_action action

  initializer
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = false; code = Prev_page })] [Edit (LTerm_edit.Zed (Zed_edit.Insert up))]);
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = false; code = Next_page })] [Edit (LTerm_edit.Zed (Zed_edit.Insert down))]);
    LTerm_read_line.(bind [LTerm_key.({ control = true; meta = false; shift = false; code = Prev_page })] [Edit (LTerm_edit.Zed (Zed_edit.Insert ctrlup))]);
    LTerm_read_line.(bind [LTerm_key.({ control = true; meta = false; shift = false; code = Next_page })] [Edit (LTerm_edit.Zed (Zed_edit.Insert ctrldown))]);
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = false; code = F5 })] [Edit (LTerm_edit.Zed (Zed_edit.Insert f5))]);
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = false; code = F12 })] [Edit (LTerm_edit.Zed (Zed_edit.Insert f12))]);
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = false; code = F11 })] [Edit (LTerm_edit.Zed (Zed_edit.Insert f11))]);
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = true; code = F11 })] [Edit (LTerm_edit.Zed (Zed_edit.Insert shift_f11))]);
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = false; code = F10 })] [Edit (LTerm_edit.Zed (Zed_edit.Insert f10))]);
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = true; code = F10 })] [Edit (LTerm_edit.Zed (Zed_edit.Insert shift_f10))]);
    LTerm_read_line.(bind [LTerm_key.({ control = true; meta = false; shift = false; code = Char (UChar.of_int 0x71) })] [Edit (LTerm_edit.Zed (Zed_edit.Insert ctrlq))]);
    LTerm_read_line.(bind [LTerm_key.({ control = true; meta = false; shift = false; code = Char (UChar.of_int 0x78) })] [Edit (LTerm_edit.Zed (Zed_edit.Insert ctrlx))]);
    self#set_input_buffer (Zed_rope.of_string input_buffer) ;
    self#set_prompt (S.l3 (fun size network redraw -> make_prompt size network state redraw)
                       self#size network redraw)
end

let quit state =
  match !xmpp_session with
  | None -> return_unit
  | Some x ->
     let otr_sessions =
       Contact.fold
         (fun _ u acc ->
          match u with
          | `Room _ -> acc
          | `User u ->
             List.fold_left
               (fun acc s ->
                if User.(encrypted s.otr) then
                  (u, s) :: acc
                else acc)
               acc
               u.User.active_sessions)
         state.contacts []
     in
     let send_out (user, session) =
       match Otr.Engine.end_otr session.User.otr with
       | _, Some body ->
          let jid = `Full (user.User.bare_jid, session.User.resource) in
          send x jid None body (fun _ -> return_unit)
       | _ -> return_unit
     in
     Lwt_list.iter_s send_out otr_sessions

let warn jid user add_msg =
  let last_msg =
    try Some (List.find
                (fun m -> match m.User.direction with
                          | `From (`Full _) -> true
                          | `Local ((`Bare _), s) when s = "resource warning" -> true
                          | _ -> false)
                user.User.message_history)
    with Not_found -> None
  in
  match last_msg, jid with
  | Some m, `Full (_, r) ->
     (match m.User.direction with
      | `From (`Full (_, r'))  when not (Xjid.resource_similar r r') ->
         let msg =
           "message sent to the active resource, " ^ r ^ ", while the last \
            message was received from " ^ r' ^ "."
         in
         add_msg (`Local (`Bare (Xjid.t_to_bare jid), "resource warning")) false msg
      | _ -> ())
  | _ -> ()

let send_msg t state active_user failure message =
  let handle_otr_out jid user_out =
    let add_msg direction encrypted data =
      let msg = User.message direction encrypted false data in
      let u = active state in
      let u = Contact.new_message u msg in
      Contact.replace_contact state.contacts u
    in
    (match active state with
     | `User u -> warn jid u add_msg
     | `Room _ -> ()) ;
    match user_out with
    | `Warning msg      ->
       add_msg (`Local (jid, "OTR Warning")) false msg ;
       ""
    | `Sent m           ->
       let id = random_string () in
       add_msg (`To (jid, id)) false m ;
       id
    | `Sent_encrypted m ->
       let id = random_string () in
       add_msg (`To (jid, id)) true (Escape.unescape m) ;
       id
  in
  let maybe_send ?kind jid out user_out =
    Utils.option
      Lwt.return_unit
      (fun body -> send t ?kind jid (Some (handle_otr_out jid user_out)) body failure)
      out
  in
  let jid, out, user_out, kind =
    match active_user with
    | `Room _ -> (* XXX MUC should also be more careful, privmsg.. *)
       let jid = `Bare (Xjid.t_to_bare state.active_contact) in
       (jid, Some message, `Sent message, Some Xmpp_callbacks.XMPPClient.Groupchat)
    | `User u ->
       let bare = u.User.bare_jid in
       match session state with
       | None ->
          let ctx = Otr.State.new_session (otr_config u state) state.config.Xconfig.dsa () in
          let _, out, user_out = Otr.Engine.send_otr ctx message in
          (`Bare bare, out, user_out, None)
       | Some session ->
          let ctx = session.User.otr in
          let msg =
            if Otr.State.is_encrypted ctx then
              Escape.escape message
            else
              message
          in
          let ctx, out, user_out = Otr.Engine.send_otr ctx msg in
          let user = User.update_otr u session ctx in
          Contact.replace_user state.contacts user ;
          (`Full (bare, session.User.resource), out, user_out, None)
  in
  maybe_send ?kind jid out user_out

let rec loop term state network log =
  let history = Contact.readline_history (active state) in
  match_lwt
    try_lwt
      let input_buffer = Contact.saved_input_buffer (active state) in
      (new read_line ~term ~history ~state ~network ~input_buffer)#run >>= fun message ->
      if List.length state.notifications = 0 then
        Lwt.async (fun () -> Lwt_mvar.put state.state_mvar Clear) ;
      return (Some message)
    with
      | Sys.Break -> return None
      | LTerm_read_line.Interrupt -> return (Some "/quit")
  with
    | None -> loop term state network log
    | Some message ->
       let active =
         let b = active state in
         let b = Contact.add_readline_history b message in
         let b = Contact.set_saved_input_buffer b "" in
         Contact.replace_contact state.contacts b ;
         b
       in
       let failure reason =
         Connect.disconnect () >>= fun () ->
         log (`Local (state.active_contact, "session error"), Printexc.to_string reason) ;
         ignore (Lwt_engine.on_timer 10. false (fun _ -> Lwt.async (fun () ->
                   Lwt_mvar.put state.connect_mvar Reconnect))) ;
         Lwt.return_unit
       and self = Xjid.jid_matches (`Bare (fst state.config.Xconfig.jid)) state.active_contact
       and err data = log (`Local (state.active_contact, "error"), data) ; return_unit
       in
       let fst =
         if String.length message = 0 then
           None
         else
           Some (String.get message 0)
       in
       match String.length message, fst with
       | 0, _ ->
          if Contact.expanded active || potentially_visible_resource state active then
            (Contact.replace_contact state.contacts (Contact.expand active) ;
             if Contact.expanded active then
               (state.active_contact <- `Bare (Contact.bare active))) ;
          loop term state network log
       | _, Some '/' ->
          if String.trim message = "/quit" then
            quit state >|= fun () -> state
          else
            Cli_commands.exec message state term active session self failure log force_redraw >>= fun () ->
            loop term state network log
       | _, _ when self ->
          err "try `M-x doctor` in emacs instead" >>= fun () ->
          loop term state network log
       | _, _ ->
          (match !xmpp_session with
           | None -> err "no active session, try to connect first"
           | Some t -> send_msg t state active failure message) >>= fun () ->
          loop term state network log

let init_system log myjid connect_mvar =
  let err r m =
    Lwt.async (fun () ->
      Connect.disconnect () >|= fun () ->
      log (`Local (`Full myjid, "async error"), m) ;
      if r then
        ignore (Lwt_engine.on_timer 10. false (fun _ ->
                  Lwt.async (fun () -> Lwt_mvar.put connect_mvar Reconnect))))
  in
  Lwt.async_exception_hook := (function
      | Tls_lwt.Tls_failure `Error (`AuthenticationFailure _) as exn ->
         err false (Printexc.to_string exn)
      | Unix.Unix_error (Unix.EBADF, _, _ ) as exn ->
         xmpp_session := None ; err false (Printexc.to_string exn)
      | exn -> err true (Printexc.to_string exn)
  )

