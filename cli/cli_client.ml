
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

let rec find_index id i = function
  | []                                  -> 0
  | x::_ when User.Jid.jid_matches id x -> i
  | _::xs                               -> find_index id (succ i) xs

let color_session self = function
  | Some x when User.(encrypted x.otr) -> green
  | Some _ when self -> default
  | Some _ -> red
  | None -> default

let show_buddy_list users show_offline self active notifications =
  let really_show jid =
    let jid_m = User.Jid.jid_matches jid in
    show_offline || jid_m (`Bare (fst self)) || List.exists jid_m notifications || jid_m active
  in
  let show id user =
    match
      List.fold_right (fun s acc ->
        if s.User.presence <> `Offline || really_show (`Full (id, s.User.resource)) then
          s :: acc
        else
          acc)
        (User.sorted_sessions user) []
    with
    | [] when really_show (`Bare id) -> [(user, [])]
    | [] -> []
    | xs -> [(user, xs)]
  in
  List.sort
    (fun (x, _) (y, _) -> User.Jid.compare_bare_jid x.User.bare_jid y.User.bare_jid)
    (User.fold (fun id u acc -> show id u @ acc) users [])

let flatten_buddies us =
  List.fold_right (fun (user, xs) acc ->
    let bare = user.User.bare_jid in
    match xs with
    | [] -> (match User.active_session user with
             | None -> `Bare bare
             | Some s -> `Full (bare, s.User.resource)) :: acc
    | [s] -> `Full (bare, s.User.resource) :: acc
    | xs when user.User.expand ->
       `Bare bare :: (List.map (fun s -> `Full (bare, s.User.resource)) xs) @ acc
    | _ -> (match User.active_session user with
            | None -> `Bare bare
            | Some s -> `Full (bare, s.User.resource)) :: acc)
    us []

let show_buddies users show_offline self active notifications =
  flatten_buddies (show_buddy_list users show_offline self active notifications)

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


let print_time ?now timestamp =
  let now = match now with
    | Some x -> x
    | None -> Unix.time ()
  in
  let display = Unix.localtime timestamp in
  if now -. timestamp < 86400. then (* less than a day ago *)
    Printf.sprintf "%02d:%02d:%02d "
      display.Unix.tm_hour
      display.Unix.tm_min
      display.Unix.tm_sec
  else
    Printf.sprintf "%02d-%02d %02d:%02d "
      (succ display.Unix.tm_mon)
      display.Unix.tm_mday
      display.Unix.tm_hour
      display.Unix.tm_min

let format_log log =
  let now = Unix.time () in
  let print_log { User.direction ; timestamp ; message ; _ } =
    let time = print_time ~now timestamp in
    let from = match direction with
      | `From jid -> User.Jid.jid_to_string jid ^ ":"
      | `Local (_, x) when x = "" -> "***"
      | `Local (_, x) -> "*** " ^ x ^ " ***"
      | `To _ -> ">>>"
    in
    time ^ from ^ " " ^ message
  in
  List.map print_log log

let format_buddies buddies self active notifications width =
  let draw expanded print user session =
    let jid = match session with
      | None -> `Bare user.User.bare_jid
      | Some s -> `Full (user.User.bare_jid, s.User.resource)
    in
    let jid_m o = User.Jid.jid_matches o jid in
    let notify =
      if expanded then
        List.exists jid_m notifications
      else
        List.exists (User.Jid.jid_matches (`Bare user.User.bare_jid)) notifications
    and self = jid_m (`Bare (fst self))
    in
    let item =
      let f, t = if self then
                   ("{", "}")
                 else
                   User.subscription_to_chars user.User.subscription
      and st = match notify, expanded with
        | true , true  -> "*"
        | false, false -> "+"
        | true , false -> Zed_utf8.singleton (UChar.of_int 0x2600)
        | false, true  -> " "
      and presence =
        let p = match session with
          | None -> `Offline
          | Some s -> s.User.presence
        in
        User.presence_to_char p
      and bare = User.Jid.t_to_bare jid
      in
      let data = print st f presence t (User.Jid.bare_jid_to_string bare) (User.Jid.resource jid) in
      pad width data
    and highlight, e_highlight =
      if jid = active || (not user.User.expand && jid_m active) then
        ([ B_reverse true ], [ E_reverse ])
      else
        ([], [])
    and fg = color_session self session
    in
    let show = highlight @ [B_fg fg ; S item ; E_fg ] @ e_highlight in
    if notify then
      B_blink true :: show @ [ E_blink ]
    else
      show
  in
  let draw_single st f p t jid r = Printf.sprintf "%s%s%s%s %s%s" st f p t jid (match r with None -> "" | Some r -> "/" ^ r)
  and draw_user st f _ t jid _ = Printf.sprintf "%s%s %s %s" st f t jid
  and draw_session st _ p _ _ r = Printf.sprintf " %s%s   %s" st p (match r with None -> "NONE" | Some r -> r)
  in

  List.fold_right (fun (user, sessions) acc ->
    match sessions with
    | [] -> draw true draw_single user (User.active_session user) :: acc
    | [s] -> draw true draw_single user (Some s) :: acc
    | xs when user.User.expand ->
       draw true draw_user user None ::
         List.map (draw true draw_session user)
           (List.map (fun s -> Some s) xs) @
         acc
    | xs -> draw (List.length xs <= 1) draw_single user (User.active_session user) :: acc)
    buddies []

let format_messages user jid msgs =
  let now = Unix.time () in
  let printmsg { User.direction ; encrypted ; received ; timestamp ; message ; _ } =
    let time = print_time ~now timestamp in
    let en = if encrypted then "O" else "-" in
    let msg_color, pre = match direction with
      | `From _ -> (`Highlight, "<" ^ en ^ "- ")
      | `To _   ->
         let f = if received then "-" else "?" in
         (`Default, f ^ en ^ "> ")
      | `Local (_, x) when x = "" -> (`Default, "*** ")
      | `Local (_, x) -> (`Default, "***" ^ x ^ "*** ")
    in
    let r =
      let other = User.jid_of_direction direction in
      match jid with
      | `Bare _ ->
         Utils.option
           ""
           (fun x -> "(" ^ x ^ ") ")
           (User.Jid.resource other)
      | `Full (_, r) ->
         Utils.option "" (fun x -> "(" ^ x ^ ") ")
                      (match User.Jid.resource other with
                       | None -> None
                       | Some x when x = r -> None
                       | Some x -> Some x)
    in
    (msg_color, time ^ r ^ pre ^ message)
  in
  let jid_tst o =
    if
      List.length user.User.active_sessions < 2 ||
        not user.User.expand
    then
      true
    else
      match jid with
        | `Bare _ -> User.Jid.jid_matches jid o
        | `Full _ -> User.Jid.jid_matches o jid
  in
  List.map printmsg
    (List.filter
       (fun m -> jid_tst (User.jid_of_direction m.User.direction))
       msgs)

let buddy_list users show_offline self active notifications length width =
  let buddies = show_buddy_list users show_offline self active notifications in
  let formatted_buddies = format_buddies buddies self active notifications width in

  let flattened = flatten_buddies buddies in
  let bs = List.length flattened
  and up, down = (length / 2, (length + 1) / 2)
  and active_idx = find_index active 0 flattened
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

let horizontal_line user session fg_color buddy_width scrollback show_buddy_list width =
  let buddy, presence, status, otr, otrcolor = match session with
    | Some s ->
      let presence = User.presence_to_string s.User.presence in
      let status = match s.User.status with
        | None   -> ""
        | Some x ->
          let stripped =
            try String.sub x 0 (String.index x '\n')
            with Not_found -> x
          in
          " - " ^ stripped
      in
      let otrcolor, otr =
        Utils.option
          (red, " - no OTR")
          (fun fp -> match User.verified_fp user fp with
                     | `Verified -> (fg_color, " - OTR verified")
                     | `Unverified -> (red, " - unverified OTR: " ^ (User.pp_fingerprint fp))
                     | `Revoked -> (red, " - revoked"))
          (User.otr_fingerprint s.User.otr)
      in
      (User.userid user s, " -- " ^ presence, status, otr, otrcolor)
    | None -> (User.jid user, "", "", "", default)
  in
  let pre =
    if show_buddy_list then
      (Zed_utf8.make buddy_width (UChar.of_int 0x2500)) ^
      (Zed_utf8.singleton (UChar.of_int 0x2534))
    else
      (Zed_utf8.singleton (UChar.of_int 0x2500))
  in
  let txt =
    match scrollback with
    | 0 -> " buddy: " ^ buddy
    | _ -> "*scroll*" ^ buddy
  in
  (* now we have the building blocks and might need to cut some of them down *)
  let buddy, left = maybe_trim txt width in
  let otr, left = maybe_trim otr left in
  let presence, left = maybe_trim presence left in
  let status, left = maybe_trim status left in
  let pre, left = maybe_trim pre left in
  let post = if left > 0 then [S (Zed_utf8.make left (UChar.of_int 0x2500))] else [] in
  B_fg fg_color :: pre @ buddy @ [ E_fg ; B_fg otrcolor ] @ otr @ [ E_fg ; B_fg fg_color ] @ presence @ status @ post @ [ E_fg ]

let status_line user session notify log redraw fg_color width =
  let status = User.presence_to_string session.User.presence in
  let jid = User.userid user session in

  let status_color =
    if session.User.presence = `Offline then
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
         let reset s =
           let receipt = match s.User.receipt with
             | `Requested -> `Unknown
             | x -> x
           and presence = `Offline
           in
           { s with User.receipt ; presence }
         in
         User.iter (fun _ v ->
                    let active_sessions = List.map reset v.User.active_sessions in
                    User.replace_user state.users { v with User.active_sessions })
                   state.users ;
         Lwt.async (fun () -> Lwt_mvar.put state.state_mvar Disconnected)
      | _ -> ())
   | _ -> ()) ;

  let log_size = state.log_height in
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

  if main_size <= 6 || chat_width <= 20 then
    eval ([S "need more space"])
  else
    begin
      let self = User.find_user state.users (fst state.config.Config.jid) in
      let statusses = self.User.message_history in
      let logs =
        let entries =
          if List.length statusses > log_size then
            take log_size statusses []
          else
            statusses
        in
        let entries =
          let entries = format_log entries in
          line_wrap ~max_length:size.cols entries
        in
        let data = pad_l_rev "" log_size entries in
        String.concat "\n" data
      in

      let active = active state in
      let session = session state in

      let fg_color = color_session (self = active) session in

      let main_window =
        let msg_colors, data =
          let msgs =
            if active = self then
              List.map (fun s -> `Default, s) (format_log statusses)
            else
              format_messages active state.active_contact active.User.message_history
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

          let buddies = buddy_list state.users state.show_offline state.config.Config.jid state.active_contact state.notifications main_size buddy_width in
          let comb = List.combine buddies chat in
          let pipe = S (Zed_utf8.singleton (UChar.of_int 0x2502)) in
          List.map (fun (buddy, chat) ->
              buddy @ [ B_fg fg_color ; pipe ; E_fg; ] @ (render_msg chat))
            comb

        | FullScreen ->
          let chat = line_wrap_with_tags ~max_length:chat_width ~tags:msg_colors data in
          let chat = scroll (`Default, "") chat in
          List.map render_msg chat

        | Raw ->
          let data = List.map (fun x -> x.User.message)
              (List.filter (fun x -> match x.User.direction with
                   | `Local _ -> false
                   | `From _ -> true
                   | `To _ -> false)
                  active.User.message_history)
          in
          let wrapped = line_wrap ~raw:() ~max_length:chat_width data in
          let chat = scroll "" wrapped in
          List.map (fun x -> [ S x ; S "\n" ]) chat
      in

      let showing_buddies = match state.window_mode with
        | BuddyList -> true
        | FullScreen | Raw -> false
      in
      let hline = horizontal_line active session fg_color buddy_width state.scrollback showing_buddies size.cols in

      let notify = List.length state.notifications > 0 in
      let log = active.User.preserve_messages in
      let mysession =
        let r = snd state.config.Config.jid in
        List.find (fun s -> s.User.resource = r) self.User.active_sessions in
      let status = status_line self mysession notify log redraw fg_color size.cols in
      let main = List.flatten main_window in

      try
        if log_size = 0 then
          eval ( main @ status @ [ S "\n" ] )
        else
          eval ( main @ hline @ [ S "\n" ; S logs ; S "\n" ] @ status @ [ S "\n" ] )
      with
        _ -> eval ([ S "error during evaluating layout"])
    end

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
    if state.scrollback = 0 then
      notified state state.active_contact ;
    force_redraw ()
  | Up, n -> state.scrollback <- n + 1; force_redraw ()

let navigate_buddy_list state direction =
  let userlist = show_buddies state.users state.show_offline state.config.Config.jid state.active_contact state.notifications in
  let set_active idx =
    let user = List.nth userlist idx in
    activate_user state user ;
    force_redraw ()
  and active_idx = find_index state.active_contact 0 userlist
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
    User.replace_user state.users { u with User.saved_input_buffer }

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
         activate_user state (List.hd (List.rev state.notifications)) ;
         force_redraw () ;
         super#send_action LTerm_read_line.Break )
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = ctrlx ->
      self#save_input_buffer ;
      activate_user state state.last_active_contact ;
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
       User.fold (fun _ u acc ->
        List.fold_left (fun acc s ->
          if User.(encrypted s.otr) then
            (u, s) :: acc
          else acc)
          acc
          u.User.active_sessions)
       state.users []
     in
     let send_out (user, session) =
       match Otr.Engine.end_otr session.User.otr with
       | _, Some body ->
          let jid = `Full (user.User.bare_jid, session.User.resource) in
          send x jid None body (fun _ -> return_unit)
       | _ -> return_unit
     in
     Lwt_list.iter_s send_out otr_sessions

let rec loop term state network log =
  let history = (active state).User.readline_history in
  match_lwt
    try_lwt
      let input_buffer = (active state).User.saved_input_buffer in
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
         let u = active state in
         let history =
           let num = min 50 (List.length u.User.readline_history) in
           take num u.User.readline_history []
         in
         let readline_history = message :: history in
         let u = { u with User.readline_history ; saved_input_buffer = "" } in
         User.replace_user state.users u ;
         u
       in
       let failure reason =
         Connect.disconnect () >>= fun () ->
         log (`Local (state.active_contact, "session error"), Printexc.to_string reason) ;
         ignore (Lwt_engine.on_timer 10. false (fun _ -> Lwt.async (fun () ->
                   Lwt_mvar.put state.connect_mvar Reconnect))) ;
         Lwt.return_unit
       and self = User.Jid.jid_matches (`Bare active.User.bare_jid) (`Full state.config.Config.jid)
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
          User.replace_user state.users { active with User.expand = not active.User.expand } ;
          (* transition from expanded to collapsed *)
          if active.User.expand then
            (let jid = match User.active_session active with
               | None -> `Bare active.User.bare_jid
               | Some s -> `Full (active.User.bare_jid, s.User.resource)
             in
             state.active_contact <- jid) ;
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
          let jid = state.active_contact in
          let handle_otr_out user_out =
            let add_msg direction enc data =
              let u = User.find_user state.users (User.Jid.t_to_bare jid) in
              let u = User.insert_message u direction enc false data in
              User.replace_user state.users u
            in
            let warn () =
              let u =
                User.find_user state.users (User.Jid.t_to_bare jid) in
              let last =
                try
                  Some
                    (List.find
                       (fun m -> match m.User.direction with
                                 | `From (`Full _) -> true
                                 | `Local ((`Bare _), s) when s = "resource warning" -> true
                                 | _ -> false)
                       u.User.message_history)
                with Not_found -> None
              in
              match last, jid with
              | Some m, `Full (_, r) ->
                 (match m.User.direction with
                  | `From (`Full (_, r'))  when not (User.Jid.resource_similar r r') ->
                     add_msg (`Local (`Bare (User.Jid.t_to_bare jid),
                                      "resource warning"))
                             false
                             ("last message was received from a different resource, " ^ r' ^ "; you might want to expand the contact and send messages directly to that resource (instead of " ^ r ^ ")")
                  | _ -> ())
              | _ -> ()
            in
            warn () ;
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
          let maybe_send t out user_out =
            Utils.option
              Lwt.return_unit
              (fun body ->
                 send t jid (Some (handle_otr_out user_out)) body failure)
              out
          in
          (match jid, !xmpp_session with
           | `Full (_, r), Some t ->
              (match User.find_session active r with
               | Some session ->
                  let ctx = session.User.otr in
                  let msg =
                    if Otr.State.is_encrypted ctx then
                      Escape.escape message
                    else
                      message
                  in
                  let ctx, out, user_out = Otr.Engine.send_otr ctx msg in
                  let user = User.update_otr active session ctx in
                  User.replace_user state.users user ;
                  maybe_send t out user_out
               | None ->
                  let ctx = Otr.State.new_session (otr_config active state) state.config.Config.dsa () in
                  let _, out, user_out = Otr.Engine.send_otr ctx message in
                  maybe_send t out user_out)
           | `Bare _     , Some t ->
              let ctx = Otr.State.new_session (otr_config active state) state.config.Config.dsa () in
              let _, out, user_out = Otr.Engine.send_otr ctx message in
              maybe_send t out user_out
           | _           , None ->
              err "no active session, try to connect first") >>= fun () ->
          loop term state network log

let init_system log myjid connect_mvar =
  let domain = snd (fst myjid) in
  let err r m =
    Lwt.async (fun () ->
      Connect.disconnect () >|= fun () ->
      log (`Local (`Full myjid, "async error"), m) ;
      if r then
        ignore (Lwt_engine.on_timer 10. false (fun _ ->
                  Lwt.async (fun () -> Lwt_mvar.put connect_mvar Reconnect))))
  in
  Lwt.async_exception_hook := (function
      | Tls_lwt.Tls_failure `Error (`AuthenticationFailure (`InvalidServerName x)) ->
        let pre = Printf.sprintf "invalid hostname in certificate: expected %s," domain
        and warn =
          Printf.sprintf "inform your server administrator about that, in the meantime add '(certificate_hostname (\"%s\")' to your config.sexp"
        in
        (match X509.hostnames x with
         | x::_ -> err false (Printf.sprintf "%s, but got %s" pre x) ; err false (warn x)
         | [] -> err false (Printf.sprintf "%s, but found no name" pre))
      | Unix.Unix_error (Unix.EBADF, _, _ ) as exn ->
         xmpp_session := None ; err false (Printexc.to_string exn)
      | exn -> err true (Printexc.to_string exn)
  )

