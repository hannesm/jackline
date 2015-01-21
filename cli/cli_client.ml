
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
  match x - (String.length s) with
  | 0                  -> s
  | d when d > 0       -> s ^ (String.make d ' ')
  | _ (* when d < 0 *) -> String.sub s 0 x

let rec find_index id i = function
  | []               -> 0
  | x::_ when x = id -> i
  | _::xs            -> find_index id (succ i) xs

let color_session u su = function
  | Some x when User.(encrypted x.otr) -> green
  | Some _ when u = su -> default
  | Some _ -> red
  | None -> default

let show_buddies users show_offline self active notifications =
  List.fold_right (fun id acc ->
      let u = User.Users.find users id in
      let session = User.active_session u in
      let presence = match session with
        | None -> `Offline
        | Some s -> s.User.presence
      in
      let rly_show = u = self || u = active || List.mem u notifications in
      match rly_show, show_offline, presence with
      | true,  _    , _        -> id :: acc
      | false, true , _        -> id :: acc
      | false, false, `Offline -> acc
      | false, false, _        -> id :: acc)
    (User.keys users) []

let rec line_wrap ?raw ~max_length entries acc : string list =
  let pre = match raw with
    | None   -> "  "
    | Some _ -> ""
  in
  match entries with
  | entry::remaining when String.contains entry '\n' ->
    let part1     = String.(sub entry 0 (index entry '\n')) in
    let part1_len = succ (String.length part1) in
    let part2     = pre ^ String.sub entry part1_len ((String.length entry) - part1_len) in
    let remaining =
      match raw with
      | Some _ -> part2::part1::remaining
      | None   ->
        match String.trim part1 = "", String.trim part2 = "" with
        | true , true  -> remaining
        | false, true  -> part1::remaining
        | true , false -> part2::remaining
        | false, false -> part2::part1::remaining
    in
    line_wrap ?raw ~max_length remaining acc
  | entry::remaining when (Zed_utf8.length entry) > max_length ->
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
        ( match find_space 10 with
          | None   -> (p1, pre ^ String.trim p2)
          | Some x ->
            let p1, p2 = Zed_utf8.break entry (x + (max_length - n)) in
            (p1, pre ^ String.trim p2) )
      | Some _ -> (p1, p2)
    in
    line_wrap ?raw ~max_length (part2::part1::remaining) acc
  | entry::remaining ->
    line_wrap ?raw ~max_length remaining (entry::acc)
  | [] -> acc

let format_log log =
  let open User in
  let print_log { direction ; timestamp ; message ; _ } =
    let time =
      let lt = Unix.localtime timestamp in
      Printf.sprintf "[%02d:%02d:%02d] "
        lt.Unix.tm_hour lt.Unix.tm_min lt.Unix.tm_sec
    in
    let from = match direction with
      | `From jid -> jid ^ ":"
      | `Local x when x = "" -> "***"
      | `Local x -> "*** " ^ x ^ " ***"
      | `To _ -> ">>>"
    in
    time ^ from ^ " " ^ message
  in
  List.map print_log log

let format_buddies buddies users self active notifications width =
  List.map (fun id ->
      let u = User.Users.find users id in
      let session = User.active_session u in
      let presence = match session with
        | None -> `Offline
        | Some s -> s.User.presence
      in
      let fg = color_session u self session in
      let highlight, e_highlight = if u = active then
          ([ B_reverse true ], [ E_reverse ])
        else
          ([], [])
      in
      let f, t =
        if u = self then
          ("{", "}")
        else
          User.subscription_to_chars u.User.subscription
      in
      let notify = List.mem u notifications in
      let item =
        let data = Printf.sprintf "%s%s%s%s %s"
            (if notify then "*" else " ")
            f (User.presence_to_char presence) t id
        in
        pad width data
      in
      let show = highlight @ [B_fg fg ; S item ; E_fg ] @ e_highlight in
      if notify then
        B_blink true :: show @ [ E_blink ]
      else
        show)
    buddies

let format_messages msgs =
  let open User in
  let printmsg { direction ; encrypted ; received ; timestamp ; message ; _ } =
    let lt = Unix.localtime timestamp in
    let time =
      Printf.sprintf "%02d %02d:%02d:%02d "
        lt.Unix.tm_mday
        lt.Unix.tm_hour lt.Unix.tm_min lt.Unix.tm_sec
    in
    let en = if encrypted then "O" else "-" in
    let pre = match direction with
      | `From _ -> "<" ^ en ^ "- "
      | `To _   -> (if received then "-" else "r") ^ en ^ "> "
      | `Local x when x = "" -> "*** "
      | `Local x -> "***" ^ x ^ "*** "
    in
    time ^ pre ^ message
  in
  List.map printmsg msgs

let buddy_list users show_offline self active notifications length width =
  let buddies = show_buddies users show_offline self active notifications in
  let formatted_buddies = format_buddies buddies users self active notifications width in

  let bs = List.length buddies
  and up, down = (length / 2, (length + 1) / 2)
  and active_idx = find_index active.User.jid 0 buddies
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
  let open User in
  let buddy, presence, status, otr, otrcolor = match session with
    | Some s ->
      let presence = User.presence_to_string s.User.presence in
      let status = match s.status with
        | None   -> ""
        | Some x ->
          let stripped =
            try String.sub x 0 (String.index x '\n')
            with Not_found -> x
          in
          " - " ^ stripped
      in
      let otrcolor, otr = match otr_fingerprint s.otr with
        | Some raw when verified_fp user raw -> (fg_color, " - OTR verified")
        | Some raw                           -> (red, " - unverified OTR: " ^ (User.format_fp raw))
        | None                               -> (red, " - no OTR")
      in
      (userid user s, " -- " ^ presence, status, otr, otrcolor)
    | None -> (user.jid, "", "", "", default)
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

let status_line now user session notify log redraw fg_color width =
  let status = User.presence_to_string session.User.presence in
  let jid = User.userid user session in
  let time =
    let now = Unix.localtime now in
    Printf.sprintf "%02d:%02d" now.Unix.tm_hour now.Unix.tm_min
  in

  let jid_color = if log then red else lblue in

  let status_color =
    if session.User.presence = `Offline then
      lred
    else
      lgreen
  in

  let jid, left = maybe_trim jid (pred width) in
  let jid_pre, left = maybe_trim "< " left in
  let jid_post, left = maybe_trim " >─" left in

  let status, left = maybe_trim status left in
  let status_pre, left = maybe_trim "[ " left in
  let status_post, left = maybe_trim " ]─" left in

  let col = index (redraw mod 16) in
  let redraw, left = maybe_trim (Printf.sprintf "%02x" redraw) left in

  let time, left = maybe_trim time left in
  let time_pre, left = maybe_trim "─( " left in
  let time_post, left = maybe_trim " )─" left in

  let fill = if left > 0 then [S (Zed_utf8.make left (UChar.of_int 0x2500))] else [] in

  let first =
    let rnd = [ B_fg col ] @ redraw @ [ E_fg ] in
    if notify then
      [ B_bold true ; B_blink true ; B_fg blue ; S "#" ] @ redraw @ [ E_fg ; E_blink ]
    else
      [ B_bold true ; B_fg fg_color ; S (Zed_utf8.make 1 (UChar.of_int 0x2500)) ; E_fg ] @ rnd
  in

  first @
  [ B_fg fg_color ] @ time_pre @ time @ time_post @
  jid_pre @ [ E_fg ; B_fg jid_color ] @ jid @ [ E_fg ; B_fg fg_color ] @ jid_post @
  fill @
  status_pre @ [ E_fg ; B_fg status_color ] @ status @ [ E_fg ; B_fg fg_color ] @ status_post @
  [ E_fg ; E_bold ]

let make_prompt size time network state redraw =
  (* we might have gotten a connection termination - if so mark everything offline *)
  (match fst network with
   | `Local err ->
     let err_prefix = try String.sub err 0 11 with Invalid_argument _ -> "" in
     ( match err_prefix, !xmpp_session with
       | (x, None) when x = "async error" || x = "session err" ->
         let users = User.Users.fold (fun id u acc ->
             let act = List.map
                 (fun s -> { s with User.presence = `Offline })
                 u.User.active_sessions
             in
             (id, { u with User.active_sessions = act }) :: acc)
             state.users []
         in
         List.iter (fun (id, u) -> User.Users.replace state.users id u) users
       | _ -> () )
   | _ -> () );

  (* network should be an event, then I wouldn't need a check here *)
  (if state.last_status <> network then
     (add_status state (fst network) (snd network) ;
      state.last_status <- network) ) ;

  let log_size = 6 in
  let main_size = size.rows - log_size - 3 (* status + hline + readline *) in
  let buddy_width = 24 in
  let chat_width =
    match state.window_mode with
    | BuddyList        -> size.cols - buddy_width - 1
    | FullScreen | Raw -> size.cols
  in

  if main_size <= 6 || chat_width <= 10 then
    eval ([S "need more space"])
  else
    begin
      let self = User.Users.find state.users state.user in
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
          line_wrap ~max_length:size.cols entries []
        in
        let data = pad_l_rev "" log_size entries in
        String.concat "\n" data
      in

      let active = User.Users.find state.users state.active_contact in
      let active_session = User.active_session active in
      let notifications =
        List.map
          (fun id -> User.Users.find state.users id)
          state.notifications
      in

      let fg_color = color_session active self active_session in

      let main_window =
        let data =
          if active = self then
            format_log statusses
          else
            format_messages active.User.message_history
        in
        let scroll data =
          (* data is already in right order -- but we need to strip scrollback *)
          let elements = drop (state.scrollback * main_size) (List.rev data) in
          pad_l_rev "" main_size (List.rev elements)
        in

        match state.window_mode with
        | BuddyList ->
          let chat = line_wrap ~max_length:chat_width data [] in
          let chat = scroll chat in

          let buddies = buddy_list state.users state.show_offline self active notifications main_size buddy_width in
          let comb = List.combine buddies chat in
          let pipe = S (Zed_utf8.singleton (UChar.of_int 0x2502)) in
          List.map (fun (buddy, chat) ->
              buddy @ [ B_fg fg_color ; pipe ; E_fg ; S chat ; S "\n" ])
            comb

        | FullScreen ->
          let chat = line_wrap ~max_length:chat_width data [] in
          let chat = scroll chat in
          List.map (fun chat -> [ S chat ; S "\n"]) chat

        | Raw ->
          let data = List.map (fun x -> x.User.message)
              (List.filter (fun x -> match x.User.direction with
                   | `Local _ -> false
                   | `From _ -> true
                   | `To _ -> false)
                  active.User.message_history)
          in
          let wrapped = line_wrap ~raw:() ~max_length:chat_width data [] in
          let chat = scroll wrapped in
          List.map (fun x -> [ S x ; S "\n" ]) chat
      in

      let showing_buddies = match state.window_mode with
        | BuddyList -> true
        | FullScreen | Raw -> false
      in
      let hline = horizontal_line active active_session fg_color buddy_width state.scrollback showing_buddies size.cols in

      let notify = List.length notifications > 0 in
      let log = active.User.preserve_messages in
      let mysession = List.find (fun s -> s.User.resource = state.resource) self.User.active_sessions in
      let status = status_line time self mysession notify log redraw fg_color size.cols in
      let main = List.flatten main_window in

      try
        eval ( main @ hline @ [ S "\n" ; S logs ; S "\n" ] @ status @ [ S "\n" ] )
      with
        _ -> eval ([ S "error during evaluating layout"])
    end

let time =
  let time, set_time = S.create (Unix.time ()) in
  (* Update the time every 60 seconds. *)
  ignore (Lwt_engine.on_timer 60.0 true (fun _ -> set_time (Unix.time ())));
  time

let up = UChar.of_int 0x2500
let down = UChar.of_int 0x2501
let f5 = UChar.of_int 0x2502
let f12 = UChar.of_int 0x2503
let ctrlq = UChar.of_int 0x2504
let ctrlx = UChar.of_int 0x2505
let ctrlup = UChar.of_int 0x2506
let ctrldown = UChar.of_int 0x2507


let redraw, force_redraw =
  (* this is just an ugly hack which should be removed *)
  let a, b = S.create 0 in
  (a, fun () -> b (Random.int 256) )

type direction = Up | Down

let activate_user state active =
  if state.active_contact <> active then
    (state.last_active_contact <- state.active_contact ;
     state.active_contact      <- active ;
     state.scrollback          <- 0 ;
     state.notifications       <- List.filter (fun a -> a <> active) state.notifications ;
     state.window_mode         <- BuddyList ;
     force_redraw ())

let navigate_message_buffer state direction =
  match
    direction,
    state.scrollback
  with
  | Down, 0 -> ()
  | Down, n -> state.scrollback <- n - 1 ; force_redraw ()
  | Up, n -> state.scrollback <- n + 1; force_redraw ()

let navigate_buddy_list state direction =
  let find u = User.Users.find state.users u in
  let active = find state.active_contact in
  let notifications = List.map find state.notifications in
  let self = find state.user in
  let userlist = show_buddies state.users state.show_offline self active notifications in
  let set_active idx =
    let user = List.nth userlist idx in
    activate_user state user
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

class read_line ~term ~network ~history ~state = object(self)
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

  method! send_action = function
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = down ->
      navigate_buddy_list state Down
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = up ->
      navigate_buddy_list state Up
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
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = ctrlq ->
      if List.length state.notifications > 0 then
        activate_user state (List.hd (List.rev state.notifications))
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = ctrlx ->
      let user = state.last_active_contact in
      activate_user state user
    | action ->
      super#send_action action

  initializer
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = false; code = Prev_page })] [Edit (LTerm_edit.Zed (Zed_edit.Insert up))]);
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = false; code = Next_page })] [Edit (LTerm_edit.Zed (Zed_edit.Insert down))]);
    LTerm_read_line.(bind [LTerm_key.({ control = true; meta = false; shift = false; code = Prev_page })] [Edit (LTerm_edit.Zed (Zed_edit.Insert ctrlup))]);
    LTerm_read_line.(bind [LTerm_key.({ control = true; meta = false; shift = false; code = Next_page })] [Edit (LTerm_edit.Zed (Zed_edit.Insert ctrldown))]);
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = false; code = F5 })] [Edit (LTerm_edit.Zed (Zed_edit.Insert f5))]);
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = false; code = F12 })] [Edit (LTerm_edit.Zed (Zed_edit.Insert f12))]);
    LTerm_read_line.(bind [LTerm_key.({ control = true; meta = false; shift = false; code = Char (UChar.of_int 0x71) })] [Edit (LTerm_edit.Zed (Zed_edit.Insert ctrlq))]);
    LTerm_read_line.(bind [LTerm_key.({ control = true; meta = false; shift = false; code = Char (UChar.of_int 0x78) })] [Edit (LTerm_edit.Zed (Zed_edit.Insert ctrlx))]);
    self#set_prompt (S.l4 (fun size time network redraw -> make_prompt size time network state redraw)
                       self#size time network redraw)
end

let rec loop ?out (config : Config.t) term hist state network log =
  let history = LTerm_history.contents hist in
  match_lwt
    try_lwt
      lwt command = (new read_line ~term ~history ~state ~network)#run in
      return (Some command)
    with
      | Sys.Break -> return None
      | LTerm_read_line.Interrupt -> return (Some "/quit")
  with
    | Some command when (String.length command > 0) && String.get command 0 = '/' ->
       LTerm_history.add hist command ;
       if String.trim command <> "/quit" then
         Cli_commands.exec ?out command state config log force_redraw >>= fun () ->
         loop ?out config term hist state network log
       else
         ( begin
             match !xmpp_session with
               | None -> return_unit
               | Some x ->
                  let otr_sessions = User.Users.fold (fun _ u acc ->
                      List.fold_left (fun acc s ->
                          if User.(encrypted s.otr) then
                            User.(userid u s, s.otr) :: acc
                          else acc)
                        acc
                        u.User.active_sessions)
                      state.users []
                  in
                  Lwt_list.iter_s
                    (fun (jid_to, ctx) ->
                       let _, out = Otr.Engine.end_otr ctx in
                       Xmpp_callbacks.XMPPClient.(send_message x
                                                    ~kind:Chat
                                                    ~jid_to:(JID.of_string jid_to)
                                                    ?body:out ()))
                    otr_sessions
           end >|= fun () -> state )

    | Some message when String.length message > 0 ->
       LTerm_history.add hist message ;
       let err data = log (`Local "error", data) ; return_unit in
       let contact = User.Users.find state.users state.active_contact in
       let handle_otr_out user_out =
         let add_msg direction enc data =
           let user = User.Users.find state.users state.active_contact in
           let user = User.insert_message user direction enc false data in
           User.Users.replace state.users user.User.jid user
         in
         (match user_out with
          | `Warning msg      -> add_msg (`Local "OTR Warning") false msg
          | `Sent m           -> add_msg (`To "") false m
          | `Sent_encrypted m -> add_msg (`To "") true m ) ;
       in
       let failure reason =
         xmpp_session := None ;
         log (`Local "session error", Printexc.to_string reason) ;
         return_unit
       in
       (if contact.User.jid = state.user then err "try `M-x doctor` in emacs instead"
       else
         match User.active_session contact, !xmpp_session with
         | Some session, Some t ->
           let ctx, out, user_out = Otr.Engine.send_otr session.User.otr message in
           User.replace_session state.users contact { session with User.otr = ctx } ;
           handle_otr_out user_out ;
           (try_lwt Xmpp_callbacks.XMPPClient.(
                send_message t
                  ~kind:Chat
                  ~jid_to:(JID.of_string (User.userid contact session))
                  ?body:out () )
            with e -> failure e)
         | None        , Some t ->
           let ctx = Otr.State.new_session config.Config.otr_config () in
           let _, out, user_out = Otr.Engine.send_otr ctx message in
           handle_otr_out user_out ;
           (try_lwt Xmpp_callbacks.XMPPClient.(
             send_message t
               ~kind:Chat
               ~jid_to:(JID.of_string contact.User.jid)
               ?body:out () )
            with e -> failure e)
         | _           , None ->
           err "no active session, try to connect first" ) >>= fun () ->
       loop ?out config term hist state network log
     | Some _ -> loop ?out config term hist state network log
     | None -> loop ?out config term hist state network log

let init_system log =
  Lwt.async_exception_hook := (
    fun exn ->
      let err m = log (`Local "async error", m) in

      xmpp_session := None ;

      err (Printexc.to_string exn)
  )

