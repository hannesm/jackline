
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
  | Some _ when u = su -> black
  | Some _ -> red
  | None -> black

let show_buddies state =
  List.fold_right (fun id acc ->
      let u = User.Users.find state.users id in
      let session = User.good_session u in
      let presence = match session with
        | None -> `Offline
        | Some s -> s.User.presence
      in
      let rly_show = u = state.user || u = fst state.active_chat || List.mem u state.notifications in
      match rly_show, state.show_offline, presence with
      | true,  _    , _        -> id :: acc
      | false, true , _        -> id :: acc
      | false, false, `Offline -> acc
      | false, false, _        -> id :: acc)
    (User.keys state.users) []

let rec line_wrap ~max_length entries acc : string list =
  match entries with
  | entry::remaining when String.contains entry '\n' ->
    let part1     = String.sub entry 0 (String.index entry '\n') in
    let part1_len = 1 + String.length part1 in (* +1: account for \n *)
    let part2     = "  " ^ String.sub entry part1_len ((String.length entry) - part1_len) in
    let remaining =
      match String.trim part1 = "", String.trim part2 = "" with
      | true , true  -> remaining
      | false, true  -> part1::remaining
      | true , false -> part2::remaining
      | false, false -> part2::part1::remaining
    in
    line_wrap ~max_length remaining acc
  | entry::remaining when (Zed_utf8.length entry) > max_length ->
    let part1, part2 = Zed_utf8.break entry max_length in
    line_wrap ~max_length (("  " ^ part2)::part1::remaining) acc
  | entry::remaining ->
    line_wrap ~max_length remaining (entry::acc)
  | [] -> acc

let log_buffer log width =
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
  let entries = List.map print_log log in
  line_wrap ~max_length:width entries []

let format_buddies buddies users active self notifications width =
  List.map (fun id ->
      let u = User.Users.find users id in
      let session = User.good_session u in
      let presence = match session with
        | None -> `Offline
        | Some s -> s.User.presence
      in
      let fg = color_session u self session in
      let bg = if u = active then white else lwhite in
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
      let show = [B_fg fg ; B_bg bg ; S item ; E_bg ; E_fg ] in
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
      Printf.sprintf "%02d-%02d %02d:%02d "
        (succ lt.Unix.tm_mon) lt.Unix.tm_mday
        lt.Unix.tm_hour lt.Unix.tm_min
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

let message_buffer msgs width =
  line_wrap ~max_length:width (format_messages msgs) []

let buddy_list state length width =
  let buddies = show_buddies state in
  let formatted_buddies = format_buddies buddies state.users (fst state.active_chat) state.user state.notifications width in

  let bs = List.length buddies
  and up, down = (length / 2, (length + 1) / 2)
  and active_idx = find_index (fst state.active_chat).User.jid 0 buddies
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

let horizontal_line (user, session) fg_color buddy_width scrollback show_buddy_list width =
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
        | _ , Some raw when verified_fp user raw -> (fg_color, " - OTR verified")
        | fp, Some _                             -> (red, " - unverified OTR: " ^ fp)
        | _ , None                               -> (red, " - no OTR")
      in
      (userid user s, " -- " ^ presence, status, otr, otrcolor)
    | None -> (user.jid, "", "", "", black)
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
         state.session.User.presence <- `Offline ;
         state.session.User.status <- None ;
         User.Users.iter (fun _ u ->
             List.iter (fun s -> s.User.presence <- `Offline) u.User.active_sessions)
           state.users ;
       | _ -> () )
   | _ -> () );

  let statusses = status_log state in
  (* network should be an event, then I wouldn't need a check here *)
  (if state.last_status <> network then
     (add_status state (fst network) (snd network) ;
      state.last_status <- network) ) ;

  (* the user in the hashtable might have been replace *)
  (let user = User.Users.find state.users (fst state.active_chat).User.jid in
   state.active_chat <- (user, snd state.active_chat) );


  let log_size = 6 in
  let main_size = size.rows - log_size - 3 (* status + hline + readline *) in
  let buddy_width = 24 in
  let chat_width =
    if state.show_buddy_list then
      size.cols - buddy_width - 1
    else
      size.cols
  in

  if main_size <= 6 || chat_width <= 10 then
    eval ([S "need more space"])
  else
    begin
      let logs =
        let entries =
          if List.length statusses > log_size then
            take log_size statusses []
          else
            statusses
        in
        let entries = log_buffer entries size.cols in
        let data = pad_l_rev "" log_size entries in
        String.concat "\n" data
      in

      let fg_color = color_session (fst state.active_chat) state.user (snd state.active_chat) in

      let main_window =
        let chat =
          let data = match fst state.active_chat with
            | x when x = state.user -> log_buffer statusses chat_width
            | x                     -> message_buffer x.User.message_history chat_width
          in
          (* data is already in right order -- but we need to strip scrollback *)
          let elements = drop (state.scrollback * main_size) (List.rev data) in
          pad_l_rev "" main_size (List.rev elements)
        in

        if state.show_buddy_list then
          (let buddies = buddy_list state main_size buddy_width in
           let comb = List.combine buddies chat in
           let pipe = S (Zed_utf8.singleton (UChar.of_int 0x2502)) in
           List.map (fun (buddy, chat) ->
               buddy @ [ B_fg fg_color ; pipe ; E_fg ; S chat ; S "\n" ])
             comb)
        else
          List.map (fun chat -> [ S chat ; S "\n"]) chat
      in

      let hline = horizontal_line state.active_chat fg_color buddy_width state.scrollback state.show_buddy_list size.cols in

      let notify = List.length state.notifications > 0 in
      let log = (fst state.active_chat).User.preserve_messages in
      let status = status_line time state.user state.session notify log redraw fg_color size.cols in
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

let activate_user ?session state user =
  let session = match session with
    | None   -> User.good_session user
    | Some x -> Some x
  in
  let active = (user, session) in
  if state.active_chat <> active then
    (state.last_active_chat <- state.active_chat ;
     state.active_chat <- active ;
     state.scrollback <- 0 ;
     state.notifications <- List.filter (fun a -> a <> user) state.notifications ;
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
  let userlist = show_buddies state in
  let set_active idx =
    let user = User.Users.find state.users (List.nth userlist idx) in
    activate_user state user
  and active_idx = find_index (fst state.active_chat).User.jid 0 userlist
  in
  match
    direction,
    List.length userlist > succ active_idx,
    pred active_idx >= 0
  with
  | Down, true, _     -> set_active (succ active_idx)
  | Down, false, _    -> set_active 0
  | Up  , _   , true  -> set_active (pred active_idx)
  | Up  , _   , false -> set_active (pred (List.length userlist))

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
      state.show_buddy_list <- not state.show_buddy_list ;
      force_redraw ()
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = ctrlq ->
      if List.length state.notifications > 0 then
        activate_user state (List.hd (List.rev state.notifications))
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = ctrlx ->
      let user, session = state.last_active_chat in
      activate_user ?session state user
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
                       let _, out = Otr.Handshake.end_otr ctx in
                       Xmpp_callbacks.XMPPClient.(send_message x
                                                    ~kind:Chat
                                                    ~jid_to:(JID.of_string jid_to)
                                                    ?body:out ()))
                    otr_sessions
           end >|= fun () -> state )

    | Some message when String.length message > 0 ->
       LTerm_history.add hist message ;
       let err data = log (`Local "error", data) ; return_unit in
       let send_msg user otr_ctx jid_to setter t =
         let ctx, out, user_out = Otr.Handshake.send_otr otr_ctx message in
         setter ctx ;
         let add_msg direction enc data =
           User.new_message user direction enc false data
         in
         (match user_out with
          | `Warning msg      -> add_msg (`Local "OTR Warning") false msg
          | `Sent m           -> add_msg (`To "") false m
          | `Sent_encrypted m -> add_msg (`To "") true m ) ;
         Xmpp_callbacks.XMPPClient.(send_message t
                                      ~kind:Chat
                                      ~jid_to:(JID.of_string jid_to)
                                      ?body:out () )
       in
       ( match fst state.active_chat,
               (User.good_session (fst state.active_chat)),
               !xmpp_session
         with
         | user, _           , _      when user = state.user ->
           err "try `M-x doctor` in emacs instead"
         | user, None        , Some x ->
           let ctx = Otr.State.new_session config.Config.otr_config () in
           send_msg user ctx user.User.jid (fun _ -> ()) x
         | user, Some session, Some x ->
           ( match snd state.active_chat with
             | Some x when x = session -> ()
             | old                     ->
               state.active_chat <- (user, Some session) ;
               match old with
               | None   -> ()
               | Some y ->
                 User.new_message user (`Local "switching active session")
                   false false
                   ("now " ^ session.User.resource ^ " was " ^ y.User.resource)
           ) ;
           let set = fun ctx -> session.User.otr <- ctx in
           send_msg user session.User.otr (User.userid user session) set x
         | _   , _           , None ->
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

