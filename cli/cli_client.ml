
open Lwt

open LTerm_style
open LTerm_text
open LTerm_geom
open CamomileLibraryDyn.Camomile
open React

open Cli_state

let rec take_rev x l acc =
  match x, l with
  | 0, _ -> acc
  | n, [] -> acc
  | n, x :: xs -> take_rev (pred n) xs (x :: acc)

let rec take_fill neutral x l acc =
  match x, l with
  | 0, _     -> List.rev acc
  | n, x::xs -> take_fill neutral (pred n) xs (x::acc)
  | n, []    -> take_fill neutral (pred n) [] (neutral::acc)

let rec take x l acc =
  match x, l with
  | 0, _       -> List.rev acc
  | n, x :: xs -> take (pred n) xs (x :: acc)
  | n, _       -> assert false

let rec drop x l =
  match x, l with
  | 0, xs      -> xs
  | n, x :: xs -> drop (pred n) xs
  | n, []      -> []

let rec pad_l neutral x l =
  match x - (List.length l) with
  | 0 -> l
  | d when d > 0 ->  pad_l neutral x (neutral :: l)
  | d -> assert false

let pad x s =
  match x - (String.length s) with
  | 0 -> s
  | d when d > 0 -> s ^ (String.make d ' ')
  | d (* when d < 0 *) -> String.sub s 0 x

let rec find_index id i = function
  | [] -> 0
  | x::xs when x = id -> i
  | _::xs -> find_index id (succ i) xs

let color_session u su= function
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
  let open String in
  match entries with
  | entry::remaining when contains entry '\n' ->
    let part1     = sub entry 0 (index entry '\n') in
    let part1_len = 1 + length part1 in (* +1: account for \n *)
    let part2     = "  " ^ sub entry part1_len ((length entry) - part1_len) in
    let acc       = if 0 <> length (trim part1) then part1::acc else acc
    and remaining = if 0 <> length (trim part2) then part2::remaining else remaining
    in
    line_wrap ~max_length remaining acc
  | entry::remaining when (length entry) > max_length ->
    let part1 = sub entry 0 max_length
    and part2 = "  " ^ sub entry max_length ((length entry) - max_length)
    in
    line_wrap ~max_length (part2::remaining) (part1::acc)
  | entry::remaining ->
    line_wrap ~max_length remaining (entry::acc)
  | [] -> acc

let make_prompt size time network state redraw =
  let tm = Unix.localtime time in

  (* network should be an event, then I wouldn't need a check here *)
  (if List.length state.log = 0 || List.hd state.log <> network then
     state.log <- (network :: state.log)) ;

  let print_log (lt, from, msg) =
    let time = Printf.sprintf "[%02d:%02d:%02d] " lt.Unix.tm_hour lt.Unix.tm_min lt.Unix.tm_sec in
    time ^ from ^ ": " ^ msg
  in
  let logs =
    let entries = take_rev 6 state.log [] in
    let entries = List.map print_log entries in
    let log_entries = line_wrap ~max_length:size.cols entries [] in
    String.concat "\n" (List.rev (take_fill "" 6 log_entries  []))
  in

  let main_size = size.rows - 6 (* log *) - 3 (* status + readline *) in
  assert (main_size > 0) ;

  let buddy_width = 24 in

  let buddies = show_buddies state in
  let active_idx = find_index (fst state.active_chat).User.jid 0 buddies in

  let buddies =
    List.mapi (fun x id ->
        let u = User.Users.find state.users id in
        let session = User.good_session u in
        let presence = match session with
          | None -> `Offline
          | Some s -> s.User.presence
        in
        let fg = color_session u state.user session in
        let bg = if x = active_idx then 7 else 15 in
        let f, t =
          if u = state.user then
            ("{", "}")
          else
            User.subscription_to_chars u.User.subscription
        in
        let item =
          let data = Printf.sprintf " %s%s%s %s" f (User.presence_to_char presence) t id in
          pad buddy_width data
        in
        let show = [B_fg fg ; B_bg(index bg) ; S item ; E_bg ; E_fg ] in
        if List.mem u state.notifications then
          B_blink true :: show @ [ E_blink ]
        else
          show)
      buddies
  in

  let chat =
    let printmsg (dir, enc, received, lt, msg) =
      let time = Printf.sprintf "[%02d:%02d:%02d] " lt.Unix.tm_hour lt.Unix.tm_min lt.Unix.tm_sec in
      let en = if enc then "O" else "-" in
      let pre = match dir with
        | `From -> "<" ^ en ^ "- "
        | `To -> (if received then "-" else "r") ^ en ^ "> "
        | `Local -> "*** "
      in
      time ^ pre ^ msg
    in
    (match snd state.active_chat, User.good_session (fst state.active_chat) with
     | None, Some x -> state.active_chat <- (fst state.active_chat, Some x)
     | Some x, Some y when x <> y -> state.active_chat <- (fst state.active_chat, Some y)
     | _ -> () );
    match snd state.active_chat with
    | None -> []
    | Some x when x = state.session -> List.map print_log state.log
    | Some x -> List.map printmsg x.User.messages
  in

  let fg_color = color_session (fst state.active_chat) state.user (snd state.active_chat) in

  let buddylist =
    let buddylst =
      let bs = List.length buddies in
      if main_size >= bs then
        take_fill [ S (String.make buddy_width ' ') ] main_size buddies []
      else
        (* active_idx in the middle *)
        let up, down = (main_size / 2, (main_size + 1) / 2) in
        let from =
          if active_idx - up >= 0 then
            if active_idx + down > bs then
              bs - main_size
            else
              active_idx - up
          else
            0
        in
        take main_size (drop from buddies) []
    in
    let chat_wrap_length = (size.cols - buddy_width - 1 (* hline char *)) in
    let chat = line_wrap ~max_length:chat_wrap_length (List.rev chat) [] in
    let chatlst = List.rev (take_fill "" main_size chat []) in
    let comb = List.combine buddylst chatlst in
    List.map (fun (b, c) -> b @ [ B_fg fg_color ; S (Zed_utf8.singleton (UChar.of_int 0x2502)) ; E_fg ; S c ; S "\n" ]) comb
  in

  let hline =
    let buddy, pres, col, otr = match state.active_chat with
      | u, Some s ->
        let p = User.presence_to_string s.User.presence in
        let status = match s.User.status with | None -> "" | Some x -> " - " ^ x in
        let otr, col = match User.fingerprint s.User.otr with
          | fp, Some raw when User.verified_fp u raw -> (" - OTR verified", fg_color)
          | fp, Some raw -> (" - unverified OTR: " ^ fp, red)
          | fp, None -> (" - no OTR", red)
        in
        (User.userid u s, " -- " ^ p ^ status, col, otr)
      | u, None -> (u.User.jid, "", black, "")
    in
    let pre = (Zed_utf8.make buddy_width (UChar.of_int 0x2500)) ^ (Zed_utf8.singleton (UChar.of_int 0x2534)) in
    let txt = " buddy: " ^ buddy in
    let leftover = size.cols - (String.length txt) - buddy_width - 1 in
    if leftover > 0 && (String.length otr) < leftover then
      let leftover' = leftover - (String.length otr) in
      let post =
        if (String.length pres) < leftover' then
          let pos = Zed_utf8.make (leftover' - (String.length pres) - 1) (UChar.of_int 0x2500) in
          [ B_fg fg_color ; S pres ; S " " ; S pos ; E_fg ]
        else
          [ B_fg fg_color ; S (String.sub pres 0 leftover') ; E_fg ]
      in
      [ B_fg fg_color ; S pre ; S txt ; E_fg ; B_fg col ; S otr ; E_fg ] @ post
    else if leftover > 0 then
      [ B_fg fg_color ; S pre ; S txt ; E_fg ; B_fg col ; S (String.sub otr 0 leftover) ; E_fg ]
    else if (String.length txt) < size.cols then
      let pos = Zed_utf8.make (size.cols - (String.length txt) - 1) (UChar.of_int 0x2500) in
      [ B_fg fg_color ; S txt ; S " " ; S pos ; E_fg ]
    else
      [ B_fg fg_color ; S (String.sub txt 0 size.cols) ; E_fg ]
  in


  let status =
    let mysession = state.session in
    let status = User.presence_to_string mysession.User.presence in
    let jid = User.userid state.user mysession in
    let time = Printf.sprintf "%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min in

    let first =
      if List.length state.notifications > 0 then
        [ B_blink true ; B_fg blue ; S "#" ; E_fg ; E_blink ]
      else
        [ B_fg fg_color ; S (Zed_utf8.make 1 (UChar.of_int 0x2500)) ; E_fg ]
    in


    let leftover = size.cols - (String.length jid) - 6 in
    let jid, left =
      if leftover > 0 then
        ([ S "< "; B_fg lblue; S jid; E_fg; S" >─" ], leftover)
      else if (size.cols > String.length jid) then
        ([ B_fg blue ; S jid ; E_fg ], size.cols - String.length jid)
      else
        ([ B_fg blue ; S (String.sub jid 0 (pred size.cols)) ; E_fg ], 0)
    in

    let leftover' = left - (String.length status) - 5 in
    let status, left =
      if leftover' > 0 then
        ([ S "[ " ;
           B_fg (if mysession.User.presence = `Offline then lred else lgreen);
           S status;
           E_fg ;
           S" ]─" ],
         leftover')
      else
        ([], left)
    in

    let left = left - (String.length redraw) in

    let leftover''' = left - 11 in
    let time, left =
      if leftover''' > 0 then
        ([ S "─( " ; S time ; S " )─" ], leftover''')
      else
        ([], left)
    in

    let fill =
      if left > 0 then
        Zed_utf8.make left (UChar.of_int 0x2500)
      else
        ""
    in
    [ B_bold true ] @ first @
    [ B_fg fg_color ] @
      time @ jid @
      [ S redraw ; S fill ] @
      status @
    [ E_fg; S"\n"; E_bold ]
  in

  eval (
    List.flatten buddylist @ hline @ [ S "\n" ; S logs ; S "\n" ] @ status
  )

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

let redraw, force_redraw =
  (* this is just an ugly hack which should be removed *)
  let a, b = S.create "" in
  (a, fun () -> b "bla" ; b "")

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
     state.notifications <- List.filter (fun a -> a <> user) state.notifications ;
     force_redraw ())

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
  | Down, true, _    -> set_active (succ active_idx)
  | Up  , _   , true -> set_active (pred active_idx)
  | _                -> ()

class read_line ~term ~network ~history ~state = object(self)
  inherit LTerm_read_line.read_line ~history () as super
  inherit [Zed_utf8.t] LTerm_read_line.term term as t

  method completion =
    let prefix  = Zed_rope.to_string self#input_prev in
    let completions = Cli_commands.completion prefix in
    self#set_completion 0 completions

  method complete =
    try super#complete with
    | _ -> ()

  method show_box = false

  method send_action = function
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = down ->
      navigate_buddy_list state Down
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = up ->
      navigate_buddy_list state Up
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = f5 ->
      state.show_offline <- not state.show_offline ;
      force_redraw ()
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = f12 ->
      ()
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
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = false; code = F5 })] [Edit (LTerm_edit.Zed (Zed_edit.Insert f5))]);
    LTerm_read_line.(bind [LTerm_key.({ control = false; meta = false; shift = false; code = F12 })] [Edit (LTerm_edit.Zed (Zed_edit.Insert f12))]);
    LTerm_read_line.(bind [LTerm_key.({ control = true; meta = false; shift = false; code = Char (UChar.of_int 0x71) })] [Edit (LTerm_edit.Zed (Zed_edit.Insert ctrlq))]);
    LTerm_read_line.(bind [LTerm_key.({ control = true; meta = false; shift = false; code = Char (UChar.of_int 0x78) })] [Edit (LTerm_edit.Zed (Zed_edit.Insert ctrlx))]);
    self#set_prompt (S.l4 (fun size time network redraw -> make_prompt size time network state redraw)
                       self#size time network redraw)
end

let rec loop ?out (config : Config.t) term hist state session_data network s_n =
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
      LTerm_history.add hist command;
      Cli_commands.exec ?out command state config session_data s_n force_redraw >>= fun (cont, session_data) ->
      if cont then
        loop ?out config term hist state session_data network s_n
      else
        (match session_data with
         | None -> return_unit
         | Some x ->
           let otr_sessions = User.Users.fold (fun _ u acc ->
               List.fold_left (fun acc s ->
                   if User.encrypted s.User.otr then
                     ((User.userid u s), s.User.otr) :: acc
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
             (* close connection! *)
        ) >|= fun () -> state
    | Some message when String.length message > 0 ->
       LTerm_history.add hist message;
       let err data = s_n (Unix.localtime (Unix.time ()), "error", data) ; return_unit in
       let send_msg user session t =
         let ctx, out, user_out = Otr.Handshake.send_otr session.User.otr message in
         session.User.otr <- ctx ;
         let add_msg direction enc data =
           let msg =
             let now = Unix.localtime (Unix.time ()) in
             (direction, enc, false, now, data)
           in
           session.User.messages <- msg :: session.User.messages
         in
         (match user_out with
          | `Warning msg      -> add_msg `Local false msg
          | `Sent m           -> add_msg `To false m
          | `Sent_encrypted m -> add_msg `To true m ) ;
         let jid_to = if session.User.resource = "/special/" then
             user.User.jid
           else
             user.User.jid ^ "/" ^ session.User.resource
         in
         Xmpp_callbacks.XMPPClient.(send_message t
                                      ~kind:Chat
                                      ~jid_to:(JID.of_string jid_to)
                                      ?body:out ())
       in
       ( match state.active_chat, session_data with
         | (user, _), _ when user = state.user -> return_unit
         | (user, None), Some x ->
           let dummy_session = User.empty_session "/special/" config.Config.otr_config () in
           user.User.active_sessions <- dummy_session :: user.User.active_sessions ;
           send_msg user dummy_session x
         | (user, Some session), Some x -> send_msg user session x
         | _, None -> err "no active session, try to connect first" ) >>= fun () ->
       loop ?out config term hist state session_data network s_n
     | Some message -> loop ?out config term hist state session_data network s_n
     | None -> loop ?out config term hist state session_data network s_n

