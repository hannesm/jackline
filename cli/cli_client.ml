
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
  | [] -> assert false
  | x::xs when x = id -> i
  | _::xs -> find_index id (succ i) xs

let color_session u su= function
  | Some x when User.(encrypted x.otr) -> green
  | Some _ when u = su -> black
  | Some _ -> red
  | None -> black

let make_prompt size time network state redraw =

  let rec line_wrap ~max_length ent acc : string list =
    match ent with
    | entry::remaining when String.length entry > max_length ->
        let part1 = String.sub entry 0 max_length
        and part2 = "  " ^ String.sub entry max_length ((String.length entry) - max_length)
        in
          line_wrap ~max_length (part2::remaining) (part1::acc)
    | entry::remaining ->
        line_wrap ~max_length remaining (entry::acc)
    | [] -> acc
  in

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

  let buddies =
    let us = User.keys state.users in
    List.map (fun id ->
        let u = User.Users.find state.users id in
        let session = User.good_session u in
        let s = match session with
          | None -> `Offline
          | Some s -> s.User.presence
        in
        let fg = color_session u state.user session in
        let f, t =
          if u = state.user then
            ("{", "}")
          else
            User.subscription_to_chars u.User.subscription
        in
        let bg = if (fst state.active_chat) = u then 7 else 15 in
        let item =
          let data = Printf.sprintf " %s%s%s %s" f (User.presence_to_char s) t id in
          pad buddy_width data
        in
        let blinka, blinkb = if List.mem u state.notifications then ([ B_blink true ], [ E_blink ]) else ([], []) in
        blinka @ [B_fg fg ; B_bg(index bg) ; S item ; E_bg ; E_fg ] @ blinkb)
      us
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
    match snd state.active_chat with
      | None -> []
      | Some x when x = state.session -> List.map print_log state.log
      | Some x -> List.map printmsg x.User.messages
  in

  let fg_color = color_session (fst state.active_chat) state.user (snd state.active_chat) in

  let buddylist =
    let buddylst = take_fill [ S (String.make buddy_width ' ') ] main_size buddies [] in
    let chat_wrap_length = (size.cols - buddy_width - 1 (* hline char *)) in
    let chat = line_wrap ~max_length:chat_wrap_length (List.rev chat) [] in
    let chatlst = List.rev (take_fill "" main_size chat []) in
    let comb = List.combine buddylst chatlst in
    List.map (fun (b, c) -> b @ [ B_fg fg_color ; S (Zed_utf8.singleton (UChar.of_int 0x2502)) ; E_fg ; S c ; S "\n" ]) comb
  in

  let hline =
    let buddy, pres = match state.active_chat with
      | u, Some s ->
        let p = User.presence_to_string s.User.presence in
        let status = match s.User.status with | None -> "" | Some x -> " - " ^ x in
        let otr, _ = User.fingerprint s.User.otr in
        (User.userid u s, " - " ^ p ^ status ^ " " ^ otr)
      | u, None -> (u.User.jid, "")
    in
    let txt = " chatting with " ^ buddy ^ pres ^ " " in
    (Zed_utf8.make buddy_width (UChar.of_int 0x2500)) ^
    (Zed_utf8.singleton (UChar.of_int 0x2534)) ^
    txt ^
    (Zed_utf8.make (size.cols - (succ buddy_width) - (String.length txt)) (UChar.of_int 0x2500))
  in

  let mysession = state.session in
  let status = User.presence_to_string mysession.User.presence in
  let jid = User.userid state.user mysession in


  eval (
    List.flatten buddylist @ [

    B_fg fg_color;
    S hline ;
    E_fg;
    S "\n" ;

    S logs ;
    S "\n" ;

    B_bold true;

    B_fg fg_color;
    S"─( ";
    S(Printf.sprintf "%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min);
    S" )─< ";
    B_fg lblue; S jid; E_fg;
    S" >─";
    S redraw ;
    S(Zed_utf8.make
        (size.cols - 22 - String.length jid - String.length status - String.length redraw)
        (UChar.of_int 0x2500));
    S"[ ";
    B_fg (if mysession.User.presence = `Offline then lred else lgreen); S status; E_fg;
    S" ]─";
    E_fg;
    S"\n";

    E_bold;
  ])

let time =
  let time, set_time = S.create (Unix.time ()) in
  (* Update the time every 60 seconds. *)
  ignore (Lwt_engine.on_timer 60.0 true (fun _ -> set_time (Unix.time ())));
  time

let up = UChar.of_int 0x2500
let down = UChar.of_int 0x2501

let redraw, force_redraw =
  (* this is just an ugly hack which should be removed *)
  let a, b = S.create "" in
  (a, fun () -> b "bla" ; b "")

let navigate_buddy_list state direction (* true - up ; false - down *) =
  let userlist = User.keys state.users in
  let active_idx = find_index (fst state.active_chat).User.jid 0 userlist in
  let user_idx =
    if (not direction) && List.length userlist > (succ active_idx) then
      Some (succ active_idx)
    else if direction && pred active_idx >= 0 then
      Some (pred active_idx)
    else
      None
  in
  match user_idx with
  | Some idx ->
    let user = User.Users.find state.users (List.nth userlist idx) in
    let session = User.good_session user in
    state.active_chat <- (user, session) ;
    state.notifications <- List.filter (fun a -> a <> user) state.notifications ;
    force_redraw ()
  | None -> ()

class read_line ~term ~network ~history ~state = object(self)
  inherit LTerm_read_line.read_line ~history () as super
  inherit [Zed_utf8.t] LTerm_read_line.term term as t

  method completion =
    let prefix  = Zed_rope.to_string self#input_prev in
    let completions = Cli_commands.topcompletion prefix in
    self#set_completion 0 (List.map (fun f -> ("/" ^ f, " ")) completions)

  method show_box = false

  method send_action = function
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = down ->
      navigate_buddy_list state false
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = up ->
      navigate_buddy_list state true
    | action ->
      super#send_action action

  initializer
    LTerm_read_line.bind [LTerm_key.({ control = false; meta = false; shift = false; code = Prev_page })] [LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert up))];
    LTerm_read_line.bind [LTerm_key.({ control = false; meta = false; shift = false; code = Next_page })] [LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert down))];
    self#set_prompt (S.l4 (fun size time network redraw -> make_prompt size time network state redraw)
                       self#size time network redraw)
end

let rec loop (config : Config.t) term hist state session_data network s_n =
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
      Cli_commands.exec command state config session_data s_n force_redraw >>= fun (cont, session_data) ->
      if cont then
        loop config term hist state session_data network s_n
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
                Xmpp_callbacks.XMPPClient.send_message x ~jid_to:(JID.of_string jid_to) ?body:out ())
             otr_sessions
             (* close connection! *)
        ) >|= fun () -> state
    | Some message when String.length message > 0 ->
       LTerm_history.add hist message;
       (match state.active_chat with
         | (user, _) when user = state.user -> return_unit
         | (user, None) ->
           s_n (Unix.localtime (Unix.time ()), "error", "no active session, cannot send") ;
           return_unit
         | (user, Some session) ->
           let ctx, out, user_out = Otr.Handshake.send_otr session.User.otr message in
           session.User.otr <- ctx ;
           let enc = User.encrypted ctx in
           (match user_out with
            | None -> ()
            | Some w -> session.User.messages <- (`To, enc, false, Unix.localtime (Unix.time ()), w) :: session.User.messages) ;
           (match session_data with
            | None -> s_n (Unix.localtime (Unix.time ()), "error", "not connected, cannot send: " ^ message) ; return_unit
            | Some x -> Xmpp_callbacks.XMPPClient.send_message x
                          ~jid_to:(JID.of_string user.User.jid)
                          ?body:out () ) ) >>= fun () ->
       loop config term hist state session_data network s_n
     | Some message -> loop config term hist state session_data network s_n
     | None -> loop config term hist state session_data network s_n

