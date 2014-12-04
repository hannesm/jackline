
open Lwt

open LTerm_style
open LTerm_text
open LTerm_geom
open CamomileLibraryDyn.Camomile
open React

open Str

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

type ui_state = {
  user : User.user ; (* set initially *)
  session : User.session ; (* set initially *)
  mutable log : (Unix.tm * string * string) list ; (* set by xmpp callbacks -- should be time * string list *)
  mutable active_chat : (User.user * User.session option) ; (* modified by user (scrolling through buddies) *)
  users : User.users ; (* extended by xmpp callbacks *)
  mutable notifications : User.user list ;
}

let empty_ui_state user session users = {
  user ;
  session ;
  log = [] ;
  active_chat = (user, Some session) ;
  users ;
  notifications = []
}

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
    let ent = List.map print_log entries in
    let msgs = pad_l "" 6 ent in
    String.concat "\n" msgs
  in

  let mysession = state.session in
  let status = User.presence_to_string mysession.User.presence in
  let jid = state.user.User.jid ^ "/" ^ mysession.User.resource in

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
        let fg = match session with
          | None -> black
          | Some x -> match Otr.State.(x.User.otr.state.message_state) with
            | `MSGSTATE_ENCRYPTED _ -> lgreen
            | _ -> black
        in
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
  (* handle overflowings: text might be too long for one row *)

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

  let buddylist =
    let buddylst = take_fill [ S (String.make buddy_width ' ') ] main_size buddies [] in
    let chatlst = List.rev (take_fill "" main_size chat []) in
    let comb = List.combine buddylst chatlst in
    List.map (fun (b, c) -> b @ [ B_fg lcyan ; S (Zed_utf8.singleton (UChar.of_int 0x2502)) ; E_fg ; S c ; S "\n" ]) comb
  in
  let hline =
    (Zed_utf8.make buddy_width (UChar.of_int 0x2500)) ^
    (Zed_utf8.singleton (UChar.of_int 0x2534)) ^
    (Zed_utf8.make (size.cols - (succ buddy_width)) (UChar.of_int 0x2500))
  in

  eval (
    List.flatten buddylist @ [

    B_fg lcyan;
    S hline ;
    E_fg;
    S "\n" ;

    S logs ;
    S "\n" ;

    B_bold true;

    B_fg lcyan;
    S"─( ";
    B_fg lmagenta; S(Printf.sprintf "%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min); E_fg;
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

let commands =
  [ "/connect" ; "/add" ; "/status" ; "/quit"; "/help" ]

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

class read_line ~term ~network ~history ~state ~completions = object(self)
  inherit LTerm_read_line.read_line ~history () as super
  inherit [Zed_utf8.t] LTerm_read_line.term term as t

  method completion =
    let prefix  = Zed_rope.to_string self#input_prev in
    let completions = List.filter (fun f -> Zed_utf8.starts_with f prefix) completions in
    self#set_completion 0 (List.map (fun f -> (f, " ")) completions)

  method show_box = false

  method send_action = function
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = down ->
      let userlist = User.keys state.users in
      let active_idx = find_index (fst state.active_chat).User.jid 0 userlist in
      if List.length userlist > (succ active_idx) then
        (let user = User.Users.find state.users (List.nth userlist (succ active_idx)) in
         let session = User.good_session user in
         state.active_chat <- (user, session) ;
         state.notifications <- List.filter (fun a -> a <> user) state.notifications ) ;
      force_redraw ()
    | LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert k)) when k = up ->
      let userlist = User.keys state.users in
      let active_idx = find_index (fst state.active_chat).User.jid 0 userlist in
      if pred active_idx >= 0 then
        (let user = User.Users.find state.users (List.nth userlist (pred active_idx)) in
         let session = User.good_session user in
         state.active_chat <- (user, session) ;
         state.notifications <- List.filter (fun a -> a <> user) state.notifications ) ;
      force_redraw ()
    | action ->
      super#send_action action

  initializer
    LTerm_read_line.bind [LTerm_key.({ control = false; meta = false; shift = false; code = Prev_page })] [LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert up))];
    LTerm_read_line.bind [LTerm_key.({ control = false; meta = false; shift = false; code = Next_page })] [LTerm_read_line.Edit (LTerm_edit.Zed (Zed_edit.Insert down))];
    self#set_prompt (S.l4 (fun size time network redraw -> make_prompt size time network state redraw)
                       self#size time network redraw)
end

let split_ws s =
  let l = String.length s in
  let ws = try String.index s ' ' with Not_found -> l in
  let ws' = if ws = l then ws else succ ws in
  String.(sub s 0 ws, sub s ws' (l - ws'))

let set_status session_data status =
  let open Xmpp_callbacks.XMPPClient in
  let p, msg = split_ws status in
  let presence = User.string_to_presence p in
  let kind, show = match presence with
    | `Offline -> Some Unavailable, None
    | `Online -> None, None
    | `Free -> None, Some ShowChat
    | `Away -> None, Some ShowAway
    | `DoNotDisturb -> None, Some ShowDND
    | `ExtendedAway -> None, Some ShowXA
  in
  let status = if msg = "" then None else Some msg in
  send_presence session_data ?kind ?show ?status ()

let rec loop (config : Config.t) term hist state session_data network s_n =
  let completions = commands in
  let history = LTerm_history.contents hist in
  match_lwt
    try_lwt
      lwt command = (new read_line ~term ~history ~completions ~state ~network)#run in
      return (Some command)
    with
      | Sys.Break -> return None
      | LTerm_read_line.Interrupt -> return (Some "/quit")
  with
   | Some command when (String.length command > 0) && String.get command 0 = '/' ->
       LTerm_history.add hist command;
       let cmd, args = split_ws (String.sub command 1 (pred (String.length command)))
       and now = Unix.localtime (Unix.time ())
       in
       (match cmd, args with
        | "quit", _ -> return (false, session_data)
        | "help", _ -> s_n (now, "help", String.concat " " (List.sort compare commands) ) ;
                            return (true, session_data)
        | "connect", _ ->
          (match session_data with
           | None ->
             let otr_config = config.Config.otr_config in
             let cb jid msg =
               let now = Unix.localtime (Unix.time ()) in
               s_n (now, jid, msg)
             and notify u =
               (if (List.mem u state.notifications) || (fst state.active_chat = u) then
                  ()
                else
                  state.notifications <- u :: state.notifications) ;
               force_redraw ()
             in
             let (user_data : Xmpp_callbacks.user_data) = Xmpp_callbacks.({
                 otr_config ;
                 users = state.users ;
                 received = cb ;
                 notify ;
               }) in
             (* TODO: I'd like to catch tls and auth failures here, but neither try_lwt nor Lwt.catch seem to do that *)
             (Xmpp_callbacks.connect config user_data () >|= fun s -> Some s) >>= fun session_data ->
             (match session_data with
               | None -> return (true, None)
               | Some s ->
                 Lwt.async (fun () -> Xmpp_callbacks.parse_loop s) ;
                 return (true, Some s))
           | Some _ -> s_n (now, "error", "already connected") ; return (true, session_data))
        | c, a ->
          ( match session_data with
            | None -> s_n (now, "error", "not connected") ; return_unit
            | Some s -> match c with
              | "status" -> set_status s a
              | _ -> s_n (now, "error", "unimplemented command: " ^ command) ; return_unit
          )  >|= fun () -> (true, session_data) ) >>= fun (cont, session_data) ->
       if cont then
         loop config term hist state session_data network s_n
       else
         (* close! *)
         return state
     | Some message when String.length message > 0 ->
       LTerm_history.add hist message;
       let user, session = match state.active_chat with
         | (user, None) -> assert false
         | (user, Some x) -> user, x
       in
       if user = state.user then
         loop config term hist state session_data network s_n
       else
         let ctx, out, user_out = Otr.Handshake.send_otr session.User.otr message in
         session.User.otr <- ctx ;
         let enc = match Otr.State.(ctx.state.message_state) with
           | `MSGSTATE_ENCRYPTED _ -> true
           | _ -> false
         in
         (match user_out with
          | None -> ()
          | Some w -> session.User.messages <- (`To, enc, false, Unix.localtime (Unix.time ()), w) :: session.User.messages) ;
         (match session_data with
          | None -> s_n (Unix.localtime (Unix.time ()), "error", "not connected, cannot send: " ^ message) ; return_unit
          | Some x -> Xmpp_callbacks.XMPPClient.send_message x
                        ~jid_to:(JID.of_string user.User.jid)
                        ?body:out () ) >>= fun () ->
         loop config term hist state session_data network s_n
     | Some message -> loop config term hist state session_data network s_n
     | None -> loop config term hist state session_data network s_n

