
open Lwt

open LTerm_style
open LTerm_text
open LTerm_geom
open CamomileLibraryDyn.Camomile
open React

let rec take_fill x l acc =
  match x, l with
  | 0, _     -> List.rev acc
  | n, x::xs -> take_fill (pred n) xs (x::acc)
  | n, []    -> take_fill (pred n) [] (""::acc)

type ui_state = {
  user : User.user ; (* set initially *)
  session : User.session ; (* set initially *)
  log : string list ; (* set by xmpp callbacks -- should be time * string list *)
  active_chat : User.user option ; (* modified by user (scrolling through buddies) *)
  users : User.users ; (* extended by xmpp callbacks *)
  notifications : User.user list ; (* or a set? adjusted once messages drop in, reset when chat becomes active *)
}

let empty_ui_state user session users = {
  user ;
  session ;
  log = [] ;
  active_chat = None ;
  users ;
  notifications = []
}

let make_prompt size time state =
  let tm = Unix.localtime time in
  (*  Printf.printf "\n\nblabla r%dc%d\n\n%!" size.rows size.cols ; *)
  let logs = String.concat "\n" (List.rev (take_fill 6 state.log [])) in

  let session = state.session in
  let status = User.presence_to_string session.User.presence in
  let jid = state.user.User.jid ^ "/" ^ session.User.resource in

  let buddies =
    User.Users.fold (fun k u acc ->
        let s = match User.good_session u with
          | None -> `Offline
          | Some s -> s.User.presence
        in
        let f, t =
          if u = state.user then
            ("{", "}")
          else
            User.subscription_to_chars u.User.subscription
        in
        (Printf.sprintf " %s%s%s %s" f (User.presence_to_char s) t k) :: acc)
      state.users []
  in
  (* handle overflowings: text might be too long for one row *)
  let main_size = size.rows - 6 (* log *) - 2 (* status + readline *) in
  assert (main_size > 0) ;
  let buddylist = take_fill main_size buddies [] in

  eval [

    S (String.concat "\n" buddylist) ; S "\n" ;

    B_fg lcyan;
    S (Zed_utf8.make (size.cols) (UChar.of_int 0x2500));
    E_fg;

    S logs ;
    S "\n" ;

    B_bold true;

    B_fg lcyan;
    S"─( ";
    B_fg lmagenta; S(Printf.sprintf "%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min); E_fg;
    S" )─< ";
    B_fg lblue; S jid; E_fg;
    S" >─";
    S(Zed_utf8.make
        (size.cols - 22 - String.length jid - String.length status)
        (UChar.of_int 0x2500));
    S"[ ";
    B_fg lred; S status; E_fg;
    S" ]─";
    E_fg;
    S"\n";

    E_bold;
  ]

let commands =
  [ "/connect" ; "/add" ; "/status" ; "/quit" ]

let time =
  let time, set_time = S.create (Unix.time ()) in
  (* Update the time every 60 seconds. *)
  ignore (Lwt_engine.on_timer 60.0 true (fun _ -> set_time (Unix.time ())));
  time

class read_line ~term ~history ~state ~completions = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method completion =
    let prefix  = Zed_rope.to_string self#input_prev in
    let completions = List.filter (fun f -> Zed_utf8.starts_with f prefix) completions in
    self#set_completion 0 (List.map (fun f -> (f, " ")) completions)

  method show_box = false

  initializer
    self#set_prompt (S.l2 (fun size time -> make_prompt size time state) self#size time)
end

let rec loop term hist state =
  let completions = commands in
  let history = LTerm_history.contents hist in
  match_lwt
    try_lwt
      lwt command = (new read_line ~term ~history ~completions ~state)#run in
      return (Some command)
    with
      | Sys.Break -> return None
      | LTerm_read_line.Interrupt -> return (Some "/quit")
  with
   | Some command when (String.length command > 0) && String.get command 0 = '/' ->
       LTerm_history.add hist command;
       let cmd =
         let ws = try String.index command ' ' with Not_found -> String.length command in
         String.sub command 1 (pred ws)
       in
       let cont = match String.trim cmd with
         | "quit" -> false
         | _ -> Printf.printf "NYI" ; true
       in
       if cont then
         loop term hist { state with log = (command::state.log) }
       else
         return state
   | Some message ->
       LTerm_history.add hist message;
       loop term hist { state with log = (message::state.log) }
   | None -> loop term hist state

let () =
  Lwt_main.run (
    ignore (LTerm_inputrc.load ());
    (* look for -f command line flag *)
    Lwt_unix.getlogin () >>= fun user ->
    Lwt_unix.getpwnam user >>= fun pw_ent ->
    let cfgdir =
      let home = pw_ent.Lwt_unix.pw_dir in
      Filename.concat home ".config"
    in
    Xmpp_callbacks.init cfgdir >>= fun (config, users) ->
    let history = LTerm_history.create [] in
    let user = User.find_or_add config.Config.jid users in
    let session = User.ensure_session config.Config.jid config.Config.otr_config user in
    let state = empty_ui_state user session users in
    Lazy.force LTerm.stdout >>= fun term ->
    loop term history state >>= fun state ->
    Printf.printf "now dumping state %d\n%!" (User.Users.length state.users) ;
    print_newline () ;
    (* dump_users cfgdir x.users *)
    return ()
  )
