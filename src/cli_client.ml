
open Lwt

open LTerm_style
open LTerm_text
open LTerm_geom
open CamomileLibraryDyn.Camomile
open React

let rec take_fill ?(neutral = "") x l acc =
  match x, l with
  | 0, _     -> List.rev acc
  | n, x::xs -> take_fill ~neutral (pred n) xs (x::acc)
  | n, []    -> take_fill ~neutral (pred n) [] (neutral::acc)

let pad x s =
  match x - (String.length s) with
  | 0 -> s
  | d when d > 0 -> s ^ (String.make d ' ')
  | d (* when d < 0 *) -> String.sub s 0 x

type ui_state = {
  user : User.user ; (* set initially *)
  session : User.session ; (* set initially *)
  mutable log : string list ; (* set by xmpp callbacks -- should be time * string list *)
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

  let main_size = size.rows - 6 (* log *) - 3 (* status + readline *) in
  assert (main_size > 0) ;

  let buddy_width = 24 in

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
        let item = Printf.sprintf " %s%s%s %s" f (User.presence_to_char s) t k in
        (pad buddy_width item) :: acc)
      state.users []
  in
  (* handle overflowings: text might be too long for one row *)

  let buddylist =
    let lst = take_fill ~neutral:(pad buddy_width "") main_size buddies [] in
    List.map (fun x -> x ^ (Zed_utf8.singleton (UChar.of_int 0x2502))) lst
  in
  let hline =
    (Zed_utf8.make buddy_width (UChar.of_int 0x2500)) ^
    (Zed_utf8.singleton (UChar.of_int 0x2534)) ^
    (Zed_utf8.make (size.cols - (succ buddy_width)) (UChar.of_int 0x2500))
  in

  eval [

    S (String.concat "\n" buddylist) ; S "\n" ;

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
    S(Zed_utf8.make
        (size.cols - 22 - String.length jid - String.length status)
        (UChar.of_int 0x2500));
    S"[ ";
    B_fg (if session.User.presence = `Offline then lred else lgreen); S status; E_fg;
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

let rec loop (config : Config.t) term hist state =
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
         | "connect" ->
           let received x = state.log <- (x :: state.log) in
           let otr_config = config.Config.otr_config in
           let (user_data : Xmpp_callbacks.user_data) = Xmpp_callbacks.({
             otr_config ;
             users = state.users ;
             received
           }) in
           Xmpp_callbacks.connect config user_data () ;
           true
         | _ -> Printf.printf "NYI" ; true
       in
       if cont then
         loop config term hist state
       else
         return state
   | Some message ->
       LTerm_history.add hist message;
       loop config term hist state
   | None -> loop config term hist state

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
    loop config term history state >>= fun state ->
    Printf.printf "now dumping state %d\n%!" (User.Users.length state.users) ;
    print_newline () ;
    (* dump_users cfgdir x.users *)
    return ()
  )
