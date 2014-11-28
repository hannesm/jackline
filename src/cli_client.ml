
open Lwt

open LTerm_style
open LTerm_text
open LTerm_geom
open CamomileLibraryDyn.Camomile
open React

let rec take_rev_fill x l acc =
  match x, l with
  | 0, _     -> acc
  | n, x::xs -> take_rev_fill (pred n) xs (x::acc)
  | n, []    -> take_rev_fill (pred n) [] ("no"::acc)

type ui_state = {
  user : User.user ; (* set initially *)
  session : User.session ; (* set initially *)
  log : string list ; (* set by xmpp callbacks -- should be time * string list *)
  active_chat : User.user option (* not entirely true - might also be group or status -- focus! *) ;
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
  let logs = String.concat "\n" (take_rev_fill 6 state.log []) in

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
  let buddylist = String.concat "\n" buddies in

  eval [
    S "bla\n" ;

    S buddylist ; S "\n" ;

    S(Zed_utf8.make
        (size.rows - 6 (* log *) - 4 (* status + readline + ^ 'bla' *) - (List.length buddies))
        (UChar.of_int 0x0a));

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

(*    B_fg lred; S "user"; E_fg;
    B_fg lgreen; S"@"; E_fg;
      B_fg lblue; S "domain"; E_fg;
      B_fg lgreen; S" $ "; E_fg; *)

    E_bold;
  ]

let commands =
  [ "connect" ; "connect foo" ; "add" ; "status" ]

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

let rec loop term history state =
  let completions = commands in
  (match_lwt
     try_lwt
       lwt command = (new read_line ~term ~history:(LTerm_history.contents history) ~completions ~state)#run in
       return (Some command)
     with Sys.Break -> return None
   with
     | Some command ->
        Printf.printf "executing %s\n%!" command ;
        LTerm_history.add history command;
        return (command, history)
     | None -> return ("", history)
  ) >>= fun (li, history) ->
  loop term history { state with log = (li::state.log) }

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
    Printf.printf "your config %s\n%!" (Config.store_config config) ;
    let history = LTerm_history.create [] in
    let user, users = User.find_or_add config.Config.jid users in
    let session = User.ensure_session config.Config.jid config.Config.otr_config user in
    let state = empty_ui_state user session users in
    Lazy.force LTerm.stdout >>= fun term ->
    try_lwt
      loop term history state
    with
      LTerm_read_line.Interrupt ->
        (
          (* dump_config cfgdir !cfg >>= fun () ->
             match !user_data with
             | None -> return ()
             | Some x -> dump_users cfgdir x.users *)
          return ())
  )
