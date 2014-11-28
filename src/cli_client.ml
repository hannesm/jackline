
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
  username : string ;
  status : User.presence ;
  status_message : string option ;
  log : string list ;
  active_chat : User.user option (* not entirely true - might be group or status *) ;
  users : User.users ;
  notifications : User.user list ;
}

let empty_ui_state username users = {
  username ;
  status = `Offline ;
  status_message = None ;
  log = [] ;
  active_chat = None ;
  users = User.Users.empty ;
  notifications = []
}

let make_prompt size time state =
  let tm = Unix.localtime time in
(*  Printf.printf "\n\nblabla r%dc%d r%dc%d\n\n%!" size.rows size.cols tsize.rows tsize.cols ;
  let matrix = LTerm_draw.make_matrix size in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
    LTerm_draw.draw_frame ctx { row1 = 0; col1 = 0; row2 = size.rows; col2 = size.cols } LTerm_draw.Light; *)
  let logs = String.concat "\n" (take_rev_fill 6 state.log []) in
  let status = User.presence_to_string state.status in

  eval [
    S "bla\n" ;
    S(Zed_utf8.make
        (size.rows - 6 (* log *) - 3 (* status + readline + ^ 'bla' *))
        (UChar.of_int 0x0a));

    S logs ;
    S "\n" ;

    B_bold true;

    B_fg lcyan;
    S"─( ";
    B_fg lmagenta; S(Printf.sprintf "%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min); E_fg;
    S" )─< ";
    B_fg lblue; S (state.username); E_fg;
    S" >─";
    S(Zed_utf8.make
        (size.cols - 22 - String.length (state.username) - String.length status)
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
    let state = empty_ui_state (JID.string_of_jid config.Config.jid) users in
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
