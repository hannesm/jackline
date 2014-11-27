
open Lwt

open LTerm_style
open LTerm_text
open LTerm_geom
open CamomileLibraryDyn.Camomile
open React

let make_prompt size time user status =
  let tm = Unix.localtime time in
(*  Printf.printf "\n\nblabla r%dc%d r%dc%d\n\n%!" size.rows size.cols tsize.rows tsize.cols ;
  let matrix = LTerm_draw.make_matrix size in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  LTerm_draw.draw_frame ctx { row1 = 0; col1 = 0; row2 = size.rows; col2 = size.cols } LTerm_draw.Light; *)

  eval [
    S "bla\n" ;
    S(Zed_utf8.make (size.rows - 6) (UChar.of_int 0x0a));
    B_bold true;

    B_fg lcyan;
    S"─( ";
    B_fg lmagenta; S(Printf.sprintf "%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min); E_fg;
    S" )─< ";
    B_fg lblue; S user; E_fg;
    S" >─";
    S(Zed_utf8.make
        (size.cols - 22 - String.length user - String.length status)
        (UChar.of_int 0x2500));
    S"[ ";
    B_fg lred; S status; E_fg;
    S" ]─";
    E_fg;
    S"\n";

(*    B_fg lred; S "user"; E_fg;
    B_fg lgreen; S"@"; E_fg;
      B_fg lblue; S "domain"; E_fg; *)
    B_fg lgreen; S" $ "; E_fg;

    E_bold;
  ]

let commands =
  [ "connect" ; "add" ; "status" ]

let time =
  let time, set_time = S.create (Unix.time ()) in
  (* Update the time every 60 seconds. *)
  ignore (Lwt_engine.on_timer 60.0 true (fun _ -> set_time (Unix.time ())));
  time

class read_line ~term ~history ~user ~completions = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method completion =
    let prefix  = Zed_rope.to_string self#input_prev in
    let completions = List.filter (fun f -> Zed_utf8.starts_with f prefix) completions in
    self#set_completion 0 (List.map (fun f -> (f, " ")) completions)

  initializer
    self#set_prompt (S.l2 (fun size time -> make_prompt size time user "offline") self#size time)
end

let rec loop term history config users =
  let completions = commands in
  let user = JID.string_of_jid config.Config.jid in
  (match_lwt
     try_lwt
       lwt command = (new read_line ~term ~history:(LTerm_history.contents history) ~user ~completions)#run in
       return (Some command)
     with Sys.Break -> return None
   with
     | Some command ->
         
        Printf.printf "executing %s\n%!" command ;
        LTerm_history.add history command;
        return history
     | None -> return history
        ) >>= fun history ->
  loop term history config users

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
    Lazy.force LTerm.stdout >>= fun term ->
    try_lwt
      loop term history config users
    with
      LTerm_read_line.Interrupt ->
        (
          (* dump_config cfgdir !cfg >>= fun () ->
             match !user_data with
             | None -> return ()
             | Some x -> dump_users cfgdir x.users *)
          return ())
  )
