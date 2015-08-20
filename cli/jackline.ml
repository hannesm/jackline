open Lwt
open React

let start_client cfgdir debug () =
  ignore (LTerm_inputrc.load ());

  Printexc.register_printer (function
      | Tls_lwt.Tls_alert x -> Some ("TLS alert: " ^ Tls.Packet.alert_type_to_string x)
      | Tls_lwt.Tls_failure f -> Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
      | _ -> None) ;

  Lazy.force LTerm.stdout >>= fun term ->

  Persistency.load_dsa cfgdir >>= fun dsa ->

  Persistency.load_config dsa cfgdir >>= ( function
      | None ->
        Cli_config.configure term () >>= fun config ->
        Persistency.dump_config cfgdir config >>= fun () ->
        ( match config.Config.password with
          | None -> return_unit
          | Some x -> Persistency.dump_password cfgdir x ) >>= fun () ->
        Persistency.dump_dsa cfgdir config.Config.dsa >|= fun () ->
        config
      | Some cfg -> return cfg ) >>= fun config ->

  ( match config.Config.password with
    | None -> Persistency.load_password cfgdir
    | Some x -> return (Some x)) >>= fun password ->
  let config = { config with Config.password = password } in

  ( match config.Config.password with
    | None ->
       let jid = User.Jid.full_jid_to_string config.Config.jid in
       (new Cli_config.read_password term ~prompt:("password for " ^ jid ^ ": "))#run >|= fun password ->
       Some password
    | Some x -> return (Some x)) >>= fun password ->
  let config = { config with Config.password = password } in

  Persistency.load_users cfgdir >>= fun users ->
  let users_sexp_existed = User.Users.length users > 0 in

  Persistency.load_user_dir cfgdir users >>= fun () ->

  let history = LTerm_history.create [] in

  (* setup self contact *)
  let myjid = config.Config.jid in
  let (bare, resource) = myjid in
  let user = User.find_or_create users bare in
  let user, _ = User.find_or_create_session user resource config.Config.otr_config config.Config.dsa in
  User.Users.replace users bare user ;

  let state = Cli_state.empty_state cfgdir config users in
  let n, log = S.create (`Local "welcome to jackline", "type /help for help") in

  let us = User.Users.fold (fun _ v acc -> v :: acc) users [] in

  (if users_sexp_existed then
     (* write out all the users to users/ *)
     Lwt_list.iter_s (Lwt_mvar.put state.Cli_state.user_mvar) us >>= fun () ->
     (* delete users.sexp *)
     Persistency.delete (Filename.concat cfgdir Persistency.users)
   else
     Lwt.return_unit) >>= fun () ->

  (* dump histories every 10 minutes *)
  let hist_dumper =
    let dump () = Persistency.dump_histories cfgdir users in
    Lwt_engine.on_timer 600. true (fun _ -> Lwt.async dump)
  in

  (if debug then
     Persistency.open_append (Unix.getenv "PWD") "out.txt" >|= fun fd ->
     Some fd
   else
     return None) >>= fun out ->

  Cli_client.init_system (log ?step:None) (snd bare) ;

  ignore (LTerm.save_state term);  (* save the terminal state *)

  (* main loop *)
  Cli_client.loop ?out term history state n (log ?step:None) >>= fun state ->

  (match out with
   | None -> return_unit
   | Some fd -> Lwt_unix.close fd) >>= fun () ->

  Lwt_mvar.put state.Cli_state.state_mvar Cli_state.Quit >>= fun () ->

  (* cancel history dumper *)
  Lwt_engine.stop_event hist_dumper ;
  (* store histories a last time *)
  Persistency.dump_histories cfgdir users >>= fun () ->

  (* restore the terminal state *)
  LTerm.load_state term


let config_dir = ref ""
let debug = ref false
let rest = ref []

let _ =
  let home = Unix.getenv "HOME" in
  let cfgdir = Filename.concat home ".config" in
  config_dir := Filename.concat cfgdir "ocaml-xmpp-client"

let usage = "usage " ^ Sys.argv.(0)

let arglist = [
  ("-f", Arg.String (fun d -> config_dir := d), "configuration directory (defaults to ~/.config/ocaml-xmpp-client/)") ;
  ("-d", Arg.Bool (fun d -> debug := d), "log to out.txt in current working directory")
]

let _ =
  try
    Arg.parse arglist (fun x -> rest := x :: !rest) usage ;
    Lwt_main.run (start_client !config_dir !debug ())
  with
  | Sys_error s -> print_endline s
