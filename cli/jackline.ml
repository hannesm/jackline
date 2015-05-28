open Lwt
open React

let start_client cfgdir debug () =
  ignore (LTerm_inputrc.load ());
  Nocrypto_entropy_lwt.initialize () >>= fun () ->

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
        (new Cli_config.read_password term ~prompt:"password: ")#run >|= fun password ->
        Some password
    | Some x -> return (Some x)) >>= fun password ->
  let config = { config with Config.password = password } in

  Persistency.load_users cfgdir >>= fun (users) ->

  let history = LTerm_history.create [] in

  (* setup self contact *)
  let jid, resource = User.bare_jid config.Config.jid in
  let user = User.find_or_create users jid in
  let user, _ = User.find_or_create_session user resource config.Config.otr_config config.Config.dsa in
  User.Users.replace users jid user ;

  let state = Cli_state.empty_ui_state cfgdir jid resource users in
  let n, log = S.create (`Local "welcome to jackline", "type /help for help") in

  ( if debug then
      Persistency.open_append (Unix.getenv "PWD") "out.txt" >|= fun fd ->
      Some fd
    else
      return None ) >>= fun out ->

  Cli_client.init_system (log ?step:None) config.Config.jid users ;

  ignore (LTerm.save_state term);  (* save the terminal state *)

  Cli_client.loop ?out config term history state n (log ?step:None) >>= fun state ->

  ( match out with
    | None -> return_unit
    | Some fd -> Lwt_unix.close fd ) >>= fun () ->

  Lwt_mvar.put state.Cli_state.notify_mvar Cli_state.Quit >>= fun () ->

  Persistency.dump_users cfgdir state.Cli_state.users >>= fun () ->

  LTerm.load_state term   (* restore the terminal state *)



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
