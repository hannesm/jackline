open Lwt
open React

let start_client cfgdir debug () =
  Sys.(set_signal sigpipe Signal_ignore) ;
  ignore (LTerm_inputrc.load ());

  Printexc.register_printer (function
      | Tls_lwt.Tls_alert x -> Some ("TLS alert: " ^ Tls.Packet.alert_type_to_string x)
      | Tls_lwt.Tls_failure f -> Some ("TLS failure: " ^
        begin match f with
        | `Error (`AuthenticationFailure (`InvalidFingerprint cert)) ->
            "AuthenticationFailure: InvalidFingerprint: " ^
            begin match
              Hex.of_cstruct (X509.fingerprint cert `SHA256)
            with `Hex s -> s end
        | _ ->
            Tls.Engine.string_of_failure f
        end )
      | _ -> None) ;

  Nocrypto_entropy_lwt.initialize () >>= fun () ->

  Lazy.force LTerm.stdout >>= fun term ->

  Persistency.load_dsa cfgdir >>= fun dsa ->

  Persistency.load_config dsa cfgdir >>= ( function
      | None ->
        Cli_config.configure term () >>= fun config ->
        Persistency.dump_config cfgdir config >>= fun () ->
        (match config.Xconfig.password with
         | None -> return_unit
         | Some x -> Persistency.dump_password cfgdir x) >>= fun () ->
        Persistency.dump_dsa cfgdir config.Xconfig.dsa >|= fun () ->
        config
      | Some cfg -> return cfg ) >>= fun config ->

  ( match config.Xconfig.password with
    | None -> Persistency.load_password cfgdir
    | Some x -> return (Some x)) >>= fun password ->
  let config = { config with Xconfig.password = password } in

  ( match config.Xconfig.password with
    | None ->
       let jid = Xjid.full_jid_to_string config.Xconfig.jid in
       (new Cli_config.read_password term ~prompt:("password for " ^ jid ^ ": "))#run >|= fun password ->
       Some password
    | Some x -> return (Some x)) >>= fun password ->
  let config = { config with Xconfig.password = password } in

  Persistency.load_users cfgdir >>= fun users ->
  let users_sexp_existed = Contact.length users > 0 in

  Persistency.load_user_dir cfgdir users >>= fun () ->
  Persistency.load_histories cfgdir users >>= fun () ->

  (* setup self contact *)
  let myjid = config.Xconfig.jid in
  let _ =
    let bare, resource = myjid in
    let user = match Contact.find_user users bare with
      | None -> User.new_user ~jid:bare ()
      | Some u -> u
    in
    let u, _ = User.create_session user resource config.Xconfig.otr_config config.Xconfig.dsa in
    Contact.replace_user users u
  in

  let greeting =
    "multi user chat support: see you at /join test@jabber.ietf.org; \
     type /help for help"
  in

  let n, log = S.create (`Local (`Full myjid, "welcome to jackline " ^ Utils.version), greeting)
  in

  (if debug then
     Persistency.open_append (Unix.getenv "PWD") "out.txt" >|= fun fd ->
     Some fd
   else
     return None) >>= fun out ->

  let state_mvar =
    let file = Filename.concat cfgdir "notification.state" in
    Cli_state.Notify.notify_writer myjid config.Xconfig.notification_callback file
  in
  let connect_mvar = Cli_state.Connect.connect_me config (log ?step:None) out state_mvar users in
  let state = Cli_state.empty_state cfgdir config users connect_mvar state_mvar in

  let us = Contact.fold (fun _ v acc -> v :: acc) users [] in

  (if users_sexp_existed then
     (* write out all the users to users/ *)
     Lwt_list.iter_s (Lwt_mvar.put state.Cli_state.contact_mvar) us >>= fun () ->
     (* delete users.sexp *)
     Persistency.delete (Filename.concat cfgdir Persistency.users)
   else
     Lwt.return_unit) >>= fun () ->

  (* dump histories every 10 minutes *)
  (* NOTE: this might be racy if dump_histories does not finish within
           600 seconds, should ensure that only one dumper is running
           at a given time *)
  let hist_dumper =
    let dump () = Persistency.dump_histories cfgdir users in
    Lwt_engine.on_timer 600. true (fun _ -> Lwt.async dump)
  in

  Cli_client.init_system (log ?step:None) myjid connect_mvar ;

  ignore (LTerm.save_state term);  (* save the terminal state *)

  (* main loop *)
  Cli_client.loop term state n (log ?step:None) >>= fun state ->

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
