open Lwt.Infix

let init_system ui_mvar =
  let open Cli_state in
  let err r m =
    Lwt.async (fun () ->
        Connect.disconnect () >>= fun () ->
        let handle s =
          add_status ~kind:`Error s (`Local ((`Full s.config.Xconfig.jid), "async error")) m ;
          if r then
            Lwt.return (`Failure s)
          else
            Lwt.return (`Disconnect s)
        in
        Lwt_mvar.put ui_mvar handle)
  in
  Lwt.async_exception_hook := (function
      | Tls_lwt.Tls_failure `Error (`AuthenticationFailure _) as exn ->
        err false (Printexc.to_string exn)
      | Unix.Unix_error (Unix.EBADF, _, _ ) as exn ->
        xmpp_session := None ; err false (Printexc.to_string exn) (* happens on /disconnect *)
      | Unix.Unix_error (Unix.EINVAL, "select", _ ) as exn ->
        xmpp_session := None ; err true (Printexc.to_string exn) (* not sure whether true should be false *)
      | exn -> err true (Printexc.to_string exn)
  )

let start_client cfgdir debug unicode fd_gui fd_nfy () =
  Sys.(set_signal sigpipe Signal_ignore) ;

  Printexc.register_printer (function
      | Tls_lwt.Tls_alert x -> Some ("TLS alert: " ^ Tls.Packet.alert_type_to_string x)
      | Tls_lwt.Tls_failure f -> Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
      | _ -> None) ;

  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);

  Utils.unicode := unicode ;

  let term = Notty_lwt.Term.create ~mouse:false () in

  let tc = Unix.(tcgetattr stdin) in
  Unix.(tcsetattr stdin TCSANOW { tc with c_isig = false ; c_ixon = false ; c_ixoff = false }) ;

  Persistency.load_dsa cfgdir >>= fun dsa ->

  Persistency.load_config dsa cfgdir >>= (function
      | None ->
        Cli_config.configure term () >>= fun config ->
        Persistency.dump_config cfgdir config >>= fun () ->
        (match config.Xconfig.password with
         | None -> Lwt.return_unit
         | Some x -> Persistency.dump_password cfgdir (Bytes.of_string x)) >>= fun () ->
        Persistency.dump_dsa cfgdir config.Xconfig.dsa >|= fun () ->
        config
      | Some cfg -> Lwt.return cfg) >>= fun config ->

  Cli_colour.init () ;
  (Persistency.load_colours cfgdir >|= function
    | None -> ()
    | Some colours -> Cli_colour.load colours) >>= fun () ->

  (match config.Xconfig.password with
   | None -> Persistency.load_password cfgdir
   | Some x -> Lwt.return (Some x)) >>= (function
      | None ->
        let jid = Xjid.full_jid_to_string config.Xconfig.jid in
        Cli_config.read_password ~prefix:("Password (" ^ jid ^ "): ") term >|= fun password ->
        Some password
      | Some x -> Lwt.return (Some x)) >>= fun password ->
  let config = { config with Xconfig.password = password } in
  let myjid = config.Xconfig.jid in

  (* loading old users.sexp *)
  Persistency.load_users cfgdir >>= fun users ->
  let users_sexp_existed = Contact.length users > 0 in

  Persistency.load_user_dir cfgdir users >>= fun () ->
  Persistency.load_histories cfgdir users >>= fun () ->

  (* setup self contact *)
  let () =
    let bare, resource = myjid in
    let user = match Contact.find_user users bare with
      | None -> User.new_user ~jid:bare ()
      | Some u -> u
    in
    let user = { user with User.self = true } in
    let u, _session = User.create_session user resource config.Xconfig.otr_config config.Xconfig.dsa in
    let u = { u with User.readline_history = [ "/join jackline@conference.jabber.ccc.de" ; "/connect" ] } in
    Contact.replace_user users u
  in

  (match debug with
   | None -> Lwt.return (fun () -> Lwt.return_unit)
   | Some filename ->
     let file = if String.equal filename "true" then "out.txt" else filename in
     (* I used to use getenv "PWD", but this breaks on (at least) ksh *)
     let dir, file = Filename.(dirname file, basename file) in
     Persistency.open_append dir file >|= fun fd ->
     Xmpp_connection.debug_out := Some fd ;
     (fun () -> Lwt_unix.close fd)) >>= fun closing ->

  let ui_mvar = Lwt_mvar.create_empty () in

  Cli_state.Notify.notify_writer myjid config.Xconfig.notification_callback fd_nfy >>= fun state_mvar ->
  Cli_state.Notify.gui_focus_reader fd_gui ui_mvar ;
  let he = Happy_eyeballs_lwt.create () in
  let connect_mvar = Cli_state.Connect.connect_me he config ui_mvar state_mvar users in
  let state = Cli_state.empty_state cfgdir config users connect_mvar state_mvar in

  let greetings = [
    "welcome to jackline " ^ Utils.version ;
    "type /help for command list" ;
    "* configurable colours!  Put '((Presence \"cyan\") (Success \"lightgreen\"))' in your " ^ cfgdir ^ "/colours.sexp (see https://github.com/hannesm/jackline#colours for details)" ;
    "* improved MUC support: join&leave messages; autojoin on (re)connect; /rooms discovery" ]
  and sender = `Local (`Full myjid,"")
  in
  List.iter (Cli_state.add_status ~kind:`Info state sender) greetings ;

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

  init_system ui_mvar ;

  let input_mvar = Lwt_mvar.create_empty () in
  Lwt.async (Cli_input.read_terminal term ui_mvar input_mvar) ;
  (* main loop *)
  let size = Notty_lwt.Term.size term in
  Cli_client.loop term size Lwt_engine.fake_event ui_mvar input_mvar state >>= fun state ->

  closing () >>= fun () ->

  Lwt_mvar.put state.Cli_state.state_mvar Cli_state.Quit >>= fun () ->

  (* cancel history dumper *)
  Lwt_engine.stop_event hist_dumper ;
  (* store histories a last time *)
  Persistency.dump_histories cfgdir users

let config_dir = ref ""
let debug = ref None
let fd_gui = ref None
let fd_nfy = ref None
let ascii = ref false
let rest = ref []

let _ =
  let home = Unix.getenv "HOME" in
  let cfgdir = Filename.concat home ".config" in
  config_dir := Filename.concat cfgdir "ocaml-xmpp-client"

let _ =
  let utf s = Astring.String.is_infix ~affix:"UTF" (Astring.String.Ascii.uppercase s) in
  let rec tst = function
    | [] -> false
    | x::xs -> try utf (Unix.getenv x) with Not_found -> tst xs
  in
  if not (tst [ "LC_ALL" ; "LC_CTYPE" ; "LANG" ]) then
    ascii := true

let usage = "usage " ^ Sys.argv.(0)

let arglist = [
  ("-f", Arg.String (fun d -> config_dir := d), "configuration directory (defaults to ~/.config/ocaml-xmpp-client/)") ;
  ("-d", Arg.String (fun d -> debug := Some d), "debug log (either filename or out.txt)") ;
  ("-a", Arg.Bool (fun a -> ascii := a), "ASCII only output") ;
  ("--fd-gui", Arg.String (fun fd -> fd_gui := Some fd), "File descriptor to receive GUI focus updates on.") ;
  ("--fd-nfy", Arg.String (fun fd -> fd_nfy := Some fd), "File descriptor to send notification updates on.")
]

let _ =
  try
    Arg.parse arglist (fun x -> rest := x :: !rest) usage ;
    Lwt_main.run (start_client !config_dir !debug (not !ascii) !fd_gui !fd_nfy ())
  with
  | Sys_error s -> print_endline s
