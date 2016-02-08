open Lwt.Infix

(*
  mutable c_ignbrk : bool; 	(* Ignore the break condition. *)
  mutable c_brkint : bool; 	(* Signal interrupt on break condition. *)
  mutable c_ignpar : bool; 	(* Ignore characters with parity errors. *)
  mutable c_parmrk : bool; 	(* Mark parity errors. *)
  mutable c_inpck : bool; 	(* Enable parity check on input. *)
  mutable c_istrip : bool; 	(* Strip 8th bit on input characters. *)
  mutable c_inlcr : bool; 	(* Map NL to CR on input. *)
  mutable c_igncr : bool; 	(* Ignore CR on input. *)
T  mutable c_icrnl : bool; 	(* Map CR to NL on input. *)
T  mutable c_ixon : bool; 	(* Recognize XON/XOFF characters on input. *)
  mutable c_ixoff : bool; 	(* Emit XON/XOFF chars to control input flow. *)
T  mutable c_opost : bool; 	(* Enable output processing. *)
  mutable c_obaud : int; 	(* Output baud rate (0 means close connection). *)
  mutable c_ibaud : int; 	(* Input baud rate. *)
  mutable c_csize : int; 	(* Number of bits per character (5-8). *)
  mutable c_cstopb : int; 	(* Number of stop bits (1-2). *)
T  mutable c_cread : bool; 	(* Reception is enabled. *)
T  mutable c_parenb : bool; 	(* Enable parity generation and detection. *)
  mutable c_parodd : bool; 	(* Specify odd parity instead of even. *)
T  mutable c_hupcl : bool; 	(* Hang up on last close. *)
  mutable c_clocal : bool; 	(* Ignore modem status lines. *)
T  mutable c_isig : bool; 	(* Generate signal on INTR, QUIT, SUSP. *)
T  mutable c_icanon : bool; 	(* Enable canonical processing (line buffering and editing) *)
  mutable c_noflsh : bool; 	(* Disable flush after INTR, QUIT, SUSP. *)
T  mutable c_echo : bool; 	(* Echo input characters. *)
T  mutable c_echoe : bool; 	(* Echo ERASE (to erase previous character). *)
T  mutable c_echok : bool; 	(* Echo KILL (to erase the current line). *)
  mutable c_echonl : bool; 	(* Echo NL even if c_echo is not set. *)
\003  mutable c_vintr : char; 	(* Interrupt character (usually ctrl-C). *)
\028  mutable c_vquit : char; 	(* Quit character (usually ctrl-\). *)
\b  mutable c_verase : char; 	(* Erase character (usually DEL or ctrl-H). *)
\021  mutable c_vkill : char; 	(* Kill line character (usually ctrl-U). *)
\004  mutable c_veof : char; 	(* End-of-file character (usually ctrl-D). *)
\255  mutable c_veol : char; 	(* Alternate end-of-line char. (usually none). *)
1  mutable c_vmin : int; 	(* Minimum number of characters to read before the read request is satisfied. *)
0  mutable c_vtime : int; 	(* Maximum read wait (in 0.1s units). *)
\017  mutable c_vstart : char; 	(* Start character (usually ctrl-Q). *)
\019  mutable c_vstop : char; 	(* Stop character (usually ctrl-S). *)
*)

let init_system ui_mvar =
  let open Cli_state in
  let err r m =
    Lwt.async (fun () ->
        Connect.disconnect () >>= fun () ->
        let handle s =
          add_status s (`Local ((`Full s.config.Xconfig.jid), "async error")) m ;
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

let start_client cfgdir debug unicode () =
  Sys.(set_signal sigpipe Signal_ignore) ;

  Printexc.register_printer (function
      | Tls_lwt.Tls_alert x -> Some ("TLS alert: " ^ Tls.Packet.alert_type_to_string x)
      | Tls_lwt.Tls_failure f -> Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
      | _ -> None) ;

  Nocrypto_entropy_lwt.initialize () >>= fun () ->

  Utils.unicode := unicode ;

  Lwt_unix.tcgetattr Lwt_unix.stdin >>= fun saved_tc ->
  Lwt_unix.(tcsetattr stdin TCSANOW { saved_tc with c_ignbrk = true ; c_isig = false ; c_icanon = false ; c_echo = false ; c_vstart = '\255' ; c_vstop = '\255' ; c_verase = '\255' }) >>= fun () ->
  let term = Notty_lwt.Term.create ~input:Lwt_unix.stdin ~mouse:false () in

  Persistency.load_dsa cfgdir >>= fun dsa ->

  Persistency.load_config dsa cfgdir >>= ( function
      | None ->
        Cli_config.configure term () >>= fun config ->
        Persistency.dump_config cfgdir config >>= fun () ->
        (match config.Xconfig.password with
         | None -> Lwt.return_unit
         | Some x -> Persistency.dump_password cfgdir x) >>= fun () ->
        Persistency.dump_dsa cfgdir config.Xconfig.dsa >|= fun () ->
        config
      | Some cfg -> Lwt.return cfg ) >>= fun config ->

  ( match config.Xconfig.password with
    | None -> Persistency.load_password cfgdir
    | Some x -> Lwt.return (Some x)) >>= fun password ->
  let config = { config with Xconfig.password = password } in

  ( match config.Xconfig.password with
    | None ->
       let jid = Xjid.full_jid_to_string config.Xconfig.jid in
       Cli_config.read_password ~prefix:("Password (" ^ jid ^ "): ") term >|= fun password ->
       Some password
    | Some x -> Lwt.return (Some x)) >>= fun password ->
  let config = { config with Xconfig.password = password } in

  Persistency.load_users cfgdir >>= fun users ->
  let users_sexp_existed = Contact.length users > 0 in

  Persistency.load_user_dir cfgdir users >>= fun () ->
  Persistency.load_histories cfgdir users >>= fun () ->

  (* setup self contact *)
  let myjid = config.Xconfig.jid in
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

  (if debug then
     Persistency.open_append (Unix.getenv "PWD") "out.txt" >|= fun fd ->
     Xmpp_connection.debug_out := Some fd ;
     (fun () -> Lwt_unix.close fd)
   else
     Lwt.return (fun () -> Lwt.return_unit)) >>= fun closing ->

  let ui_mvar = Lwt_mvar.create_empty () in

  let state_mvar =
    let file = Filename.concat cfgdir "notification.state" in
    Cli_state.Notify.notify_writer myjid config.Xconfig.notification_callback file
  in
  let connect_mvar = Cli_state.Connect.connect_me config ui_mvar state_mvar users in
  let state = Cli_state.empty_state cfgdir config users connect_mvar state_mvar in

  let greeting =
    "multi user chat support: see you at /join jackline@conference.jabber.ccc.de (use ArrowUp key); \
     type /help for help"
  and sender = `Local (`Full myjid, "welcome to jackline " ^ Utils.version) in
  Cli_state.add_status state sender greeting ;

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
  Cli_client.loop term ui_mvar input_mvar state >>= fun state ->

  closing () >>= fun () ->

  Lwt_mvar.put state.Cli_state.state_mvar Cli_state.Quit >>= fun () ->

  (* cancel history dumper *)
  Lwt_engine.stop_event hist_dumper ;
  (* store histories a last time *)
  Persistency.dump_histories cfgdir users >>= fun () ->
  Lwt_unix.(tcsetattr stdin TCSANOW saved_tc)


let config_dir = ref ""
let debug = ref false
let rest = ref []
let ascii = ref false

let _ =
  let home = Unix.getenv "HOME" in
  let cfgdir = Filename.concat home ".config" in
  config_dir := Filename.concat cfgdir "ocaml-xmpp-client"

let _ =
  let utf s = Astring.String.is_infix ~affix:"UTF" (String.uppercase s) in
  let rec tst = function
    | [] -> false
    | x::xs -> try utf (Unix.getenv x) with Not_found -> tst xs
  in
  if not (tst [ "LC_ALL" ; "LC_CTYPE" ; "LANG" ]) then
    ascii := true

let usage = "usage " ^ Sys.argv.(0)

let arglist = [
  ("-f", Arg.String (fun d -> config_dir := d), "configuration directory (defaults to ~/.config/ocaml-xmpp-client/)") ;
  ("-d", Arg.Bool (fun d -> debug := d), "log to out.txt in current working directory") ;
  ("-a", Arg.Bool (fun a -> ascii := a), "ASCII only output")
]

let _ =
  try
    Arg.parse arglist (fun x -> rest := x :: !rest) usage ;
    Lwt_main.run (start_client !config_dir !debug (not !ascii) ())
  with
  | Sys_error s -> print_endline s
