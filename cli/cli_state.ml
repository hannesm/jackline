
type display_mode =
  | BuddyList
  | FullScreen
  | Raw

let next_display_mode = function
  | BuddyList  -> FullScreen
  | FullScreen -> Raw
  | Raw        -> BuddyList

type notify_v =
  | Disconnected
  | Connected
  | Notifications
  | Clear
  | Quit

type connect_v =
  | Cancel
  | Connect of Xmpp_callbacks.user_data
  | Success of Xmpp_callbacks.user_data
  | Reconnect

type state = {
  config_directory            : string                    ; (* set initially *)
  config                      : Config.t                  ; (* set initially *)

  state_mvar                  : notify_v Lwt_mvar.t       ; (* set initially *)
  user_mvar                   : User.user Lwt_mvar.t      ; (* set initially *)
  connect_mvar                : connect_v Lwt_mvar.t      ; (* set initially *)

  users                       : User.users                ; (* read from disk, extended by xmpp callbacks *)

  mutable active_contact      : User.Jid.t                ; (* modified by scrolling *)
  mutable last_active_contact : User.Jid.t                ; (* modified by scrolling *)

  mutable notifications       : User.Jid.t list           ; (* list to blink *)

  mutable show_offline        : bool                      ; (* F5 stuff *)
  mutable window_mode         : display_mode              ; (* F12 stuff *)
  mutable scrollback          : int                       ; (* scroll-pgup/down state *)

  mutable last_status         : (User.direction * string) ; (* internal use only *)

  mutable log_height          : int                       ;
  mutable buddy_width         : int                       ;
}

module Notify = struct
  type notify_writer_s = Q | D | C | D_N | C_N

  let to_string = function
    | Q -> "quit"
    | D -> "disconnected"
    | C -> "connected"
    | D_N -> "disconnected_notifications"
    | C_N -> "connected_notifications"

  let notify_writer jid cb fname =
    let open Lwt.Infix in
    let mvar = Lwt_mvar.create Disconnected in
    let write_file buf =
      let open Lwt_unix in
      Lwt.catch (fun () ->
          openfile fname [O_WRONLY ; O_TRUNC ; O_CREAT ] 0o600 >>= fun fd ->
          Persistency.write_data fd buf >>= fun () ->
          close fd)
        (fun _ -> Lwt.return_unit) >>= fun () ->
      match cb with
      | None -> Lwt.return_unit
      | Some x ->
        Lwt.catch (fun () ->
            system (x ^ " " ^ User.Jid.full_jid_to_string jid ^ " " ^ buf) >>= fun _ ->
            Lwt.return_unit)
          (fun _ -> Lwt.return_unit)
    in
    let rec loop s0 =
      Lwt_mvar.take mvar >>= fun v ->
      let s1 =
        match v, s0 with
        | Quit, _ -> Q
        | Disconnected, C -> D
        | Disconnected, C_N -> D_N
        | Connected, D -> C
        | Connected, D_N -> C_N
        | Notifications, D -> D_N
        | Notifications, C -> C_N
        | Clear, C_N -> C
        | Clear, D_N -> D
        | _, _ -> s0
      in
      match s1 with
      | Q -> write_file (to_string Q)
      | s when s == s0 -> loop s
      | _ -> write_file (to_string s1) >>= fun () -> loop s1
    in
    Lwt.async (fun () -> loop C) ;
    mvar
end

let (xmpp_session : Xmpp_callbacks.user_data Xmpp_callbacks.XMPPClient.session_data option ref) = ref None

module Connect = struct
  open Lwt.Infix

  let disconnect () =
    Xmpp_callbacks.cancel_keepalive () ;
    Xmpp_callbacks.keepalive_running := false ;
    match !xmpp_session with
    | Some s ->
       Xmpp_callbacks.close s >|= fun () ->
       xmpp_session := None
    | None   ->
       Lwt.return_unit

  let resolve config log =
    let domain = JID.to_idn (User.Jid.jid_to_xmpp_jid (`Full config.Config.jid))
    and hostname = config.Config.hostname
    and port = config.Config.port
    in
    let report sockaddr =
      let addr = match sockaddr with
        | Unix.ADDR_INET (inet_addr, port) ->
           Unix.string_of_inet_addr inet_addr ^ " on port " ^ string_of_int port
        | Unix.ADDR_UNIX str -> str
      in
      log (`Local (`Full config.Config.jid, "connecting"), "to " ^ domain ^ " (" ^ addr ^ ")") ;
    in
    Xmpp_callbacks.resolve hostname port domain >|= fun sa ->
    report sa ;
    sa

  let connect_me config log out state_mvar =
    let mvar = Lwt_mvar.create Cancel in
    let failure reason =
      disconnect () >>= fun () ->
      log (`Local (`Full config.Config.jid, "session error"), Printexc.to_string reason) ;
      let conn = fun () -> Lwt_mvar.put mvar Reconnect in
      ignore (Lwt_engine.on_timer 10. false (fun _ -> Lwt.async conn)) ;
      Lwt.return_unit
    in
    let connect user_data =
      match config.Config.password with
      | None -> failure (Invalid_argument "no password provided, please restart")
      | Some password ->
         try_lwt
           (resolve config log >>= fun sockaddr ->
            let certname = match config.Config.certificate_hostname with
              | None -> JID.to_idn (User.Jid.jid_to_xmpp_jid (`Full config.Config.jid))
              | Some x -> x
            in
            (X509_lwt.authenticator
               (match config.Config.authenticator with
                | `Trust_anchor x -> `Ca_file x
                | `Fingerprint fp -> `Hex_fingerprints (`SHA256, [(certname, fp)]))) >>= fun authenticator ->
            Xmpp_callbacks.connect
              ?out sockaddr
              config.Config.jid certname password
              config.Config.priority authenticator user_data
              (fun () -> Lwt_mvar.put mvar (Success user_data)) ) >|= function
              | None -> ()
              | Some session ->
                 xmpp_session := Some session ;
                 Lwt.async (fun () -> Xmpp_callbacks.parse_loop session)
      with exn -> failure exn
    in
    let rec reconnect_loop user_data =
      Lwt_mvar.take mvar >>= function
        | Cancel -> reconnect_loop None
        | Connect user_data ->
           connect user_data >>= fun () ->
           reconnect_loop None
        | Success user_data ->
           Lwt_mvar.put state_mvar Connected >>= fun () ->
           reconnect_loop (Some user_data)
        | Reconnect ->
           match !xmpp_session, user_data with
           | None, Some u ->
              connect u >>= fun () ->
              reconnect_loop (Some u)
           | _, u -> reconnect_loop u
    in
    Lwt.async (fun () -> reconnect_loop None) ;
    mvar
end

let empty_state config_directory config users connect_mvar state_mvar =
  let user_mvar = Persistency.notify_user config_directory
  and last_status = (`Local (`Full config.Config.jid, ""), "")
  and active = `Full config.Config.jid
  in
  {
    config_directory                ;
    config                          ;

    state_mvar                      ;
    user_mvar                       ;
    connect_mvar                    ;

    users                           ;

    active_contact      = active    ;
    last_active_contact = active    ;

    notifications       = []        ;

    show_offline        = true      ;
    window_mode         = BuddyList ;
    scrollback          = 0         ;

    last_status                     ;

    log_height          = 6         ;
    buddy_width         = 24        ;
}


let add_status state dir msg =
  User.add_message state.users (`Full state.config.Config.jid) dir false true msg

let send s jid id body fail =
  Xmpp_callbacks.send_msg s jid id body fail

let random_string () =
  let open Nocrypto in
  let rnd = Rng.generate 12 in
  Cstruct.to_string (Base64.encode rnd)

let notify state jid =
  if List.exists (fun x -> User.Jid.jid_matches x jid) state.notifications ||
       (User.Jid.jid_matches state.active_contact jid && state.scrollback = 0)
  then
    ()
  else
    state.notifications <- jid :: state.notifications ;
  Lwt.async (fun () -> Lwt_mvar.put state.state_mvar Notifications)

let notified state jid =
  state.notifications <- List.filter
                           (fun x -> not (User.Jid.jid_matches jid x))
                           state.notifications ;
  if List.length state.notifications = 0 then
    Lwt.async (fun () -> Lwt_mvar.put state.state_mvar Clear)

let otr_config user state =
  match user.User.otr_custom_config with
  | None -> state.config.Config.otr_config
  | Some x -> x
