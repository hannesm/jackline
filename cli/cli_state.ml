open Lwt.Infix

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
  | Notification of bool (* if false, then this and all other notifications were immediately cleared *)
  | Clear
  | Quit

type connect_v =
  | Cancel
  | Connect of Xmpp_callbacks.user_data
  | Success of Xmpp_callbacks.user_data
  | Reconnect
  | Presence of (User.presence * string option * int option)

type input = Uchar.t list * Uchar.t list

type state = {
  (* set only initially *)
  config_directory : string ;
  config : Xconfig.t ;

  state_mvar : notify_v Lwt_mvar.t ;
  contact_mvar : Contact.contact Lwt_mvar.t ;
  connect_mvar : connect_v Lwt_mvar.t ;

  (* initially filled by on-disk-data, modified by xmpp callbacks *)
  contacts : Contact.contacts ;

  (* list of jid to blink *)
  notifications : Xjid.t list ;
  gui_has_focus : bool ;

  (* initially yourself, modified by key presses (page up/page down/C-x/C-q) *)
  active_contact : Xjid.t ;
  last_active_contact : Xjid.t ;

  (* UI toggles, modified by keys *)
  show_offline : bool ;
  window_mode : display_mode ;
  scrollback : int ;
  log_height : int ;
  buddy_width : int ;
  filter : string option ;

  (* current input buffer *)
  input : input ;
  (* kill ring *)
  kill : Uchar.t list ;
}

let add_status ?timestamp ?kind state dir msg =
  match Contact.find_user state.contacts (fst state.config.Xconfig.jid) with
  | None -> assert false
  | Some self ->
     let self = User.insert_message ?timestamp ?kind self dir false true msg in
     Contact.replace_user state.contacts self

let active state =
  match Contact.find_contact state.contacts (Xjid.t_to_bare state.active_contact) with
  | None -> assert false
  | Some x -> x

let isactive state jid =
  let bare = Xjid.t_to_bare state.active_contact in
  match active state with
  | `User u when not u.User.expand -> Xjid.jid_matches (`Bare bare) jid
  | _ -> jid = state.active_contact

let has_any_notifications state = List.length state.notifications > 0

let maybe_clear state =
  let newstate =
    if not state.gui_has_focus then
      state
    else
      let notifications = List.filter (fun x -> not (isactive state x)) state.notifications in
      { state with notifications }
  in
  if not (has_any_notifications newstate) && has_any_notifications state then
    (* latter test is so that we don't notify every time the user changes contact *)
    Lwt.async (fun () -> Lwt_mvar.put newstate.state_mvar Clear) ;
  newstate

let selflog mvar from message =
  let c s =
    add_status ~kind:`Info s (`Local ((`Full s.config.Xconfig.jid), from)) message ;
    Lwt.return (`Ok s)
  in
  Lwt_mvar.put mvar c

module Notify = struct
  type notify_state = Q | D | C | D_N | C_N

  let to_string s v =
    let ss = match s with
      | Q -> "quit"
      | D -> "disconnected"
      | C -> "connected"
      | D_N -> "disconnected_notifications"
      | C_N -> "connected_notifications"
    and vs = match v with
      | Connected -> "connect"
      | Disconnected -> "disconnect"
      | Notification true -> "notify_contact"
      | Notification false -> "notify_contact clear_all_notifications"
      | Clear -> "clear_all_notifications"
      | Quit -> "quit"
    in
    ss ^ " " ^ vs

  let to_gui_focus = function
    | "gui_focus true" -> true
    | "gui_focus false" -> false
    | _ -> true

  let flag_of_mode (type a) (mode : a Lwt_io.mode) = match mode with
    | Lwt_io.Input -> Unix.O_RDONLY
    | Lwt_io.Output -> Unix.O_WRONLY

  let channel fd mode =
    Lwt_unix.openfile fd [flag_of_mode mode] 0 >|=
    Lwt_io.of_fd ~mode:mode

  (* TODO(infinity0): perhaps log the exception somewhere *)
  let catch_exn f = Lwt.catch f (fun _ -> Lwt.return_unit)

  let gui_focus_reader fdo ui_mvar =
    match fdo with
    | None -> ()
    | Some fd ->
      let doit () =
        channel fd Lwt_io.input >>= fun chan ->
        let rec loop () =
          Lwt_io.read_line chan >>= fun line ->
          let cb state =
            let gui_has_focus = to_gui_focus line in
            Lwt.return (`Ok (maybe_clear { state with gui_has_focus }))
          in
          Lwt_mvar.put ui_mvar cb >>=
          loop
        in
        catch_exn loop
      in
      Lwt.async doit

  let maybe_catch_and_return opt f =
    Utils.option Lwt.return_unit (fun s -> catch_exn (fun () -> f s)) opt

  let notify_writer jid cb fdo =
    let mvar = Lwt_mvar.create Disconnected in
    (Utils.option
       (Lwt.return None)
       (fun fd -> channel fd Lwt_io.output >|= fun ch -> Some ch)
       fdo) >|= fun chan_opt ->
    let notify_call buf =
      let args = " " ^ Xjid.full_jid_to_string jid ^ " " ^ buf in
      maybe_catch_and_return cb
        (fun x -> Lwt_unix.system (x ^ args) >>= fun _ -> Lwt.return_unit) >>= fun () ->
      maybe_catch_and_return chan_opt
        (fun chan -> Lwt_io.write_line chan buf)
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
        | Notification true, D -> D_N
        | Notification false, D_N -> D
        | Notification true, C -> C_N
        | Notification false, C_N -> C
        | Clear, C_N -> C
        | Clear, D_N -> D
        | _, _ -> s0
      in
      notify_call (to_string s1 v) >>= fun () ->
      if s1 = Q then
        Lwt.return_unit
      else
        loop s1
    in
    Lwt.async (fun () -> loop C) ;
    mvar
end

let (reading : bool ref) = ref true

let (xmpp_session : Xmpp_callbacks.user_data Xmpp_callbacks.XMPPClient.session_data option ref) = ref None

let (connecting : bool ref) = ref false

module Connect = struct
  let disconnect () =
    Xmpp_callbacks.Keepalive.cancel_keepalive () ;
    Xmpp_callbacks.Keepalive.keepalive_running := false ;
    match !xmpp_session with
    | Some s ->
       xmpp_session := None ;
       Lwt.catch
         (fun () -> Xmpp_callbacks.close s)
         (fun _ -> Lwt.return_unit)
    | None   ->
       Lwt.return_unit

  let resolve config ui_mvar =
    let domain = JID.to_idn (Xjid.jid_to_xmpp_jid (`Full config.Xconfig.jid))
    and hostname = config.Xconfig.hostname
    and port = config.Xconfig.port
    in
    let report sockaddr =
      let addr = match sockaddr with
        | Unix.ADDR_INET (inet_addr, port) ->
           Unix.string_of_inet_addr inet_addr ^ " on port " ^ string_of_int port
        | Unix.ADDR_UNIX str -> str
      and host = match hostname with
        | Some x -> x
        | None -> domain
      in
      Lwt.async (fun () -> selflog ui_mvar "connecting" ("to " ^ host ^ " (" ^ addr ^ ")"))
    in
    Lwt.async (fun () ->
        let out = match hostname with None -> domain | Some x -> x in
        selflog ui_mvar "resolving" out) ;
    Xmpp_callbacks.resolve hostname port domain >|= fun sa ->
    report sa ;
    sa

  let connect_me config ui_mvar state_mvar users =
    let mvar = Lwt_mvar.create Cancel in
    let failure reason =
      disconnect () >>= fun () ->
      Lwt.async (fun () -> selflog ui_mvar "session error" (Printexc.to_string reason)) ;
      let conn () = Lwt_mvar.put mvar Reconnect in
      ignore (Lwt_engine.on_timer 10. false (fun _ -> Lwt.async conn)) ;
      Lwt.return_unit
    in
    let connect user_data (p, s, prio) =
      match config.Xconfig.password with
      | None -> failure (Invalid_argument "no password provided, please restart")
      | Some password ->
        Lwt.catch (fun () ->
          resolve config ui_mvar >>= fun sockaddr ->
          let certname = match config.Xconfig.certificate_hostname with
            | None -> JID.to_idn (Xjid.jid_to_xmpp_jid (`Full config.Xconfig.jid))
            | Some x -> x
          in
          (match config.Xconfig.authenticator with
           | `Trust_anchor x -> X509_lwt.authenticator (`Ca_file x)
           | `Fingerprint fp ->
             let time = Ptime_clock.now () in
             let fp =
               Nocrypto.Uncommon.Cs.of_hex
                 (String.map (function ':' -> ' ' | x -> x) fp)
             in
             let fingerprints = [(certname, fp)]
             and hash = `SHA256
             in
             let auth = X509.Authenticator.server_cert_fingerprint ~time ~hash ~fingerprints in
             Lwt.return auth) >>= fun authenticator ->
          let kind, show = Xmpp_callbacks.presence_to_xmpp p in
          let socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
          Lwt.catch (fun () ->
            Lwt_unix.connect socket sockaddr >>= fun () ->
            Xmpp_callbacks.connect
              socket
              config.Xconfig.jid certname password
              (kind, show, s, prio) authenticator user_data
              (fun session ->
                 Lwt_mvar.put mvar (Success user_data) >>= fun () ->
                 let auto_rooms =
                   Contact.fold
                     (fun _ v acc ->
                        match v with
                        | `Room r when r.Muc.autojoin -> r :: acc
                        | _ -> acc)
                     users []
                 in
                 Lwt_list.iter_s (fun r ->
                     let nick = r.Muc.my_nick
                     and jid = Xjid.jid_to_xmpp_jid (`Bare r.Muc.room_jid)
                     and password = r.Muc.password
                     in
                     Xmpp_callbacks.Xep_muc.enter_room session ?password ~nick jid) auto_rooms) >|= fun session ->
            xmpp_session := Some session ;
            Lwt.async (fun () -> Xmpp_callbacks.parse_loop session))
            (fun exn -> Lwt_unix.close socket >>= fun () -> failure exn))
          (fun exn -> failure exn)
    in
    let rec reconnect_loop user_data presence =
      Lwt_mvar.take mvar >>= function
        | Cancel -> reconnect_loop None presence
        | Connect user_data ->
           connecting := true ;
           connect user_data presence >>= fun () ->
           connecting := false ;
           reconnect_loop None presence
        | Success user_data ->
           Lwt_mvar.put state_mvar Connected >>= fun () ->
           reconnect_loop (Some user_data) presence
        | Presence p ->
           reconnect_loop user_data p
        | Reconnect ->
           match !xmpp_session, user_data with
           | None, Some u ->
              connecting := true ;
              connect u presence >>= fun () ->
              connecting := false ;
              reconnect_loop (Some u) presence
           | _, u -> reconnect_loop u presence
    in
    Lwt.async (fun () -> reconnect_loop None (`Online, None, config.Xconfig.priority)) ;
    mvar
end

let empty_state config_directory config contacts connect_mvar state_mvar =
  let contact_mvar = Persistency.notify_user config_directory
  and active = `Bare (fst config.Xconfig.jid)
  in
  {
    config_directory                ;
    config                          ;

    state_mvar                      ;
    contact_mvar                    ;
    connect_mvar                    ;

    contacts                        ;

    active_contact      = active    ;
    last_active_contact = active    ;

    notifications       = []        ;
    gui_has_focus       = true      ;

    show_offline        = true      ;
    window_mode         = BuddyList ;
    scrollback          = 0         ;

    log_height          = 6         ;
    buddy_width         = 24        ;
    filter              = None      ;
    input               = ([], [])  ;
    kill                = []
}


let send s session ?kind jid id body =
  let x, req =
    match kind with
    | Some Xmpp_callbacks.XMPPClient.Groupchat -> (false, false)
    | _ ->
      Utils.option
        (false, false)
        (fun s -> match s.User.receipt with
           | `Unknown -> (false, true)
           | `Supported -> (true, false)
           | _ -> (false, false))
        session
  in
  Xmpp_callbacks.send_msg s ?kind jid x id body >>= fun () ->
  if req then
    Xmpp_callbacks.request_disco s jid
  else
    Lwt.return_unit


let random_string () =
  let open Nocrypto in
  let rnd = Rng.generate 12 in
  Cstruct.to_string (Base64.encode rnd)

let notify state jid =
  let newstate =
    if
      List.exists (Xjid.jid_matches jid) state.notifications ||
      (state.gui_has_focus && Xjid.jid_matches state.active_contact jid && state.scrollback = 0)
    then
      (* no-op <=> already has notification || has focus from {parent gui, term window, scrolling} *)
      state
    else
      { state with notifications = jid :: state.notifications } in
  Lwt.async (fun () -> Lwt_mvar.put newstate.state_mvar (Notification (has_any_notifications newstate))) ;
  newstate

let has_notifications state jid =
  List.exists (Xjid.jid_matches jid) state.notifications

let active_contacts state =
  let active jid =
    let id = `Bare jid in
    state.show_offline || Xjid.jid_matches id state.active_contact || has_notifications state id
  and online contact =
    Contact.active_presence contact <> `Offline
  and self = function
    | `User u when u.User.self -> true
    | _ -> false
  in
  let contacts =
    Contact.fold
      (fun id c acc -> if active id || online c || self c then c :: acc else acc)
      state.contacts []
  in
  List.sort Contact.compare_contact contacts

let active_resources state contact =
  if state.show_offline then
    Contact.all_resources contact
  else
    let tst jid = has_notifications state jid || state.active_contact = jid in
    Contact.active_resources tst contact

let potentially_visible_resource state contact =
  match contact, List.length (active_resources state contact) with
  | `Room _, _ -> true
  | `User _, x when x < 2 -> false
  | `User _, _ -> true

let visible_resources state contact =
  if Contact.expanded contact then
    active_resources state contact
  else
    []

let active_contacts_resources state =
  let contacts = active_contacts state in
  List.combine contacts (List.map (visible_resources state) contacts)

let show_resource (contact, res) =
  let bare = Contact.jid contact None in
  bare :: List.map (fun x -> `Full x) (List.map (Contact.full_jid contact) res)

let show_resources rs = List.fold_right (fun rs acc -> (show_resource rs) @ acc) rs []

let all_jids state = show_resources (active_contacts_resources state)

let otr_config user state =
  match user.User.otr_custom_config with
  | None -> state.config.Xconfig.otr_config
  | Some x -> x

let session state =
  match active state with
  | `Room _ -> None
  | `User user -> match state.active_contact with
                  | `Bare _ when user.User.expand -> None
                  | `Bare _ -> User.active_session user
                  | `Full (_, r) -> User.find_session user r

let resource state = match active state with
  | `Room r -> Utils.option None (fun m -> Some (`Member m)) (Muc.member r state.active_contact)
  | `User _ -> Utils.option None (fun s -> Some (`Session s))  (session state)

let self state =
  match Contact.find_user state.contacts (fst state.config.Xconfig.jid) with
  | None -> assert false
  | Some self -> self

let selfsession state =
  match User.find_session (self state) (snd state.config.Xconfig.jid) with
  | None -> assert false
  | Some s -> s

let activate_contact state active =
  let find x = Contact.find_contact state.contacts (Xjid.t_to_bare x)
  and r_jid c id = if Contact.expanded c then id else `Bare (Contact.bare c)
  and update old ojid now njid =
    Contact.replace_contact state.contacts (Contact.set_input_buffer old state.input) ;
    let state =
      { state with
        last_active_contact = ojid ;
        active_contact      = njid ;
        scrollback          = 0 ;
        filter              = None ;
        window_mode         = BuddyList ;
        input               = Contact.input_buffer now }
    in
    maybe_clear state
  in
  match find state.active_contact, find active with
  | Some cur, Some now ->
     let ojid = r_jid cur state.active_contact
     and njid = r_jid now active
     in
     if ojid <> njid then update cur state.active_contact now njid else state
  | None, Some now ->
     let old, ojid = match find state.last_active_contact with
       | Some c -> (c, r_jid c state.last_active_contact)
       | None -> (`User (self state), `Bare (fst state.config.Xconfig.jid))
     and njid = r_jid now active
     in
     update old ojid now njid
  | _, None -> assert false

let update_notifications state user oldr newr =
  let bare = user.User.bare_jid in
  let update = function
    | `Bare _ as x -> x
    | `Full (jid, r) when bare = jid && r = oldr -> `Full (jid, newr)
    | `Full _ as x -> x
  in
  let notifications = List.map update state.notifications in
  { state with notifications }
