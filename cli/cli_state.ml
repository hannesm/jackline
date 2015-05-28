
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

type ui_state = {
  config_directory            : string                    ; (* set initially *)
  user                        : string                    ; (* set initially *)
  resource                    : string                    ; (* set initially *)
  notify_mvar                 : notify_v Lwt_mvar.t       ; (* set initially *)

  users                       : User.users                ; (* read from disk, extended by xmpp callbacks *)

  mutable active_contact      : string                    ; (* modified by scrolling *)
  mutable last_active_contact : string                    ; (* modified by scrolling *)

  mutable notifications       : string list               ; (* list to blink *)

  mutable show_offline        : bool                      ; (* F5 stuff *)
  mutable window_mode         : display_mode              ; (* F12 stuff *)
  mutable scrollback          : int                       ; (* scroll-pgup/down state *)

  mutable last_status         : (User.direction * string) ; (* internal use only *)
}

type notify_writer_s = Q | D | C | D_N | C_N

let notify_writer fname =
  let open Lwt in
  let open Lwt_mvar in
  let mvar = create Disconnected in

  let write_file s =
    let open Lwt_unix in
    let open Lwt_io in
    try_lwt
      openfile fname [ O_WRONLY ; O_TRUNC ] 0o0 >>= fun fd ->
      let oc = of_fd ~mode:Output fd in
      write oc s >> flush oc >> close oc
    with Unix.Unix_error(Unix.ENOENT, _, _) -> return () in

  let to_string = function
    | Q -> "quit"
    | D -> "disconnected"
    | C -> "connected"
    | D_N -> "disconnected_notifications"
    | C_N -> "connected_notifications" in

  let rec loop s0 =
    take mvar >>= fun v ->
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
      | _, _ -> s0 in
    match s1 with
    | Q -> write_file (to_string X)
    | s when s == s0 -> loop s1
    | _ -> write_file (to_string s1) >> loop s1 in

  async (fun () -> loop C) ;
  mvar

let empty_ui_state config_directory user resource users =
  let fname = config_directory ^ Filename.dir_sep ^ "notification.state" in
  let mvar = notify_writer fname in
  let last_status = (`Local "", "") in
  {
    config_directory                ;
    user                            ;
    resource                        ;
    notify_mvar         = mvar      ;

    users                           ;

    active_contact      = user      ;
    last_active_contact = user      ;

    notifications       = []        ;

    show_offline        = true      ;
    window_mode         = BuddyList ;
    scrollback          = 0         ;

    last_status                     ;
}


let add_status state dir msg =
  let self = User.Users.find state.users state.user in
  let user = User.insert_message self dir false true msg in
  User.Users.replace state.users state.user user

let (xmpp_session : Xmpp_callbacks.user_data Xmpp_callbacks.XMPPClient.session_data option ref) = ref None

let send s contact session id body fail =
  let (>>=) = Lwt.(>>=) in
  Xmpp_callbacks.send_msg s contact session id body fail >>= fun () ->
  Xmpp_callbacks.request_disco s contact.User.jid session.User.resource

let random_string () =
  let open Nocrypto in
  let rnd = Rng.generate 12 in
  Cstruct.to_string (Base64.encode rnd)

let cleanups users =
  User.reset_receipt_requests users ;
  Xmpp_callbacks.cancel_keepalive () ;
  Xmpp_callbacks.keepalive_running := false ;
  xmpp_session := None
