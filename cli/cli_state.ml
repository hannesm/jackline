
type display_mode =
  | BuddyList
  | FullScreen
  | Raw

let next_display_mode = function
  | BuddyList  -> FullScreen
  | FullScreen -> Raw
  | Raw        -> BuddyList

type notification_state =
  | Connected
  | Disconnected
  | Notifications
  | Quit

let string_of_notification_state = function
  | Connected     -> "connected"
  | Disconnected  -> "disconnected"
  | Notifications -> "notifications"
  | Quit          -> "quit"

type ui_state = {
  config_directory            : string                    ; (* set initially *)
  user                        : string                    ; (* set initially *)
  resource                    : string                    ; (* set initially *)

  users                       : User.users                ; (* read from disk, extended by xmpp callbacks *)

  mutable notification_state  : notification_state        ; (* keeps track of /notification.state file *)

  mutable active_contact      : string                    ; (* modified by scrolling *)
  mutable last_active_contact : string                    ; (* modified by scrolling *)

  mutable notifications       : string list               ; (* list to blink *)

  mutable show_offline        : bool                      ; (* F5 stuff *)
  mutable window_mode         : display_mode              ; (* F12 stuff *)
  mutable scrollback          : int                       ; (* scroll-pgup/down state *)

  mutable last_status         : (User.direction * string) ; (* internal use only *)
}

let get_notification_state ui_state xmpp_session = match xmpp_session with
  | None   -> Disconnected
  | Some _ ->
    begin try let _ = List.hd ui_state.notifications
         in Notifications 
    with
    | _  -> Connected
    end

let flush_notification_state_file config_directory notification_state =
  let _ =
  let s  = string_of_notification_state notification_state in
  let open Lwt in
    lwt _ =
    Lwt_unix.openfile 
      (config_directory ^ Filename.dir_sep ^ "notification.state")
      Lwt_unix.[ O_WRONLY ; O_TRUNC ; O_CREAT]
      0o640 (* user: RW, group: r, other: no access *)
    >>= fun fd ->
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
        Lwt_io.set_position oc Int64.zero >>= fun () ->
        Lwt_io.write_line oc s >>= fun () ->
        Lwt_io.flush oc >>= fun () ->
        Lwt.join [(Lwt_io.close oc)]
    in Lwt.return_unit
  in ()

let update_notification_state_file ui_state xmpp_session =
  let s  = get_notification_state ui_state xmpp_session in
  if s <> ui_state.notification_state then (
    flush_notification_state_file ui_state.config_directory s ;
    ui_state.notification_state <- s)
  else ()

let empty_ui_state config_directory user resource users =
  let notification_state = Disconnected in
  let () = flush_notification_state_file config_directory notification_state in
  let last_status = (`Local "", "") in
  {
    config_directory                ;
    user                            ;
    resource                        ;

    users                           ;

    notification_state              ;

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

