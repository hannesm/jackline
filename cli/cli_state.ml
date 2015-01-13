open Lwt_io

type display_mode =
  | BuddyList
  | FullScreen
  | Raw

let next_display_mode = function
  | BuddyList  -> FullScreen
  | FullScreen -> Raw
  | Raw        -> BuddyList

type session_status =
  | Connected
  | Disconnected
  | Notifications
  | Quit
  | Default

let string_of_session_status = function
  | Connected     -> "connected"
  | Disconnected  -> "disconnected"
  | Notifications -> "notifications"
  | Quit          -> "quit"
  | Default       -> "undefinedDEFAULT"

type ui_state = {
  config_directory            : string                    ; (* set initially *)
  user                        : string                    ; (* set initially *)
  resource                    : string                    ; (* set initially *)

  session_state_out_channel   : Lwt_io.output_channel Lwt.t  ; (* file handle for session_status reporting*)

  users                       : User.users                ; (* read from disk, extended by xmpp callbacks *)

  mutable active_contact      : string                    ; (* modified by scrolling *)
  mutable last_active_contact : string                    ; (* modified by scrolling *)

  mutable notifications       : string list               ; (* list to blink *)

  mutable show_offline        : bool                      ; (* F5 stuff *)
  mutable window_mode         : display_mode              ; (* F12 stuff *)
  mutable scrollback          : int                       ; (* scroll-pgup/down state *)

  mutable last_status         : (User.direction * string) ; (* internal use only *)
}

let get_session_state ui_state xmpp_session = match xmpp_session with
  | None   -> Disconnected
  | Some _ ->
    begin try let _ = List.hd ui_state.notifications
         in Connected
    with
    | _  -> Notifications
    end

let do_write_session_state oc session_state =
  (* this function should be private; not included in the mli *)
  let s  = string_of_session_status session_state in
  lwt _ = 
  Lwt.bind
    (Lwt.bind
      (Lwt.bind
        (Lwt.return oc)
        (fun oc -> Lwt_io.set_position oc Int64.zero; Lwt.return oc)
      )
      (fun oc -> Lwt_io.write_line oc s; Lwt.return oc)
    )
    (fun oc -> Lwt.join [(Lwt_io.flush oc)])
  in
    Lwt.return_unit

let update_session_state_file ui_state xmpp_session =
  lwt oc = ui_state.session_state_out_channel in
  let s  = get_session_state ui_state xmpp_session
  in
   lwt _ = do_write_session_state oc s in
   Lwt.return_unit

let empty_ui_state config_directory user resource users =
  (*let _ = Lwt.join [(do_write_session_state session_state_out_channel Disconnected)] in*)
  let session_state_out_channel =
  let x =
     lwt fd = 
      Lwt_unix.openfile
        (config_directory ^ Filename.dir_sep ^ "ui.state")
        Lwt_unix.[ O_WRONLY ; O_TRUNC ; O_CREAT]
        0o640 (* user: RW, group: r, other: no access *)
     in
       Lwt.return (Lwt_io.of_fd Output fd)
    in x
  in
  let last_status = (`Local "", "") in
  {
    config_directory                ;
    user                            ;
    resource                        ;

    session_state_out_channel       ;

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

