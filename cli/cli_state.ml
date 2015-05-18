
type display_mode =
  | BuddyList
  | FullScreen
  | Raw

let next_display_mode = function
  | BuddyList  -> FullScreen
  | FullScreen -> Raw
  | Raw        -> BuddyList

type ui_state = {
  config_directory            : string                    ; (* set initially *)
  user                        : string                    ; (* set initially *)
  resource                    : string                    ; (* set initially *)

  users                       : User.users                ; (* read from disk, extended by xmpp callbacks *)

  mutable active_contact      : string                    ; (* modified by scrolling *)
  mutable last_active_contact : string                    ; (* modified by scrolling *)

  mutable notifications       : string list               ; (* list to blink *)

  mutable show_offline        : bool                      ; (* F5 stuff *)
  mutable window_mode         : display_mode              ; (* F12 stuff *)
  mutable scrollback          : int                       ; (* scroll-pgup/down state *)

  mutable last_status         : (User.direction * string) ; (* internal use only *)
}

let empty_ui_state config_directory user resource users =
  let last_status = (`Local "", "") in
  {
    config_directory                ;
    user                            ;
    resource                        ;

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
