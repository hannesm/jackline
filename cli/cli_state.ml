
type ui_state = {
  config_directory            : string                    ; (* set initially *)
  user                        : User.user                 ; (* set initially *)
  session                     : User.session              ; (* set initially *)

  users                       : User.users                ; (* read from disk, extended by xmpp callbacks *)

  mutable active_contact      : string                    ; (* modified by scrolling *)
  mutable last_active_contact : string                    ; (* modified by scrolling *)

  mutable notifications       : string list               ; (* list to blink *)

  mutable show_offline        : bool                      ; (* F5 stuff *)
  mutable show_buddy_list     : bool                      ; (* F12 stuff *)
  mutable scrollback          : int                       ; (* scroll-pgup/down state *)

  mutable last_status         : (User.direction * string) ; (* internal use only *)
}

let empty_ui_state config_directory user session users =
  let active = user.User.jid in
  let last_status = (`Local "", "") in
  {
    config_directory              ;
    user                          ;
    session                       ;

    users                         ;

    active_contact      = active  ;
    last_active_contact = active  ;

    notifications       = []      ;

    show_offline        = true    ;
    show_buddy_list     = true    ;
    scrollback          = 0       ;

    last_status                   ;
}

let status_log state = state.user.User.message_history

let add_status state dir msg =
  let user = User.new_message state.user dir false true msg in
  User.Users.replace state.users user.User.jid user

let (xmpp_session : Xmpp_callbacks.user_data Xmpp_callbacks.XMPPClient.session_data option ref) = ref None
