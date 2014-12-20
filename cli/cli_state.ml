
type ui_state = {
  config_directory            : string                    ; (* set initially *)
  user                        : string                    ; (* set initially *)
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
  let last_status = (`Local "", "") in
  {
    config_directory              ;
    user                          ;
    session                       ;

    users                         ;

    active_contact      = user    ;
    last_active_contact = user    ;

    notifications       = []      ;

    show_offline        = true    ;
    show_buddy_list     = true    ;
    scrollback          = 0       ;

    last_status                   ;
}

let add_status state dir msg =
  let self = User.Users.find state.users state.user in
  let user = User.new_message self dir false true msg in
  User.Users.replace state.users state.user user

let (xmpp_session : Xmpp_callbacks.user_data Xmpp_callbacks.XMPPClient.session_data option ref) = ref None
