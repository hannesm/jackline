
type ui_state = {
  config_directory : string ;
  user : User.user ; (* set initially *)
  session : User.session ; (* set initially *)
  mutable last_status : (User.direction * string) ;
  mutable active_chat : (User.user * User.session option) ; (* modified by user (scrolling through buddies) *)
  mutable last_active_chat : (User.user * User.session option) ;
  users : User.users ; (* extended by xmpp callbacks *)
  mutable notifications : User.user list ;
  mutable show_offline : bool ;
  mutable show_buddy_list: bool ;
  mutable scrollback : int ;
}

let empty_ui_state config_directory user session users = {
  config_directory ;
  user ;
  session ;
  last_status = (`Local "", "") ;
  active_chat = (user, Some session) ;
  last_active_chat = (user, Some session) ;
  users ;
  notifications = [] ;
  show_offline = true ;
  show_buddy_list = true ;
  scrollback = 0 ;
}

let status_log state = state.user.User.message_history

let add_status state dir msg =
  User.new_message state.user dir false true msg

let (xmpp_session : Xmpp_callbacks.user_data Xmpp_callbacks.XMPPClient.session_data option ref) = ref None
