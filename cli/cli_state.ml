
type ui_state = {
  config_directory : string ;
  user : User.user ; (* set initially *)
  session : User.session ; (* set initially *)
  mutable log : (Unix.tm * string * string) list ; (* set by xmpp callbacks -- should be time * string list *)
  mutable active_chat : (User.user * User.session option) ; (* modified by user (scrolling through buddies) *)
  mutable last_active_chat : (User.user * User.session option) ;
  users : User.users ; (* extended by xmpp callbacks *)
  mutable notifications : User.user list ;
  mutable show_offline : bool ;
  mutable scrollback : int ;
}

let empty_ui_state config_directory user session users = {
  config_directory ;
  user ;
  session ;
  log = [] ;
  active_chat = (user, Some session) ;
  last_active_chat = (user, Some session) ;
  users ;
  notifications = [] ;
  show_offline = true ;
  scrollback = 0 ;
}

let (xmpp_session : Xmpp_callbacks.user_data Xmpp_callbacks.XMPPClient.session_data option ref) = ref None
