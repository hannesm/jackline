
type ui_state = {
  user : User.user ; (* set initially *)
  session : User.session ; (* set initially *)
  mutable log : (Unix.tm * string * string) list ; (* set by xmpp callbacks -- should be time * string list *)
  mutable active_chat : (User.user * User.session option) ; (* modified by user (scrolling through buddies) *)
  users : User.users ; (* extended by xmpp callbacks *)
  mutable notifications : User.user list ;
  mutable show_offline : bool ;
}

let empty_ui_state user session users = {
  user ;
  session ;
  log = [] ;
  active_chat = (user, Some session) ;
  users ;
  notifications = [] ;
  show_offline = true ;
}
