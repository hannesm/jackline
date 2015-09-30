
type buddy = [ `User of User.user | `Room of Muc.groupchat ]

val compare_buddy : buddy -> buddy -> int

val name : buddy -> string option
val bare : buddy -> Xjid.bare_jid
val preserve_messages : buddy -> bool
val expanded : buddy -> bool
val messages : buddy -> User.message list
val saved_input_buffer : buddy -> string
val readline_history : buddy -> string list

val received : buddy -> string -> buddy

val info : buddy -> string list

val expand : buddy -> buddy
val set_saved_input_buffer : buddy -> string -> buddy
val add_readline_history : buddy -> string -> buddy
val set_preserve_messages : buddy -> bool -> buddy

val reset : buddy -> buddy
val clear_messages : buddy -> buddy

val marshal_history : buddy -> string option
val load_history : string -> buddy -> buddy

val active_presence : buddy -> User.presence

val new_message : buddy -> User.message -> buddy

type resource = [ `Session of User.session | `Member of Muc.member ]

val presence : resource -> User.presence

val all_resources : buddy -> resource list
val active_resources : (Xjid.t -> bool) -> buddy -> resource list

val active : buddy -> resource option

val full_jid : buddy -> resource -> Xjid.full_jid

val jid : buddy -> resource option -> Xjid.t

type color = [ `Default | `Good | `Bad ]
val color : buddy -> resource option -> color

val store : buddy -> Sexplib.Sexp.t option

type buddies

(* actions on buddies *)
val create : unit -> buddies
val length : buddies -> int

val fold : (Xjid.bare_jid -> buddy -> 'a -> 'a) -> buddies -> 'a -> 'a
val iter : (Xjid.bare_jid -> buddy -> unit) -> buddies -> unit

(* locating and creating a buddy *)
val find_buddy : buddies -> Xjid.bare_jid -> buddy option
val replace_buddy : buddies -> buddy -> unit

val find_room : buddies -> Xjid.bare_jid -> Muc.groupchat option
val find_user : buddies -> Xjid.bare_jid -> User.user option
val replace_room : buddies -> Muc.groupchat -> unit
val replace_user : buddies -> User.user -> unit

(* removal *)
val remove : buddies -> Xjid.bare_jid -> unit
