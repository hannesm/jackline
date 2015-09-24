
type buddy = [ `User of User.user | `Room of Muc.groupchat ]

val bare : buddy -> Xjid.bare_jid

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
