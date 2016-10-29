
type contact = [ `User of User.user | `Room of Muc.groupchat ]

val compare_contact : contact -> contact -> int

val name : contact -> string option
val bare : contact -> Xjid.bare_jid
val preserve_messages : contact -> bool
val expanded : contact -> bool
val messages : contact -> User.message list
val input_buffer : contact -> (int list * int list)

val readline_history : contact -> string list
val add_readline_history : contact -> string -> contact
val history_position : contact -> int
val set_history_position : contact -> int -> contact

val received : contact -> string -> contact

val expand : contact -> contact
val set_input_buffer : contact -> (int list * int list) -> contact
val set_preserve_messages : contact -> bool -> contact

val reset : contact -> contact
val clear_messages : contact -> contact

val marshal_history : contact -> string option
val load_history : string -> contact -> contact

val active_presence : contact -> User.presence

val new_message : contact -> User.message -> contact

type resource = [ `Session of User.session | `Member of Muc.member ]

val info : contact -> resource option -> string list

val presence : resource -> User.presence

val all_resources : contact -> resource list
val active_resources : (Xjid.t -> bool) -> contact -> resource list

val active : contact -> resource option

val full_jid : contact -> resource -> Xjid.full_jid

val jid : contact -> resource option -> Xjid.t

val color : contact -> resource option -> User.color
val oneline : contact -> resource option -> string

val marshal : contact -> Sexplib.Sexp.t option
val load : string -> contact option

type contacts

(* actions on contacts *)
val create : unit -> contacts
val length : contacts -> int

val fold : (Xjid.bare_jid -> contact -> 'a -> 'a) -> contacts -> 'a -> 'a
val iter : (Xjid.bare_jid -> contact -> unit) -> contacts -> unit

(* locating and creating a contact *)
val find_contact : contacts -> Xjid.bare_jid -> contact option
val replace_contact : contacts -> contact -> unit

val find_room : contacts -> Xjid.bare_jid -> Muc.groupchat option
val find_user : contacts -> Xjid.bare_jid -> User.user option
val replace_room : contacts -> Muc.groupchat -> unit
val replace_user : contacts -> User.user -> unit

(* removal *)
val remove : contacts -> Xjid.bare_jid -> unit
