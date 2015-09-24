type bare_jid = string * string

val bare_jid_of_sexp : Sexplib.Type.t -> bare_jid
val sexp_of_bare_jid : bare_jid -> Sexplib.Type.t

val bare_jid_to_string : bare_jid -> string
val string_to_bare_jid : string -> bare_jid option

val bare_jid_equal : bare_jid -> bare_jid -> bool

type full_jid = bare_jid * string

val full_jid_of_sexp : Sexplib.Type.t -> full_jid
val sexp_of_full_jid : full_jid -> Sexplib.Type.t

val full_jid_to_string : full_jid -> string
val string_to_full_jid : string -> full_jid option

type t = [
  | `Full of full_jid
  | `Bare of bare_jid
]

val t_of_sexp : Sexplib.Type.t -> t
val sexp_of_t : t -> Sexplib.Type.t

val t_to_bare : t -> bare_jid
val resource : t -> string option

val jid_to_string : t -> string
val string_to_jid : string -> t option

val compare_bare_jid : bare_jid -> bare_jid -> int

val resource_similar : string -> string -> bool

val jid_matches : t -> t -> bool

val xmpp_jid_to_jid : JID.t -> t
val jid_to_xmpp_jid : t -> JID.t

