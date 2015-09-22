
module Jid : sig
  type bare_jid = string * string

  val bare_jid_of_sexp : Sexplib.Type.t -> bare_jid
  val sexp_of_bare_jid : bare_jid -> Sexplib.Type.t

  val bare_jid_to_string : bare_jid -> string

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
end

type presence = [
  | `Online | `Free | `Away | `DoNotDisturb | `ExtendedAway | `Offline
]

val presence_to_string : presence -> string
val presence_to_char : presence -> string
val string_to_presence : string -> presence option

type receipt_state = [
  | `Unknown
  | `Requested
  | `Supported
  | `Unsupported
]

val receipt_state_to_string : receipt_state -> string

type session = {
  resource : string ;
  presence : presence ;
  status   : string option ;
  priority : int ;
  otr      : Otr.State.session ;
  dispose  : bool ;
  receipt  : receipt_state ;
}

val presence_unmodified : session -> presence -> string option -> int -> bool

type verification_status = [
  | `Verified
  | `Unverified
  | `Revoked
]

type fingerprint = {
  data          : string ;
  verified      : verification_status ;
  resources     : string list ;
  session_count : int
}

type subscription = [
  | `None
  | `From
  | `To
  | `Both
  | `Remove
]

val subscription_to_string : subscription -> string
val subscription_to_chars : subscription -> string * string

type property = [
  | `Pending | `PreApproved
]

type direction = [
  | `From of Jid.t
  | `To of Jid.t * string (* id *)
  | `Local of Jid.t * string
]

val jid_of_direction : direction -> Jid.t

type message = {
  direction  : direction ;
  encrypted  : bool ;
  received   : bool ;
  timestamp  : float ;
  message    : string ;
  mutable persistent : bool ; (* internal use only (mark whether this needs to be written) *)
}

type user = {
  bare_jid          : Jid.bare_jid ;
  name              : string option ;
  groups            : string list ;
  subscription      : subscription ;
  properties        : property list ;
  preserve_messages : bool ;
  message_history   : message list ; (* persistent if preserve_messages is true *)
  saved_input_buffer: string ; (* not persistent *)
  readline_history  : string list ; (* not persistent *)
  otr_fingerprints  : fingerprint list ;
  otr_custom_config : Otr.State.config option ;
  active_sessions   : session list ; (* not persistent *)
  expand            : bool ; (* not persistent *)
}

(* messages *)
val insert_message : ?timestamp:float -> user -> direction -> bool -> bool -> string -> user
val received_message : user -> string -> user

(* convenience *)
val jid : user -> string
val userid : user -> session -> string

(* OTR things *)
val encrypted : Otr.State.session -> bool
val pp_fingerprint : string -> string
val pp_binary_fingerprint : string -> string
val otr_fingerprint : Otr.State.session -> string option

(* fingerprint *)
val replace_fp : user -> fingerprint -> user
val find_raw_fp : user -> string -> fingerprint
val verified_fp : user -> string -> verification_status

type users

(* actions on users *)
val fold : (Jid.bare_jid -> user -> 'a -> 'a) -> users -> 'a -> 'a
val iter : (Jid.bare_jid -> user -> unit) -> users -> unit
val create : unit -> users
val length : users -> int

(* locating and creating a user *)
val find_user : users -> Jid.bare_jid -> user
val replace_user : users -> user -> unit
val find_or_create : users -> Jid.t -> user

(* removal *)
val remove : users -> Jid.bare_jid -> unit

(* messing around with sessions *)
val replace_session : user -> session -> user * bool

val update_otr : user -> session -> Otr.State.session -> user

val sorted_sessions : user -> session list

(* locating a session, creating, ... *)
val create_session : user -> string -> Otr.State.config -> Nocrypto.Dsa.priv -> user * session

val find_session : user -> string -> session option
val find_similar_session : user -> string -> session option

val active_session : user -> session option

(* persistency operations *)
val load_history : Jid.t -> string -> bool -> message list
val load_user : string -> user option
val load_users : string -> string -> users (* for the users.sexp file which no longer exists *)
val marshal_history : user -> string option
val store_user : user -> string option
