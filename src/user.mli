
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

val session_info : session -> string
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

val property_to_string : property -> string

type direction = [
  | `From of Xjid.t
  | `To of Xjid.t * string (* id *)
  | `Local of Xjid.t * string
]

val jid_of_direction : direction -> Xjid.t

type message = {
  direction  : direction ;
  encrypted  : bool ;
  received   : bool ;
  timestamp  : float ;
  message    : string ;
  mutable persistent : bool ; (* internal use only (mark whether this needs to be written) *)
} with sexp

val message : ?timestamp:float -> direction -> bool -> bool -> string -> message

type user = {
  bare_jid          : Xjid.bare_jid ;
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

val new_user : jid:Xjid.bare_jid ->
               ?name:string option ->
               ?groups:string list ->
               ?subscription:subscription ->
               ?otr_fingerprints:fingerprint list ->
               ?preserve_messages:bool ->
               ?properties:property list ->
               ?active_sessions:session list ->
               ?otr_custom_config:Otr.State.config option ->
               unit ->
               user

val info : user -> string list

(* messages *)
val insert_message : ?timestamp:float -> user -> direction -> bool -> bool -> string -> user
val received_message : user -> string -> user
val new_message : user -> message -> user

(* convenience *)
val userid : user -> session -> string
val reset_user : user -> user

(* OTR things *)
val encrypted : Otr.State.session -> bool
val pp_fingerprint : string -> string
val pp_binary_fingerprint : string -> string
val otr_fingerprint : Otr.State.session -> string option

(* fingerprint *)
val replace_fp : user -> fingerprint -> user
val find_raw_fp : user -> string -> fingerprint
val verified_fp : user -> string -> verification_status

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
val load_history : Xjid.t -> string -> bool -> message list
val load_user : string -> user option
val load_users : string -> string -> user list (* for the users.sexp file which no longer exists *)
val store_user : user -> string option
