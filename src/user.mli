
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


type verify = [ `Manual | `SMP ]

type verification_status = [
  | `Verified of (verify * Ptime.date) list
  | `Unverified
  | `Revoked of Ptime.date
]

type color = [ `Default | `Good | `Bad ]

val verification_status_to_string : verification_status -> string

val verification_status_to_color : verification_status -> color

type fingerprint = {
  data          : string ;
  verified      : verification_status ;
  resources     : string list ;
  session_count : int ;
  first         : Ptime.date ;
  last          : Ptime.date ;
}

val fingerprint_to_string : fingerprint -> string

type subscription_mod = [
  | `Probe
  | `Subscribe
  | `Subscribed
  | `Unsubscribe
  | `Unsubscribed
]

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

type chatkind = [
  | `Chat
  | `GroupChat
] with sexp

type message = {
  direction  : direction ;
  encrypted  : bool ;
  received   : bool ;
  timestamp  : Ptime.t ;
  message    : string ;
  kind       : chatkind ;
  mutable persistent : bool ; (* internal use only (mark whether this needs to be written) *)
} with sexp

val message : ?timestamp:Ptime.t -> ?kind:chatkind -> direction -> bool -> bool -> string -> message

type user = {
  bare_jid          : Xjid.bare_jid ;
  name              : string option ;
  groups            : string list ;
  subscription      : subscription ;
  properties        : property list ;
  preserve_messages : bool ;
  message_history   : message list ; (* persistent if preserve_messages is true *)
  input_buffer: (int list * int list) ; (* not persistent *)
  readline_history  : string list ; (* not persistent *)
  otr_fingerprints  : fingerprint list ;
  otr_custom_config : Otr.State.config option ;
  active_sessions   : session list ; (* not persistent *)
  expand            : bool ; (* not persistent *)
  self              : bool ; (* not persistent *)
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

val info : user -> session option -> string list

val oneline : user -> string
val oneline_with_session : user -> session -> string

(* messages *)
val insert_message : ?timestamp:Ptime.t -> user -> direction -> bool -> bool -> string -> user
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
val find_raw_fp : user -> string -> fingerprint
val verified_fp : user -> string -> verification_status

val verify_fp : user -> fingerprint -> verify -> user
val revoke_fp : user -> fingerprint -> user
val used_fp : user -> fingerprint -> string -> user

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
val load_user : string -> user option
val load_users : string -> user list (* for the users.sexp file which no longer exists *)
val marshal_user : user -> Sexplib.Sexp.t option
