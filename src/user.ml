
type presence = [
  | `Online | `Free | `Away | `DoNotDisturb | `ExtendedAway | `Offline
]

let compare_presence (a : presence) (b : presence) =
  (* ordering:
     - free and online are best
     - followed by away
     - followed by extendedaway and do no disturb
     - offline is last *)
  match a, b with
  | `Free, `Free -> 0
  | `Free, `Online -> 0
  | `Online, `Online -> 0
  | `Online, `Free -> 0
  | `Online, _ -> 1
  | `Free, _ -> 1
  | _, `Online -> -1
  | _, `Free -> -1
  | `Away, `Away -> 0
  | `Away, _ -> 1
  | _, `Away -> -1
  | `DoNotDisturb, `DoNotDisturb -> 0
  | `ExtendedAway, `ExtendedAway -> 0
  | `ExtendedAway, `DoNotDisturb -> 0
  | `DoNotDisturb, `ExtendedAway -> 0
  | `DoNotDisturb, _ -> 1
  | _, `DoNotDisturb -> -1
  | `ExtendedAway, _ -> 1
  | _, `ExtendedAway -> -1
  | `Offline, `Offline -> 0

let presence_to_string = function
  | `Online       -> "online"
  | `Free         -> "free"
  | `Away         -> "away"
  | `DoNotDisturb -> "dnd"
  | `ExtendedAway -> "xa"
  | `Offline      -> "offline"

let string_to_presence = function
  | "free"    | "f" -> Some `Free
  | "away"    | "a" -> Some `Away
  | "dnd"     | "d" -> Some `DoNotDisturb
  | "xa"      | "x" -> Some `ExtendedAway
  | "offline" | "_" -> Some `Offline
  | "online"  | "o" -> Some `Online
  | _               -> None

let presence_to_char = function
  | `Online       -> "o"
  | `Free         -> "f"
  | `Away         -> "a"
  | `DoNotDisturb -> "d"
  | `ExtendedAway -> "x"
  | `Offline      -> "_"

type receipt_state = [
  | `Unknown
  | `Requested
  | `Supported
  | `Unsupported
]

let receipt_state_to_string = function
  | `Unknown -> "unknown"
  | `Requested -> "requested"
  | `Supported -> "supported"
  | `Unsupported -> "unsupported"

type session = {
  resource : string ;
  presence : presence ;
  status   : string option ;
  priority : int ;
  otr      : Otr.State.session ;
  dispose  : bool ;
  receipt  : receipt_state ;
}

let presence_unmodified session presence status priority =
  session.presence = presence &&
    session.status = status &&
      session.priority = priority

let empty_session ~resource ?(presence=`Offline) ?otr ?config ?(priority=0) ?(status=None) ?(dispose=false) dsa () =
  let otr = match otr, config with
    | Some otr, _         -> otr
    | None    , Some conf -> Otr.State.new_session conf dsa ()
    | _ -> assert false
  in {
    resource ;
    presence ;
    status ;
    priority ;
    otr ;
    dispose ;
    receipt = `Unknown ;
  }

open Sexplib
open Sexplib.Conv

type verify = [ `Manual | `SMP ] [@@deriving sexp]

let verify_to_string = function
  | `Manual -> "manual"
  | `SMP -> "smp"

type verification_status = [
  | `Verified of (verify * Ptime.date) list
  | `Unverified
  | `Revoked of Ptime.date
]

let verification_status_to_string = function
  | `Revoked _ -> "REVOKED"
  | `Unverified -> "not verified"
  | `Verified _ -> "verified"

let day_to_string (yy, mm, dd) = Printf.sprintf "%04d-%02d-%02d" yy mm dd

let full_verification_status_to_string = function
  | `Revoked ts -> "REVOKED since " ^ day_to_string ts
  | `Unverified -> "not verified"
  | `Verified xs ->
     let evs = List.map (fun (v, ts) -> verify_to_string v ^ " on " ^ day_to_string ts) xs in
     "verified (" ^ (String.concat ", " evs) ^ ")"

type color = [ `Default | `Good | `Bad ]

let verification_status_to_color = function
  | `Revoked _ -> `Bad
  | `Unverified -> `Bad
  | `Verified _ -> `Good

type fingerprint = {
  data          : string ;
  verified      : verification_status ;
  resources     : string list ;
  session_count : int ;
  first         : Ptime.date ;
  last          : Ptime.date ;
}

let pp_fingerprint e =
  String.((sub e 0 8) ^ " " ^ (sub e 8 8) ^ " " ^ (sub e 16 8) ^ " " ^ (sub e 24 8) ^ " " ^ (sub e 32 8))

let fingerprint_to_string fp =
  let ver = full_verification_status_to_string fp.verified
  and pp_fp = pp_fingerprint fp.data
  and used = "used in " ^ string_of_int fp.session_count ^ " sessions"
  and resources = "resources: " ^ (String.concat ", " fp.resources)
  and first = "first used on " ^ day_to_string fp.first
  and last = "last used on " ^ day_to_string fp.last
  in
  "  " ^ ver ^ " " ^ pp_fp ^ " (" ^ used ^ ", " ^ resources ^ "), " ^ first ^ ", " ^ last

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
] [@@deriving sexp]

let subscription_to_string = function
  | `Both   -> "both"
  | `From   -> "from (they see your presence updates)"
  | `To     -> "to (you see their presence updates)"
  | `None   -> "none"
  | `Remove -> "removed from roster"

let subscription_to_chars = function
  | `Both   -> ("[", "]")
  | `From   -> ("F", "F")
  | `To     -> ("T", "T")
  | `None   -> ("?", "?")
  | `Remove -> ("R", "R")

type property = [
  | `Pending | `PreApproved
] [@@deriving sexp]

let property_to_string = function
  | `Pending -> "pending"
  | `PreApproved -> "preapproved"

type direction = [
  | `From of Xjid.t
  | `To of Xjid.t * string (* id *)
  | `Local of Xjid.t * string
] [@@deriving sexp]

let jid_of_direction = function
  | `From j -> j
  | `To (j, _) -> j
  | `Local (j, _) -> j

type chatkind = [
  | `Transit
  | `Chat
  | `GroupChat
  | `Presence
  | `Info
  | `Error
  | `Warning
  | `Success
] [@@deriving sexp]

let chatkind_to_string k =
  Sexplib.Sexp.to_string (sexp_of_chatkind k)

type message = {
  direction  : direction ;
  encrypted  : bool ;
  received   : bool ;
  timestamp  : Ptime.t ;
  message    : string ;
  kind       : chatkind ;
  mutable persistent : bool ; (* internal use only (mark whether this needs to be written) *)
}

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
  history_position  : int ; (* not persistent *)
  otr_fingerprints  : fingerprint list ;
  otr_custom_config : Otr.State.config option ;
  active_sessions   : session list ; (* not persistent *)
  expand            : bool ; (* not persistent *)
  self              : bool ; (* not persistent *)
}

let compare_session a b =
  match compare b.priority a.priority with
  | 0 -> compare_presence b.presence a.presence
  | x -> x

let sorted_sessions user =
  List.sort compare_session user.active_sessions

let session_info act s =
  let prio = string_of_int s.priority
  and pres = presence_to_string s.presence
  and status = match s.status with
    | None -> ""
    | Some x -> " - " ^ x
  and receipts = receipt_state_to_string s.receipt
  and active = match act with
    | Some a when a.resource = s.resource -> "(active) "
    | _ -> ""
  in
  s.resource ^ " " ^ active ^ "(" ^ prio ^ ") (receipts " ^ receipts ^ "): " ^ pres ^ status

let info u s =
  let groups =
    match u.groups with
    | [] -> []
    | xs -> ["groups: " ^ (String.concat ", " xs)]
  and add =
    let add = String.concat "," (List.map property_to_string u.properties) in
    ["subscription: " ^ subscription_to_string u.subscription ^ add]
  and sessions =
    List.map (session_info s) (sorted_sessions u)
  in
  groups @ add @ sessions

let new_user ~jid ?(name=None) ?(groups=[]) ?(subscription=`None) ?(otr_fingerprints=[]) ?(preserve_messages=false) ?(properties=[]) ?(active_sessions=[]) ?(otr_custom_config=None) () =
  let message_history = []
  and expand = false
  and self = false
  and input_buffer = ([], [])
  and readline_history = []
  and history_position = 0
  in
  { bare_jid = jid ; name ; groups ; subscription ; properties ; otr_fingerprints ; preserve_messages ; active_sessions ; message_history ; input_buffer ; readline_history ; otr_custom_config ; expand ; self ; history_position }

let active_session user =
  let sorted = sorted_sessions user in
  match List.filter (fun x -> x.presence <> `Offline) sorted, sorted with
  | x :: _, _ -> Some x
  | [], x :: _ -> Some x
  | _ -> None

let oneline user =
  let pr, po =
    if user.self then
      ("{", "}")
    else
      subscription_to_chars user.subscription
  and jid = Xjid.bare_jid_to_string user.bare_jid
  and presence =
    match active_session user with
    | None -> `Offline
    | Some s -> s.presence
  in
  let p = presence_to_char presence in
  Printf.sprintf "%s%s%s %s" pr p po jid

let oneline_with_session _ s =
  let presence = s.presence
  and resource = s.resource
  in
  Printf.sprintf " %s %s" (presence_to_char presence) resource

let message ?(timestamp = Ptime_clock.now ()) ?(kind = `Chat) direction encrypted received message =
  { direction ; encrypted ; received ;
    timestamp ; message ; persistent = false ; kind }

let new_message user message =
  { user with message_history = message :: user.message_history }

let insert_message ?timestamp ?kind u dir enc rcvd msg =
  let message = message ?timestamp ?kind dir enc rcvd msg in
  new_message u message

let encrypted = Otr.State.is_encrypted

let userid u s = Xjid.jid_to_string (`Full (u.bare_jid, s.resource))

let reset_user u =
  let active_sessions =
    List.map
      (fun s ->
       let receipt = match s.receipt with
         | `Requested -> `Unknown
         | x -> x
       and presence = `Offline
       in
       { s with receipt ; presence })
      u.active_sessions
  in
  { u with active_sessions }

let hex_fp fp =
  match Hex.of_string fp with `Hex e -> e

let pp_binary_fingerprint fp =
  pp_fingerprint (hex_fp fp)

let otr_fingerprint otr =
  match Otr.Utils.their_fingerprint otr with
  | None   -> None
  | Some x -> Some (hex_fp x)

let replace_fp u fp =
  let otr_fingerprints =
    let others = List.filter (fun x -> x.data <> fp.data) u.otr_fingerprints in
    fp :: others
  in
  { u with otr_fingerprints }

let today () =
  let now = Ptime_clock.now () in
  let date, _ = Ptime.to_date_time now in
  date

let verify_fp user fp m =
  let now = today () in
  let verified = match fp.verified with
    | `Verified xs -> `Verified ((m, now) :: xs)
    | _ -> `Verified [(m, now)]
  in
  replace_fp user { fp with verified }

let revoke_fp user fp =
  let now = today () in
  replace_fp user { fp with verified = `Revoked now }

let used_fp user fp resource =
  let resources =
    if List.mem resource fp.resources then
      fp.resources
    else
      resource :: fp.resources
  in
  let fp = {
    fp with
    session_count = succ fp.session_count ;
    resources ;
    last = today ()
  }
  in
  replace_fp user fp

let find_raw_fp u raw =
  try List.find (fun x -> x.data = raw) u.otr_fingerprints with
    Not_found -> { data = raw ; verified = `Unverified ; resources = []; session_count = 0 ; first = today () ; last = today () }

let verified_fp u raw =
  let fps = find_raw_fp u raw in
  fps.verified

let replace_session user session =
  let others = List.filter (fun s -> s.resource <> session.resource) user.active_sessions in
  let active_sessions, removed =
    if session.dispose && session.presence = `Offline then
      others, true
    else
      session :: others, false
  in
  ({ user with active_sessions }, removed)

let update_otr user session otr =
  let others = List.filter (fun s -> s.resource <> session.resource) user.active_sessions
  and session = { session with otr }
  in
  { user with active_sessions = session :: others }

let get_session user tst =
  if List.exists tst user.active_sessions then
    Some (List.find tst user.active_sessions)
  else
    None

let find_session user resource =
  let tst s = s.resource = resource in
  get_session user tst

let find_similar_session user resource =
  let r_similar s = Xjid.resource_similar s.resource resource in
  get_session user r_similar

let create_session user resource config dsa =
  assert (not (List.exists (fun s -> s.resource = resource) user.active_sessions)) ;
  let session = empty_session ~resource ~config dsa () in
  ({ user with active_sessions = session :: user.active_sessions }, session)

let db_version = 1

let fix_fp s =
  let tr_verified = function
    | "true" -> "Verified"
    | "false" -> "Unverified"
    | x -> x
  in
  let open Sexp in
  let tr_verified' = function
    | "Verified" -> List [ Atom "Verified" ; List [ List [Atom "Manual"; Atom "0"] ] ]
    | "Revoked" -> List [ Atom "Revoked" ; Atom "0" ]
    | x -> Atom x
  in
  match s with
  | List s ->
     let r = List.fold_left (fun acc s ->
       let s = match s with
         | List [ Atom "verified" ; Atom value ] -> List [ Atom "verified" ; tr_verified' (tr_verified value) ]
         | x -> x
       in
       s :: acc) [] s
     in
     let r =
       if
         List.exists
           (function List [ Atom "first" ; _ ] -> true | _ -> false)
           r
       then
         r
       else
         List [ Atom "first" ; Atom "0" ] :: List [ Atom "last" ; Atom "0" ] :: r
     in
     List r
  | x -> x

let record kvs =
  Sexp.List (List.map (fun (k, v) -> (Sexp.List [Sexp.Atom k; v])) kvs)


let date_of_sexp s =
  let f = float_of_sexp s in
  match Ptime.of_float_s f with
  | None -> (0,0,0)
  | Some d -> let date, _ = Ptime.to_date_time d in date

let sexp_of_date d =
  let f =  match Ptime.of_date_time (d, ((0, 0, 0), 0)) with
    | Some x -> Ptime.to_float_s x
    | None -> 0.
  in
  sexp_of_float f

let verification_status_of_sexp = function
  | Sexp.Atom "Unverified" ->
     `Unverified
  | Sexp.List [ Sexp.Atom "Revoked" ; day ] ->
     `Revoked (date_of_sexp day)
  | Sexp.List [ Sexp.Atom "Verified" ; vs ] ->
     let vs = list_of_sexp (pair_of_sexp verify_of_sexp date_of_sexp) vs in
     `Verified vs
  | _ -> assert false


let sexp_of_verification_status = function
  | `Revoked day -> Sexp.List [ Sexp.Atom "Revoked" ; sexp_of_date day ]
  | `Verified xs -> Sexp.List [ Sexp.Atom "Verified" ; sexp_of_list (sexp_of_pair sexp_of_verify sexp_of_date) xs ]
  | `Unverified -> Sexp.Atom "Unverified"

let fingerprint_of_sexp = function
  | Sexp.List l ->
     (match List.fold_left
              (fun (data, verified, resources, session_count, first, last) v -> match v with
               | Sexp.List [ Sexp.Atom "data" ; Sexp.Atom data ] ->
                  (Some data, verified, resources, session_count, first, last)
               | Sexp.List [ Sexp.Atom "verified" ; v ] ->
                  let verified = verification_status_of_sexp v in
                  (data, Some verified, resources, session_count, first, last)
               | Sexp.List [ Sexp.Atom "resources" ; r ] ->
                  let resources = list_of_sexp string_of_sexp r in
                  (data, verified, Some resources, session_count, first, last)
               | Sexp.List [ Sexp.Atom "session_count" ; c ] ->
                  let session_count = int_of_sexp c in
                  (data, verified, resources, Some session_count, first, last)
               | Sexp.List [ Sexp.Atom "first" ; d ] ->
                  let first = date_of_sexp d in
                  (data, verified, resources, session_count, Some first, last)
               | Sexp.List [ Sexp.Atom "last" ; d ] ->
                  let last = date_of_sexp d in
                  (data, verified, resources, session_count, first, Some last)
               | _ -> assert false)
               (None, None, None, None, None, None) l
      with
      | Some data, Some verified, Some resources, Some session_count, Some first, Some last ->
         { data ; verified ; resources ; session_count ; first ; last }
      | _ -> assert false)
  | _ -> assert false


let sexp_of_fingerprint f =
  record [
      "data", sexp_of_string f.data ;
      "verified", sexp_of_verification_status f.verified ;
      "resources", sexp_of_list sexp_of_string f.resources ;
      "session_count", sexp_of_int f.session_count ;
      "first", sexp_of_date f.first ;
      "last", sexp_of_date f.last ;
    ]

let t_of_sexp t version =
  match t with
  | Sexp.List l ->
    (match
       List.fold_left (fun (name, jid, groups, preserve_messages, properties, subscription, otr_fingerprints, otr_config) v -> match v with
           | Sexp.List [ Sexp.Atom "name" ; nam ] ->
             assert (name = None);
             let name = match version with
               | 0 -> let str = string_of_sexp nam in
                 if str = "" then None else Some str
               | _ -> option_of_sexp string_of_sexp nam
             in
             (Some name, jid, groups, preserve_messages, properties, subscription, otr_fingerprints, otr_config)
           | Sexp.List [ Sexp.Atom "jid" ; Sexp.Atom jabberid ] ->
             assert (jid = None);
             let bare_jid = Xjid.string_to_bare_jid jabberid in
             (name, bare_jid, groups, preserve_messages, properties, subscription, otr_fingerprints, otr_config)
           | Sexp.List [ Sexp.Atom "bare_jid" ; jabberid ] ->
             assert (jid = None);
             let bare_jid = Xjid.bare_jid_of_sexp jabberid in
             (name, Some bare_jid, groups, preserve_messages, properties, subscription, otr_fingerprints, otr_config)
           | Sexp.List [ Sexp.Atom "groups" ; gps ] ->
             assert (groups = None);
             let groups = list_of_sexp string_of_sexp gps in
             (name, jid, Some groups, preserve_messages, properties, subscription, otr_fingerprints, otr_config)
           (* TODO: rename to preserve_messages and bump version *)
           | Sexp.List [ Sexp.Atom "preserve_history" ; hf ] ->
             assert (preserve_messages = None) ;
             let preserve_messages = bool_of_sexp hf in
             (name, jid, groups, Some preserve_messages, properties, subscription, otr_fingerprints, otr_config)
           | Sexp.List [ Sexp.Atom "properties" ; p ] ->
             assert (properties = None) ;
             let properties = list_of_sexp property_of_sexp p in
             (name, jid, groups, preserve_messages, Some properties, subscription, otr_fingerprints, otr_config)
           | Sexp.List [ Sexp.Atom "subscription" ; s ] ->
             assert (subscription = None) ;
             let subscription = subscription_of_sexp s in
             (name, jid, groups, preserve_messages, properties, Some subscription, otr_fingerprints, otr_config)
           | Sexp.List [ Sexp.Atom "otr_fingerprints" ; Sexp.List fps ] ->
             assert (otr_fingerprints = None);
             let otr_fingerprints = List.map fingerprint_of_sexp (List.map fix_fp fps) in
             (name, jid, groups, preserve_messages, properties, subscription, Some otr_fingerprints, otr_config)
           | Sexp.List [ Sexp.Atom "otr_custom_config" ; cfg ] ->
             assert (otr_config = None);
             let otr_config = option_of_sexp Otr.State.config_of_sexp cfg in
             (name, jid, groups, preserve_messages, properties, subscription, otr_fingerprints, otr_config)
           | _ -> assert false)
         (None, None, None, None, None, None, None, None) l
     with
     | Some name, Some jid, Some groups, Some preserve_messages, Some properties, Some subscription, Some otr_fingerprints, otr_custom_config ->
       Some (new_user ~jid ~name ~groups ~subscription ~properties ~otr_fingerprints ~preserve_messages ~otr_custom_config ())
     | _ -> None )
  | _ -> None

let sexp_of_t t =
  record [
    "name"             , sexp_of_option sexp_of_string t.name ;
    "bare_jid"         , Xjid.sexp_of_bare_jid t.bare_jid ;
    "groups"           , sexp_of_list sexp_of_string t.groups ;
    (* TODO: rename preserve_messages and bump version *)
    "preserve_history" , sexp_of_bool t.preserve_messages ;
    "properties"       , sexp_of_list sexp_of_property t.properties ;
    "subscription"     , sexp_of_subscription t.subscription ;
    "otr_fingerprints" , sexp_of_list sexp_of_fingerprint t.otr_fingerprints ;
    "otr_custom_config", sexp_of_option Otr.State.sexp_of_config t.otr_custom_config ;
  ]

let load_user bytes =
  try (match Sexp.of_string bytes with
       | Sexp.List [ ver ; user ] ->
          let version = int_of_sexp ver in
          (try t_of_sexp user version with _ -> None)
       | _ -> None)
    with _ -> None

let load_users bytes =
  ( try (match Sexp.of_string bytes with
       | Sexp.List [ ver ; Sexp.List users ] ->
         let version = int_of_sexp ver in
         List.fold_left (fun acc s ->
             match try t_of_sexp s version with _ -> None with
               | None -> Printf.printf "parse failure %s\n%!" (Sexp.to_string_hum s) ; acc
               | Some u -> u :: acc)
           [] users
       | _ -> Printf.printf "parse failed while parsing db\n" ; [])
    with _ -> [] )

let marshal_user user =
  match user.preserve_messages, user.otr_fingerprints, user.otr_custom_config with
  | false, [], None -> None
  | _ ->
     let sexp = sexp_of_t user in
     Some Sexp.(List [ sexp_of_int db_version ; sexp ])

let message_of_sexp sexp =
  match sexp with
  | Sexp.List l ->
     (match List.fold_left
              (fun (dir, encrypted, received, ts, msg, kind) v -> match v with
               | Sexp.List [ Sexp.Atom "direction" ; d ] ->
                  let dir = direction_of_sexp d in
                  (Some dir, encrypted, received, ts, msg, kind)
               | Sexp.List [ Sexp.Atom "encrypted" ; e ] ->
                  let encrypted = bool_of_sexp e in
                  (dir, Some encrypted, received, ts, msg, kind)
               | Sexp.List [ Sexp.Atom "received" ; r ] ->
                  let received = bool_of_sexp r in
                  (dir, encrypted, Some received, ts, msg, kind)
               | Sexp.List [ Sexp.Atom "timestamp" ; ts ] ->
                  let ts = Ptime.of_float_s (float_of_sexp ts) in
                  (dir, encrypted, received, ts, msg, kind)
               | Sexp.List [ Sexp.Atom "message" ; m ] ->
                  let msg = string_of_sexp m in
                  (dir, encrypted, received, ts, Some msg, kind)
               | Sexp.List [ Sexp.Atom "kind" ; m ] ->
                  let kind = chatkind_of_sexp m in
                  (dir, encrypted, received, ts, msg, Some kind)
               | _ -> (dir, encrypted, received, ts, msg, kind))
              (None, None, None, None, None, None) l
      with
      | Some direction, Some encrypted, Some received, Some timestamp, Some message, Some kind ->
         { direction ; encrypted ; received ; timestamp ; message ; kind ; persistent = true }
      | _ -> assert false)
  | _ -> assert false

let sexp_of_message msg =
  record [
      "direction", sexp_of_direction msg.direction ;
      "encrypted", sexp_of_bool msg.encrypted ;
      "received", sexp_of_bool msg.received ;
      "timestamp", sexp_of_float (Ptime.to_float_s msg.timestamp) ;
      "message", sexp_of_string msg.message ;
      "kind", sexp_of_chatkind msg.kind ;
    ]
