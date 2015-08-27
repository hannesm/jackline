
module Jid = struct
  open Sexplib.Conv

  type bare_jid = string * string with sexp
  type full_jid = bare_jid * string with sexp

  type t = [
  | `Full of full_jid
  | `Bare of bare_jid
  ] with sexp

  let t_to_bare = function
    | `Full (b, _) -> b
    | `Bare b -> b

  let resource = function
    | `Full (_, r) -> Some r
    | `Bare _ -> None

  let bare_jid_to_string (u, d) = u ^ "@" ^ d
  let full_jid_to_string (b, r) = bare_jid_to_string b ^ "/" ^ r

  let jid_to_string = function
    | `Full full -> full_jid_to_string full
    | `Bare bare -> bare_jid_to_string bare

  let string_to_jid_helper s =
    match Astring.String.cut ~sep:"@" s with
    | None -> None
    | Some (user, rest) ->
       match Astring.String.cut ~sep:"/" rest with
       | None -> Some (user, rest, None)
       | Some (domain, resource) -> Some (user, domain, Some resource)

  let string_to_bare_jid s =
    match string_to_jid_helper s with
    | Some (u, d, _) -> Some (u, d)
    | None -> None

  let string_to_full_jid s =
    match string_to_jid_helper s with
    | Some (u, d, Some r) -> Some ((u, d), r)
    | _ -> None

  let string_to_jid s =
    match string_to_jid_helper s with
    | Some (u, d, Some r) -> Some (`Full ((u, d), r))
    | Some (u, d, None) -> Some (`Bare (u, d))
    | None -> None

  let bare_jid_equal (u, d) (u', d') = u = u' && d = d'

  let compare_bare_jid (u, d) (u', d') =
    match compare u u' with
    | 0 -> compare d d'
    | x -> x

  (*
   xmpp resources should be unique for each client, thus multiple
   sessions between two contacts can be established. great idea!
   unfortunately the real world is crap. look at some resources:
   -mcabber.123456 mcabber.123457
   -276687891418300495410099 276687891418300495410010 ...
   -D87A6DD1
   -gmail.1234D33 ...
   -5d234568880
   -BitlBee7D0D5864
   -AAAA AAAAB (size increases/decreases (because some use random and print without leading 0s))
   -23d22ef2-6bf9-4531-abb2-e42418a4713b, 22fa77f2-9333-41b9-81e1-4cebf042ac18 (agl/xmpp-client)

   thus I have some magic here to uniquify sessions... the idea is
   (read: hand-wavy):
    if two resources share a common prefix and have some random hex numbers,
    they are similar!

    or, if they are both UUIDs (some servers use UUID if the client doesn't come up with a resource)

   this naive obviously fails:
     user X with AAAA comes online, user X with AAAB comes online
     (read: these are similar) -- then AAAA goes offline.. AAAB is
     still online (and this order of events happens on a reconnect due
     to timeout)

   thus only the otr ctx is copied over, and the dispose flag is set...
   when a contact goes offline where dispose is set, the session is removed
 *)

  let hex_chars start s =
    let hex_char = function
      | 'a' .. 'f' | 'A' .. 'F' | '0' .. '9' -> true
      | _ -> false
    in
    let rec go idx s =
      if idx < String.length s then
        if hex_char (String.get s idx) then
          go (succ idx) s
        else
          false
      else
        true
    in
    go start s

  let is_uuid s =
    let open String in
    length s = 36 &&
      hex_chars 0 (sub s 0 8) &&
      get s 8 = '-' &&
      hex_chars 0 (sub s 9 4) &&
      get s 13 = '-' &&
      hex_chars 0 (sub s 14 4) &&
      get s 18 = '-' &&
      hex_chars 0 (sub s 19 4) &&
      get s 23 = '-' &&
      hex_chars 0 (sub s 24 12)

  let resource_similar a b =
    let alen = String.length a
    and blen = String.length b
    in
    if abs (alen - blen) > 2 then
      false (* they're a bit too much off *)
    else if is_uuid a && is_uuid b then
      true
    else
      let stop = min alen blen in
      let rec equal idx =
        if idx < stop then
          if String.get a idx = String.get b idx then
            equal (succ idx)
          else
            idx
        else
          idx
      in
      let prefix_len = equal 0 in
      hex_chars prefix_len a && hex_chars prefix_len b

  (* is jid a part of jid'? *)
  let jid_matches jid jid' =
    match jid, jid' with
    | `Bare bare, `Bare bare' -> bare_jid_equal bare bare'
    | `Bare bare, `Full (bare', _) -> bare_jid_equal bare bare'
    | `Full (bare, resource), `Full (bare', resource') -> bare_jid_equal bare bare' && resource = resource'
    | _ -> false


  let xmpp_jid_to_jid jid =
    let { JID.lnode ; JID.ldomain ; JID.lresource ; _ } = jid in
    let bare = (lnode, ldomain) in
    if lresource = "" then
      `Bare bare
    else
      `Full (bare, lresource)

  let jid_to_xmpp_jid jid = JID.of_string (jid_to_string jid)
end

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
  | `DoNotDisturb -> "do not disturb"
  | `ExtendedAway -> "extended away"
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

type fingerprint = {
  data          : string ;
  verified      : bool ;
  resources     : string list ;
  session_count : int
} with sexp

type subscription = [
  | `None
  | `From
  | `To
  | `Both
  | `Remove
] with sexp

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
] with sexp

type direction = [
  | `From of Jid.t
  | `To of Jid.t * string (* id *)
  | `Local of Jid.t * string
] with sexp

(* TODO: should likely life within each session *)
type message = {
  direction  : direction ;
  encrypted  : bool ;
  received   : bool ;
  timestamp  : float ;
  message    : string ;
  mutable persistent : bool ; (* internal use only (mark whether this needs to be written) *)
} with sexp

type user = {
  bare_jid          : Jid.bare_jid ;
  name              : string option ;
  groups            : string list ;
  subscription      : subscription ;
  properties        : property list ;
  preserve_messages : bool ;
  message_history   : message list ; (* persistent if preserve_messages is true *)
  otr_fingerprints  : fingerprint list ;
  otr_custom_config : Otr.State.config option ;
  active_sessions   : session list ; (* not persistent *)
  expand            : bool ; (* not persistent *)
}

let jid u = Jid.bare_jid_to_string u.bare_jid

let new_user ~jid ?(name=None) ?(groups=[]) ?(subscription=`None) ?(otr_fingerprints=[]) ?(preserve_messages=false) ?(properties=[]) ?(active_sessions=[]) ?(otr_custom_config=None) () =
  let message_history = []
  and expand = false
  in
  { bare_jid = jid ; name ; groups ; subscription ; properties ; otr_fingerprints ; preserve_messages ; active_sessions ; message_history ; otr_custom_config ; expand }

let message direction encrypted received message =
  { direction ; encrypted ; received ;
    timestamp = Unix.time () ; message ; persistent = false }

let insert_message u dir enc rcvd msg =
  { u with message_history = (message dir enc rcvd msg) :: u.message_history }

let received_message u id =
  let tst msg = match msg.direction with
    | `To (_, x) when x = id -> true
    | _ -> false
  in
  try
    { u with message_history = List.map (fun m ->
          if tst m then { m with received = true } else m)
          u.message_history
    }
  with
    Not_found -> u

let encrypted = Otr.State.is_encrypted

let userid u s = Jid.jid_to_string (`Full (u.bare_jid, s.resource))

let format_fp e =
  String.((sub e 0 8) ^ " " ^ (sub e 8 8) ^ " " ^ (sub e 16 8) ^ " " ^ (sub e 24 8) ^ " " ^ (sub e 32 8))

let hex_fingerprint fp =
  match Hex.of_string fp with `Hex e -> e

let otr_fingerprint otr =
  match Otr.Utils.their_fingerprint otr with
  | None   -> None
  | Some x -> Some (hex_fingerprint x)

let replace_fp u fp =
  let otr_fingerprints =
    let others = List.filter (fun x -> x.data <> fp.data) u.otr_fingerprints in
    fp :: others
  in
  { u with otr_fingerprints }

let find_raw_fp u raw =
  try List.find (fun x -> x.data = raw) u.otr_fingerprints with
    Not_found -> { data = raw ; verified = false ; resources = []; session_count = 0 }

let verified_fp u raw =
  let fps = find_raw_fp u raw in
  fps.verified

module StringHash =
  struct
    type t = Jid.bare_jid
    let equal = Jid.bare_jid_equal
    let hash = Hashtbl.hash
  end

module Users = Hashtbl.Make(StringHash)
type users = user Users.t

let keys users =
  let us = Users.fold (fun k _ acc -> k :: acc) users [] in
  List.sort compare us

let add_or_replace users user =
  if Users.mem users user.bare_jid then
    Users.replace users user.bare_jid user
  else
    Users.add users user.bare_jid user

let reset_x users f =
  List.iter (fun id ->
      let u = Users.find users id in
      let active_sessions = List.map f u.active_sessions
      in
      Users.replace users id { u with active_sessions })
    (keys users)

let reset_receipt_requests users =
  reset_x users (fun s ->
     let receipt = match s.receipt with
       | `Requested -> `Unknown
       | x -> x
     in
     { s with receipt })

let reset_status users =
  reset_x users (fun s -> { s with presence = `Offline })

let find users jid =
  if Users.mem users jid then
    Some (Users.find users jid)
  else
    None

let find_or_create users jid =
  let bare = Jid.t_to_bare jid in
  match find users bare with
    | Some x -> x
    | None   ->
      let user = new_user ~jid:bare () in
      Users.replace users bare user ;
      user

let add_message users jid dir enc rcvd msg =
  let bare = Jid.t_to_bare jid in
  let user = find_or_create users jid in
  let user = insert_message user dir enc rcvd msg in
  Users.replace users bare user

let replace_user users user =
  Users.replace users user.bare_jid user

let replace_session_1 user session =
  let others = List.filter (fun s -> s.resource <> session.resource) user.active_sessions in
  let active_sessions =
    if session.dispose && session.presence = `Offline then
      others
    else
      session :: others
  in
  { user with active_sessions }

let replace_session users user session =
  let user = replace_session_1 user session in
  Users.replace users user.bare_jid user

let get_session user tst =
  if List.exists tst user.active_sessions then
    Some (List.find tst user.active_sessions)
  else
    None

let find_session user resource =
  let tst s = s.resource = resource in
  get_session user tst

let find_similar_session user resource =
  let r_similar s = Jid.resource_similar s.resource resource in
  get_session user r_similar

let find_or_create_session user resource config dsa =
  match find_session user resource with
  | Some x -> (user, x)
  | None   ->
    let session = empty_session ~resource ~config dsa () in
    let session, similar = match find_similar_session user resource with
      | None         -> (session, None)
      | Some similar -> ({ session with otr = similar.otr }, Some similar)
    in
    let others = match similar with
      | None   -> user.active_sessions
      | Some x ->
        (* XXX: need to revise notifications! *)
        let others = List.filter (fun s -> x.resource <> s.resource) user.active_sessions in
        if x.presence = `Offline then
          others
        else
          { x with dispose = true } :: others
    in
    ({ user with active_sessions = session :: others },
     session)

let compare_session a b =
  match compare b.priority a.priority with
  | 0 -> compare_presence b.presence a.presence
  | x -> x

let sorted_sessions user =
  List.sort compare_session user.active_sessions

let session users jid otr dsa =
  let user = find_or_create users jid
  and resource = Jid.resource jid
  in
  match resource with
  | Some x ->
     let user, session = find_or_create_session user x otr dsa in
     replace_user users user ;
     session
  | None -> assert false

let active_session user =
  if List.length user.active_sessions = 0 then
    None
  else
    let sorted = List.sort compare_session user.active_sessions in
    let s = List.filter (fun x -> x.presence <> `Offline) sorted in
    if List.length s = 0 then
      Some (List.hd sorted)
    else
      Some (List.hd s)

let db_version = 1

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
             let bare_jid = Jid.string_to_bare_jid jabberid in
             (name, bare_jid, groups, preserve_messages, properties, subscription, otr_fingerprints, otr_config)
           | Sexp.List [ Sexp.Atom "bare_jid" ; jabberid ] ->
             assert (jid = None);
             let bare_jid = Jid.bare_jid_of_sexp jabberid in
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
           | Sexp.List [ Sexp.Atom "otr_fingerprints" ; fps ] ->
             assert (otr_fingerprints = None);
             let otr_fingerprints = list_of_sexp fingerprint_of_sexp fps in
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


let record kvs =
  Sexp.List (List.map (fun (k, v) -> (Sexp.List [Sexp.Atom k; v])) kvs)

let sexp_of_t t =
  record [
    "name"             , sexp_of_option sexp_of_string t.name ;
    "bare_jid"         , Jid.sexp_of_bare_jid t.bare_jid ;
    "groups"           , sexp_of_list sexp_of_string t.groups ;
    (* TODO: rename preserve_messages and bump version *)
    "preserve_history" , sexp_of_bool t.preserve_messages ;
    "properties"       , sexp_of_list sexp_of_property t.properties ;
    "subscription"     , sexp_of_subscription t.subscription ;
    "otr_fingerprints" , sexp_of_list sexp_of_fingerprint t.otr_fingerprints ;
    "otr_custom_config", sexp_of_option Otr.State.sexp_of_config t.otr_custom_config ;
  ]


let tr_m s =
  let open Sexp in
  let tr_dir = function
    | List [ Atom "From" ; Atom jid ] ->
       (match Jid.string_to_jid jid with
        | Some jid -> List [ Atom "From" ; Jid.sexp_of_t jid ]
        | None -> Printf.printf "from failed" ;
                  List [ Atom "From" ; Jid.sexp_of_t (`Bare ("none", "none")) ])
    | x -> x
  in
  match s with
  | List s ->
     let r = List.fold_left (fun acc s ->
        let s = match s with
          | List [ Atom "direction" ; value ] -> List [ Atom "direction" ; tr_dir value ]
          | x -> x
        in
        s :: acc) [] s
     in
     List (List.rev r)
  | x -> x

let tr_1 jid s =
  let open Sexp in
  let tr_dir = function
    | List [ Atom "To" ; id ] ->
       List [ Atom "To" ; List [ Jid.sexp_of_t jid ; id ] ]
    | List [ Atom "Local" ; data ] ->
       List [ Atom "Local" ; List [ Jid.sexp_of_t jid ; data ] ]
    | x -> x
  in
  match s with
  | List s ->
     let r = List.fold_left (fun acc s ->
        let s = match s with
          | List [ Atom "direction" ; value ] -> List [ Atom "direction" ; tr_dir value ]
          | x -> x
        in
        s :: acc) [] s
     in
     List (List.rev r)
  | x -> x

let load_history jid file strict =
  let load_h = function
    | Sexp.List [ ver ; Sexp.List msgs ] ->
      let version = int_of_sexp ver in
      ( match version with
        | 0 -> List.map message_of_sexp (List.map (tr_1 jid) (List.map tr_m msgs))
        | 1 -> List.map message_of_sexp (List.map (tr_1 jid) msgs)
        | 2 -> List.map message_of_sexp msgs
        | _ -> Printf.printf "unknown message format" ; [] )
    | _ -> Printf.printf "parsing history failed" ; []
  in
  match (try Some (Sexp.load_rev_sexps file) with _ -> None) with
    | Some hists -> List.flatten (List.map load_h hists)
    | _ ->
      if strict then
        Printf.printf "parsing histories failed" ;
      []

let load_user bytes =
  try (match Sexp.of_string bytes with
       | Sexp.List [ ver ; user ] ->
          let version = int_of_sexp ver in
          (try t_of_sexp user version with _ -> None)
       | _ -> None)
    with _ -> None

let load_users hist_dir bytes =
  let table = Users.create 100 in
  ( try (match Sexp.of_string bytes with
       | Sexp.List [ ver ; Sexp.List users ] ->
         let version = int_of_sexp ver in
         List.iter (fun s ->
             match try t_of_sexp s version with _ -> None with
               | None -> Printf.printf "parse failure %s\n%!" (Sexp.to_string_hum s)
               | Some u ->
                  let message_history =
                    load_history
                      (`Bare u.bare_jid)
                      (Filename.concat hist_dir (jid u)) u.preserve_messages
                  in
                  let u = { u with message_history } in
                  add_or_replace table u)
           users
       | _ -> Printf.printf "parse failed while parsing db\n")
    with _ -> () ) ;
  table

let marshal_history user =
  if user.preserve_messages then
    let new_msgs =
      List.filter (fun m ->
          match m.direction, m.persistent with
          | `Local _, _    -> false
          | _       , true -> false
          | _              -> true)
        user.message_history
    in
    List.iter (fun x -> x.persistent <- true) new_msgs ;
    let hist_version = sexp_of_int 2 in
    if List.length new_msgs > 0 then
      let sexps = List.map sexp_of_message new_msgs in
      let sexp = Sexp.(List [ hist_version ; List sexps ]) in
      Some (jid user, Sexp.to_string_mach sexp)
    else
      None
  else
    None

let store_user user =
  match user.preserve_messages, user.otr_fingerprints, user.otr_custom_config with
  | false, [], None -> None
  | _ ->
     let u_sexp = sexp_of_t user in
     Some Sexp.(to_string_hum (List [ sexp_of_int db_version ; u_sexp ]))
