

type presence = [
  | `Online | `Free | `Away | `DoNotDisturb | `ExtendedAway | `Offline
]

let presence_to_string = function
  | `Online -> "online"
  | `Free -> "free"
  | `Away -> "away"
  | `DoNotDisturb -> "do not disturb"
  | `ExtendedAway -> "extended away"
  | `Offline -> "offline"

let string_to_presence = function
  | "free" | "f" -> `Free
  | "away" | "a" -> `Away
  | "dnd" | "d" -> `DoNotDisturb
  | "xa" | "x" -> `ExtendedAway
  | "offline" | "_" -> `Offline
  | "online" | "o" | _ -> `Online

let presence_to_char = function
  | `Online -> "o"
  | `Free -> "f"
  | `Away -> "a"
  | `DoNotDisturb -> "d"
  | `ExtendedAway -> "x"
  | `Offline -> "_"

type session = {
  resource : string ;
  mutable presence : presence ;
  mutable status : string option ;
  mutable priority : int ;
  mutable otr : Otr.State.session
}

let empty_session resource config () = {
  resource ;
  presence = `Offline ;
  status = None ;
  priority = 0 ;
  otr = Otr.State.new_session config ()
}

open Sexplib
open Sexplib.Conv

type fingerprint = {
  data : string ;
  verified : bool ;
  resources : string list ;
  session_count : int
} with sexp

type subscription = [
  | `None
  | `From
  | `To
  | `Both
] with sexp

let subscription_to_string = function
  | `Both -> "both"
  | `From -> "from"
  | `To -> "to"
  | `None -> "none"

let subscription_to_chars = function
  | `Both -> ("[", "]")
  | `From -> ("[", "?")
  | `To   -> ("?", "]")
  | `None -> ("?", "?")

type property = [
  | `Pending | `PreApproved
] with sexp

type direction = [
  | `From of string (* full jid *)
  | `To of string (* id *)
  | `Local
] with sexp

type message = {
  direction  : direction ;
  encrypted  : bool ;
  received   : bool ;
  timestamp  : float ;
  message    : string ;
  persistent : bool ; (* internal use only (mark whether this needs to be written) *)
} with sexp

let message direction encrypted received message =
  { direction ; encrypted ; received ;
    timestamp = Unix.time () ; message ; persistent = false }

type user = {
  jid                      : string ; (* user@domain, unique key *)
  name                     : string option ;
  groups                   : string list ;
  subscription             : subscription ;
  properties               : property list ;
  mutable preserve_history : bool ;
  mutable history          : message list ; (* persistent if preserve_history is true *)
  mutable otr_fingerprints : fingerprint list ;
  mutable active_sessions  : session list (* not persistent *)
}

let new_message u dir enc rcvd msg =
  u.history <- (message dir enc rcvd msg) :: u.history

let encrypted ctx =
  match Otr.State.(ctx.state.message_state) with
  | `MSGSTATE_ENCRYPTED _ -> true
  | _ -> false

let userid u s = match s.resource with
  | r when r = "" -> u.jid
  | r -> u.jid ^ "/" ^ r

let fingerprint otr =
  let hex x = match Hex.of_string (Cstruct.to_string x) with
      `Hex e -> (String.((sub e 0 8) ^ " " ^ (sub e 8 8) ^ " " ^ (sub e 16 8) ^ " " ^ (sub e 24 8) ^ " " ^ (sub e 32 8)), Some e)
  in
  match otr.Otr.State.their_dsa with
  | None -> ("No OTR", None)
  | Some x -> hex (Otr.Crypto.OtrDsa.fingerprint x)

let replace u fp =
  u.otr_fingerprints <-
    fp :: (List.filter (fun x -> x.data <> fp.data) u.otr_fingerprints)

let insert_inc u r fp =
  replace u
    { fp with
      session_count = succ fp.session_count ;
      resources = r :: (List.filter (fun x -> x <> r) fp.resources)
    }

let find_raw_fp u raw =
  try List.find (fun x -> x.data = raw) u.otr_fingerprints with
    Not_found -> { data = raw ; verified = false ; resources = []; session_count = 0 }

let find_fp u otr =
  match fingerprint otr with
  | fp, Some raw ->
    let fps = find_raw_fp u raw in
    (fp, Some fps)
  | fp, None -> (fp, None)

let verified_fp u raw =
  let fps = find_raw_fp u raw in
  fps.verified


let empty = {
  name             = None ;
  jid              = "a@b" ;
  groups           = [] ;
  subscription     = `None ;
  properties       = [] ;
  history          = [] ;
  preserve_history = false ;
  otr_fingerprints = [] ;
  active_sessions  = []
}

module StringHash =
  struct
    type t = string
    let equal a b = a = b
    let hash = Hashtbl.hash
  end

module Users = Hashtbl.Make(StringHash)
type users = user Users.t

let keys users =
  let us = Users.fold (fun k _ acc -> k :: acc) users [] in
  List.sort compare us

let bare_jid jid =
  let { JID.lnode ; JID.ldomain ; JID.lresource } = jid in
  (lnode ^ "@" ^ ldomain, lresource)

let find_or_add jid users =
  let id, _ = bare_jid jid in
  if Users.mem users id then
    Users.find users id
  else
    let t = { empty with jid = id } in
    Users.add users id t ;
    t

let ensure_session jid otr_cfg user =
  let { JID.lresource } = jid in
  let r_matches l s = s.resource = l in
  (if not (List.exists (r_matches lresource) user.active_sessions) then
     let sess = empty_session lresource otr_cfg () in
     (if List.exists (r_matches "/special/") user.active_sessions then
        let dummy = List.find (r_matches "/special/") user.active_sessions in
        sess.otr <- dummy.otr ;
        user.active_sessions <-
          List.filter (fun s -> not (r_matches "/special/" s)) user.active_sessions) ;
     user.active_sessions <- (sess :: user.active_sessions ) );
  List.find (r_matches lresource) user.active_sessions

let good_session user =
  if List.length user.active_sessions = 0 then
    None
  else
    let s = List.filter (fun x -> x.presence <> `Offline) user.active_sessions in
    if List.length s = 0 then
      Some (List.hd user.active_sessions)
    else
      Some (List.hd (List.sort (fun a b -> compare b.priority a.priority) s))

let t_of_sexp t version =
  match t with
  | Sexp.List l ->
      let u = List.fold_left (fun t v -> match v with
        | Sexp.List [ Sexp.Atom "name" ; nam ] ->
          let name = match version with
            | 0 -> let str = string_of_sexp nam in
                   if str = "" then None else Some str
            | n -> option_of_sexp string_of_sexp nam
          in
          { t with name }
        | Sexp.List [ Sexp.Atom "jid" ; Sexp.Atom jid ] ->
          { t with jid }
        | Sexp.List [ Sexp.Atom "groups" ; gps ] ->
          { t with groups = list_of_sexp string_of_sexp gps }
	| Sexp.List [ Sexp.Atom "preserve_history"; hf ] ->
          { t with preserve_history = bool_of_sexp hf}
        | Sexp.List [ Sexp.Atom "properties" ; p ] ->
          { t with properties = list_of_sexp property_of_sexp p }
        | Sexp.List [ Sexp.Atom "subscription" ; s ] ->
          { t with subscription = subscription_of_sexp s }
        | Sexp.List [ Sexp.Atom "otr_fingerprints" ; fps ] ->
          { t with otr_fingerprints = list_of_sexp fingerprint_of_sexp fps }
        | _ -> assert false)
        empty l
      in
      Some u
  | _ -> Printf.printf "ignoring unknown user\n" ; None


let record kvs =
  Sexp.List (List.map (fun (k, v) -> (Sexp.List [Sexp.Atom k; v])) kvs)

let sexp_of_t t =
  record [
    "name"            , sexp_of_option sexp_of_string t.name ;
    "jid"             , sexp_of_string t.jid ;
    "groups"          , sexp_of_list sexp_of_string t.groups ;
    "preserve_history", sexp_of_bool t.preserve_history ;
    "properties"      , sexp_of_list sexp_of_property t.properties ;
    "subscription"    , sexp_of_subscription t.subscription ;
    "otr_fingerprints", sexp_of_list sexp_of_fingerprint t.otr_fingerprints ;
  ]

let load_history file =
  let load_h = function
    | Sexp.List [ ver ; Sexp.List msgs ] ->
      let version = int_of_sexp ver in
      ( match version with
        | 0 -> List.map message_of_sexp msgs
        | _ -> Printf.printf "unknown message format" ; [] )
    | _ -> Printf.printf "parsing history failed" ; []
  in
  match (try Some (Sexp.load_rev_sexps file) with _ -> None) with
    | Some hists -> List.flatten (List.map load_h hists)
    | _ -> Printf.printf "parsing histories failed" ; []

let load_users hist_dir bytes =
  let table = Users.create 100 in
  ( try (match Sexp.of_string bytes with
       | Sexp.List [ ver ; Sexp.List users ] ->
         let version = int_of_sexp ver in
         List.iter (fun s ->
             match try t_of_sexp s version with _ -> None with
               | None -> Printf.printf "parse failure %s\n%!" (Sexp.to_string_hum s)
               | Some u ->
                 let id = u.jid in
                 if Users.mem table id then
                   Printf.printf "key %s already present in table, ignoring\n%!" id
                 else
                   (u.history <-
                      if u.preserve_history then
                        load_history (Filename.concat hist_dir id)
                      else
                        [] ;
                    Users.add table id u) )
           users
       | _ -> Printf.printf "parse failed while parsing db\n")
    with _ -> () ) ;
  table

let marshal_history user =
  if user.preserve_history then
    let new_msgs =
      List.filter (fun m ->
          match m.direction, m.persistent with
          | `Local, _    -> false
          | _     , true -> false
          | _            -> true)
        user.history
    in
    let new_msgs = List.map (fun x -> { x with persistent = true }) new_msgs in
    if List.length new_msgs > 0 then
      Some (user.jid, List.map sexp_of_message new_msgs)
    else
      None
  else
    None

let store_users users =
  let data = Users.fold (fun _ s acc -> (sexp_of_t s, marshal_history s) :: acc) users [] in
  let users, histories = List.split data in
  let hist_version = sexp_of_int 0 in
  Sexp.(to_string_mach (List [ sexp_of_int 1 ; List users ]),
        List.fold_left (fun acc v ->
            match v with
            | None -> acc
            | Some (u, history) ->
              (u, to_string_mach (List [ hist_version ; List history ])) :: acc)
          []
          histories)
