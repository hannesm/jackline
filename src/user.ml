

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

type direction = [ `From | `To | `Local ]

type session = {
  resource : string ;
  mutable presence : presence ;
  mutable status : string option ;
  mutable priority : int ;
  mutable messages : (direction * bool * bool * Unix.tm * string) list ;   (* direction, encryption, received, timestamp, msg *)
  mutable otr : Otr.State.session
}

let empty_session resource config () = {
  resource ;
  presence = `Offline ;
  status = None ;
  priority = 0 ;
  messages = [] ;
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

let subscription_to_chars = function
  | `Both -> ("[", "]")
  | `From -> ("[", "?")
  | `To   -> ("?", "]")
  | `None -> ("?", "?")

type props = [
  | `Pending | `PreApproved
]

type user = {
  name : string ; (* TODO: option *)
  jid : string ; (* user@domain, unique key *)
  groups : string list ;
  subscription : subscription ;
  props : props list ; (* not persistent *)
  mutable otr_fingerprints : fingerprint list ;
  mutable active_sessions : session list (* not persistent *)
}

let encrypted ctx =
  match Otr.State.(ctx.state.message_state) with
  | `MSGSTATE_ENCRYPTED _ -> true
  | _ -> false

let userid u s = match s.resource with
  | r when r = "" -> u.jid
  | r -> u.jid ^ "/" ^ r

let fingerprint otr =
  let hex x = match Hex.of_string (Cstruct.to_string x) with
      `Hex e -> (String.((sub e 0 8) ^ " " ^ (sub e 8 8) ^ " " ^ (sub e 16 8) ^ " " ^ (sub e 24 8) ^ " " ^ (sub e 32 8)), e)
  in
  match otr.Otr.State.their_dsa with
  | None -> ("No OTR", "")
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
  let fp, raw = fingerprint otr in
  let fps = find_raw_fp u raw in
  (fp, fps)

let verified_fp u raw =
  let fps = find_raw_fp u raw in
  fps.verified


let empty = {
  name = "" ;
  jid = "a@b" ;
  groups = [] ;
  subscription = `None ;
  props = [] ;
  otr_fingerprints = [] ;
  active_sessions = []
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
  (if not (List.exists
             (fun s -> s.resource = lresource)
             user.active_sessions)
   then
     let sess = empty_session lresource otr_cfg () in
     user.active_sessions <- (sess :: user.active_sessions ) );
  List.find (fun s -> s.resource = lresource) user.active_sessions

let good_session user =
  if List.length user.active_sessions = 0 then
    None
  else
    let s = List.filter (fun x -> x.presence <> `Offline) user.active_sessions in
    if List.length s = 0 then
      Some (List.hd user.active_sessions)
    else
      Some (List.hd (List.sort (fun a b -> compare b.priority a.priority) s))

let t_of_sexp t =
  match t with
  | Sexp.List l ->
      List.fold_left (fun t v -> match v with
        | Sexp.List [ Sexp.Atom "name" ; Sexp.Atom name ] -> { t with name }
        | Sexp.List [ Sexp.Atom "jid" ; Sexp.Atom jid ] ->
          { t with jid }
        | Sexp.List [ Sexp.Atom "groups" ; gps ] ->
          { t with groups = list_of_sexp string_of_sexp gps }
        | Sexp.List [ Sexp.Atom "subscription" ; s ] ->
          let subscription = subscription_of_sexp s in
          { t with subscription }
        | Sexp.List [ Sexp.Atom "otr_fingerprints" ; fps ] ->
          { t with otr_fingerprints = list_of_sexp fingerprint_of_sexp fps }
        | _ -> assert false)
        empty l
  | _ -> Printf.printf "unknown t\n" ; empty


let record kvs =
  Sexp.List List.(map (fun (k, v) -> (Sexp.List [Sexp.Atom k; v])) kvs)

let sexp_of_t t =
  record [
    "name" , sexp_of_string t.name ;
    "jid" , sexp_of_string t.jid ;
    "groups", sexp_of_list sexp_of_string t.groups ;
    "subscription", sexp_of_subscription t.subscription ;
    "otr_fingerprints", sexp_of_list sexp_of_fingerprint t.otr_fingerprints ;
  ]

let load_users bytes =
  let table = Users.create 100 in
  ( try (match Sexp.of_string bytes with
       | Sexp.List [ ver ; Sexp.List users ] ->
         let version = int_of_sexp ver in
         List.iter (fun s ->
             match try Some (t_of_sexp s) with _ -> None with
               | None -> Printf.printf "parse failure %s\n%!" (Sexp.to_string_hum s)
               | Some u ->
                 let id = u.jid in
                 if Users.mem table id then
                   Printf.printf "key %s already present in table, ignoring\n%!" id
                 else
                   Users.add table id u)
           users
       | _ -> Printf.printf "parse failed while parsing db\n")
    with _ -> () ) ;
  table

let store_users users =
  let users = Users.fold (fun _ s acc -> (sexp_of_t s) :: acc) users [] in
  Sexp.to_string_mach (Sexp.List [ sexp_of_int 0 ; Sexp.List users ])
