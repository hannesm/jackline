

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

type session = {
  resource : string ;
  mutable presence : presence ;
  mutable status : string (* XXX: option *);
  mutable priority : int ;
  mutable messages : string list ;
  mutable otr : Otr.State.session
}

let empty_session resource config () = {
  resource ;
  presence = `Offline ;
  status = "" ;
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

type props = [
  | `Pending | `PreApproved
]

type user = {
  name : string ;
  jid : string ; (* user@domain, unique key *)
  groups : string list ;
  subscription : subscription ;
  props : props list ; (* not persistent *)
  otr_fingerprints : fingerprint list ;
  mutable active_sessions : session list (* not persistent *)
}

module Users = Map.Make(String)
type users = user Users.t

let empty = {
  name = "" ;
  jid = "a@b" ;
  groups = [] ;
  subscription = `None ;
  props = [] ;
  otr_fingerprints = [] ;
  active_sessions = []
}

let find_or_get jid s =
  if Users.mem jid s then
    Users.find jid s
  else
    { empty with jid }

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
  try (match Sexp.of_string bytes with
      | Sexp.List [ ver ; Sexp.List users ] ->
        let version = int_of_sexp ver in
        List.fold_left (fun acc s ->
            match try Some (t_of_sexp s) with _ -> None with
              | None -> Printf.printf "parse failure %s\n%!" (Sexp.to_string_hum s); acc
              | Some u -> Users.add u.jid u acc)
          Users.empty users
      | _ -> Printf.printf "parse failed while parsing db\n" ; Users.empty)
  with _ -> Users.empty

let store_users users =
  let users = Users.fold (fun _ s acc -> (sexp_of_t s) :: acc) users [] in
  Sexp.to_string_mach (Sexp.List [ sexp_of_int 0 ; Sexp.List users ])
