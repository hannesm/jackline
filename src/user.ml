

type session = {
  resource : string ;
  mutable presence : string ;
  mutable priority : int ;
  mutable messages : string list ;
  mutable otr : Otr.State.session
}

open Sexplib
open Sexplib.Conv

open Roster

type fingerprint = {
  data : string ;
  verified : bool ;
  resource : string ;
  session_count : int
} with sexp

type subscription =
  | No
  | From
  | To
  | Both
  | Pending
  | PreApproved
with sexp

type user = {
  name : string ;
  jid : JID.t ;
  groups : string list ;
  subscription : subscription ;
  otr_fingerprints : fingerprint list ;
  active_sessions : session list
}

module User = struct
 type t = user
 let compare a b = JID.compare a.jid b.jid
end
module Users = Set.Make(User)

let eq_t a b = JID.compare a.jid b.jid = 0

let rec find_or_insert one =
  function
  | [] -> [one]
  | x :: xs when eq_t x one -> one :: xs
  | x :: xs -> x :: find_or_insert one xs

let rec remove j =
  function
  | [] -> []
  | x :: xs when JID.compare x.jid j = 0 -> xs
  | x :: xs -> x :: remove j xs

let empty = {
  name = "" ;
  jid = JID.of_string "a@b" ;
  groups = [] ;
  subscription = No ;
  otr_fingerprints = [] ;
  active_sessions = []
}

let t_of_sexp t =
  match t with
  | Sexp.List l ->
      List.fold_left (fun t v -> match v with
        | Sexp.List [ Sexp.Atom "name" ; Sexp.Atom name ] -> { t with name }
        | Sexp.List [ Sexp.Atom "jid" ; Sexp.Atom v ] ->
          let jid = try JID.of_string v with
              _ -> Printf.printf "parse error in jid" ; t.jid in
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
    "jid" , sexp_of_string (JID.string_of_jid t.jid) ;
    "groups", sexp_of_list sexp_of_string t.groups ;
    "subscription", sexp_of_subscription t.subscription ;
    "otr_fingerprints", sexp_of_list sexp_of_fingerprint t.otr_fingerprints ;
  ]

let load_users bytes =
  try (match Sexp.of_string bytes with
      | Sexp.List [ ver ; Sexp.List users ] ->
        let version = int_of_sexp ver in
        Printf.printf "parsing user db version %d\n" version ;
        List.fold_left (fun acc s ->
            try Users.add (t_of_sexp s) acc with
              _ -> Printf.printf "parse error in user entry\n" ; acc)
          Users.empty users
      | _ -> Printf.printf "parse failed while parsing db\n" ; Users.empty)
  with _ -> Users.empty

let store_users users =
  let users = Users.fold (fun s acc -> (sexp_of_t s) :: acc) users [] in
  Sexp.to_string_mach (Sexp.List [ sexp_of_int 0 ; Sexp.List users ])
