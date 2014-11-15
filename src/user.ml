

type session = {
  resource : string ;
  messages : string list ;
(*  presence : ?? ;
    otr :  *)
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

type t = {
  version : int ;
  name : string ;
  jid : JID.t ;
  subscription : subscription_t ;
  otr_fingerprints : fingerprint list ;
  active_sessions : session list
}


let subscription_of_sexp = function
  | Sexp.Atom "SubscriptionNone"   -> SubscriptionNone
  | Sexp.Atom "SubscriptionBoth"   -> SubscriptionBoth
  | Sexp.Atom "SubscriptionFrom"   -> SubscriptionFrom
  | Sexp.Atom "SubscriptionRemove" -> SubscriptionRemove
  | Sexp.Atom "SubscriptionTo"     -> SubscriptionTo
  | _ -> assert false

let t_of_sexp t =
  let empty = {
    version = 0 ;
    name = "" ;
    jid = JID.of_string "a@b" ;
    subscription = SubscriptionNone ;
    otr_fingerprints = [] ;
    active_sessions = []
  } in
  match t with
  | Sexp.List l ->
      List.fold_left (fun t v -> match v with
        | Sexp.List [ Sexp.Atom "version" ; v ] ->
          let version = int_of_sexp v in
          { t with version }
        | Sexp.List [ Sexp.Atom "name" ; Sexp.Atom name ] -> { t with name }
        | Sexp.List [ Sexp.Atom "jid" ; Sexp.Atom v ]   ->
          let jid = try JID.of_string v with _ -> Printf.printf "parse error in jid" ; t.jid in
          { t with jid }
        | Sexp.List [ Sexp.Atom "subscription" ; s ] ->
          let subscription = subscription_of_sexp s in
          { t with subscription }
        | Sexp.List [ Sexp.Atom "otr_fingerprints" ; fps ] ->
          { t with otr_fingerprints = list_of_sexp fingerprint_of_sexp fps }
        | _ -> assert false)
        empty l
  | _ -> Printf.printf "unknown t\n" ; empty

let sexp_of_subscription = function
  | SubscriptionNone   -> Sexp.Atom "SubscriptionNone"
  | SubscriptionBoth   -> Sexp.Atom "SubscriptionBoth"
  | SubscriptionFrom   -> Sexp.Atom "SubscriptionFrom"
  | SubscriptionRemove -> Sexp.Atom "SubscriptionRemove"
  | SubscriptionTo     -> Sexp.Atom "SubscriptionTo"

let record kvs =
  Sexp.List List.(map (fun (k, v) -> (Sexp.List [Sexp.Atom k; v])) kvs)

let sexp_of_t t =
  record [
    "version", sexp_of_int t.version ;
    "name" , sexp_of_string t.name ;
    "jid" , sexp_of_string (JID.string_of_jid t.jid) ;
    "subscription", sexp_of_subscription t.subscription ;
    "otr_fingerprints", sexp_of_list sexp_of_fingerprint t.otr_fingerprints ;
  ]

let load_users bytes =
  list_of_sexp t_of_sexp (Sexp.of_string bytes)

let store_users users =
  Sexp.to_string_mach (Sexp.List (List.map sexp_of_t users))
