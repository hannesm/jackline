
open Sexplib
open Sexplib.Conv

type t = {
  version : int ;
  jid : JID.t ;
  port : int ;
  password : string ;
  trust_anchor : string option ;
  tls_fingerprint : string option ;
  otr_config : Otr.State.config ;
}

let empty = {
  version = 1 ;
  jid = JID.of_string "user@server/resource" ;
  port = 5222 ;
  password = "" ;
  trust_anchor = None ;
  tls_fingerprint = None ;
  otr_config = Otr.State.default_config
}

let t_of_sexp t =
  match t with
  | Sexp.List l ->
      List.fold_left (fun t v -> match v with
        | Sexp.List [ Sexp.Atom "version" ; v ] ->
          let version = int_of_sexp v in
          { t with version }
        | Sexp.List [ Sexp.Atom "jid" ; Sexp.Atom v ] ->
          let jid = try JID.of_string v with _ -> Printf.printf "parse error in jid" ; t.jid in
          { t with jid }
        | Sexp.List [ Sexp.Atom "port" ; port ] ->
          { t with port = int_of_sexp port }
        | Sexp.List [ Sexp.Atom "password" ; Sexp.Atom password ] ->
          { t with password }
        | Sexp.List [ Sexp.Atom "trust_anchor" ; trust_anchor ] ->
          (match t.version with
           | 0 -> { t with trust_anchor = Some (string_of_sexp trust_anchor) }
           | _ -> { t with trust_anchor = option_of_sexp string_of_sexp trust_anchor } )
        | Sexp.List [ Sexp.Atom "tls_fingerprint" ; tls_fp ] ->
          { t with tls_fingerprint = option_of_sexp string_of_sexp tls_fp }
        | Sexp.List [ Sexp.Atom "otr_config" ; v ] ->
          { t with otr_config = Otr.State.config_of_sexp v }
        | _ -> assert false)
        empty l
  | _ -> Printf.printf "unknown t\n" ; raise (Invalid_argument "broken config")

let record kvs =
  Sexp.List List.(map (fun (k, v) -> (Sexp.List [Sexp.Atom k; v])) kvs)

let sexp_of_t t =
  record [
    "version", sexp_of_int t.version ;
    "jid" , sexp_of_string (JID.string_of_jid t.jid) ;
    "port" , sexp_of_int t.port ;
    "password" , sexp_of_string t.password ;
    "trust_anchor" , sexp_of_option sexp_of_string t.trust_anchor ;
    "tls_fingerprint" , sexp_of_option sexp_of_string t.tls_fingerprint ;
    "otr_config" , Otr.State.sexp_of_config t.otr_config ;
  ]

let load_config bytes =
  try Some (t_of_sexp (Sexp.of_string bytes)) with _ -> None

let store_config t =
  Sexp.to_string_mach (sexp_of_t t)
