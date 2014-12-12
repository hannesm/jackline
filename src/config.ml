
open Sexplib
open Sexplib.Conv

type auth = [
  | `Fingerprint of string
  | `Trust_anchor of string
  | `None
] with sexp

type t = {
  version : int ;
  jid : JID.t ;
  port : int ;
  password : string ;
  authenticator : auth ;
  otr_config : Otr.State.config ;
}

let empty = {
  version = 2 ;
  jid = JID.of_string "user@server/resource" ;
  port = 5222 ;
  password = "" ;
  authenticator = `None ;
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
           | 0 -> { t with authenticator = `Trust_anchor (string_of_sexp trust_anchor) }
           | 1 -> ( match option_of_sexp string_of_sexp trust_anchor with
               | None -> t
               | Some x -> { t with authenticator = `Trust_anchor x } )
           | _ -> raise (Invalid_argument "unexpected element: trust_anchor") )
        | Sexp.List [ Sexp.Atom "tls_fingerprint" ; tls_fp ] ->
          if t.version = 1 then
            ( match option_of_sexp string_of_sexp tls_fp with
              | None -> t
              | Some x -> { t with authenticator = `Fingerprint x } )
          else
            raise (Invalid_argument "unexpected element: tls_fingerprint")
        | Sexp.List [ Sexp.Atom "authenticator" ; auth ] ->
          { t with authenticator = auth_of_sexp auth }
        | Sexp.List [ Sexp.Atom "otr_config" ; v ] ->
          { t with otr_config = Otr.State.config_of_sexp v }
        | _ -> assert false)
        empty l
  | _ -> raise (Invalid_argument "broken config")

let record kvs =
  Sexp.List List.(map (fun (k, v) -> (Sexp.List [Sexp.Atom k; v])) kvs)

let sexp_of_t t =
  record [
    "version"       , sexp_of_int t.version ;
    "jid"           , sexp_of_string (JID.string_of_jid t.jid) ;
    "port"          , sexp_of_int t.port ;
    "password"      , sexp_of_string t.password ;
    "authenticator" , sexp_of_auth t.authenticator ;
    "otr_config"    , Otr.State.sexp_of_config t.otr_config ;
  ]

let load_config bytes =
  t_of_sexp (Sexp.of_string bytes)

let store_config t =
  Sexp.to_string_mach (sexp_of_t t)
