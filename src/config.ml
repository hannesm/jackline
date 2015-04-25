
open Sexplib
open Sexplib.Conv

type auth = [
  | `Fingerprint of string
  | `Trust_anchor of string
] with sexp

type t = {
  version : int ;
  jid : JID.t ;
  priority : int option ;
  hostname : string option ;
  port : int option ;
  password : string option ;
  authenticator : auth ;
  otr_config : Otr.State.config ;
}

let current_version = 3

let dsa_of_cfg_sexp t =
  match t with
  | Sexp.List l ->
    (match
      List.fold_left (fun dsa -> function
           | Sexp.List [ Sexp.Atom "otr_config" ; Sexp.List cfg ] ->
             List.fold_left (fun dsa -> function
                 | Sexp.List [ Sexp.Atom "policies" ; _ ] -> dsa
                 | Sexp.List [ Sexp.Atom "versions" ; _ ] -> dsa
                 | Sexp.List [ Sexp.Atom "dsa" ; d ] -> Some (Nocrypto.Dsa.priv_of_sexp d)
                 | _ -> raise (Invalid_argument "broken sexp while trying to find dsa"))
               dsa cfg
           | _ -> dsa)
        None l
     with
     | Some dsa -> dsa
     | None -> raise (Invalid_argument "couldn't find dsa"))
  | _ -> raise (Invalid_argument "couldn't parse sexp (dsa)")

let t_of_sexp dsa t =
  let dsa = match dsa with
    | None -> dsa_of_cfg_sexp t
    | Some x -> x
  in
  match t with
  | Sexp.List l ->
    (match
      List.fold_left (fun (ver, jid, prio, host, port, pass, auth, otr_cfg) -> function
          | Sexp.List [ Sexp.Atom "version" ; v ] ->
            assert (ver = None) ;
            let version = int_of_sexp v in
            (Some version, jid, prio, host, port, pass, auth, otr_cfg)
          | Sexp.List [ Sexp.Atom "jid" ; Sexp.Atom v ] ->
            assert (jid = None) ;
            let jid =
              try JID.of_string v
              with _ -> raise (Invalid_argument "parse error in jid")
            in
            (ver, Some jid, prio, host, port, pass, auth, otr_cfg)
          | Sexp.List [ Sexp.Atom "priority" ; p ] ->
            assert (prio = None) ;
            let prio = option_of_sexp int_of_sexp p
            in
            (ver, jid, Some prio, host, port, pass, auth, otr_cfg)
          | Sexp.List [ Sexp.Atom "hostname" ; h ] ->
            assert (host = None) ;
            let host = option_of_sexp string_of_sexp h in
            (ver, jid, prio, Some host, port, pass, auth, otr_cfg)
          | Sexp.List [ Sexp.Atom "port" ; p ] ->
            assert (port = None) ;
            let port =
              match ver with
              | Some x when x < 3 ->
                let p = int_of_sexp p in
                if p = 5222 then None else Some p
              | _ -> option_of_sexp int_of_sexp p
            in
            (ver, jid, prio, host, Some port, pass, auth, otr_cfg)
          | Sexp.List [ Sexp.Atom "password" ; password ] ->
            assert (pass = None) ;
            let pass = match ver with
              | Some x when x < 3 -> string_of_sexp password
              | _ -> raise (Invalid_argument "password stored in V3+ file")
            in
            (ver, jid, prio, host, port, Some pass, auth, otr_cfg)
          | Sexp.List [ Sexp.Atom "trust_anchor" ; trust_anchor ] ->
            assert (auth = None) ;
            (match ver with
             | Some 0 ->
               let auth = Some (`Trust_anchor (string_of_sexp trust_anchor)) in
               (ver, jid, prio, host, port, pass, auth, otr_cfg)
             | Some 1 -> ( match option_of_sexp string_of_sexp trust_anchor with
                 | None -> (ver, jid, prio, host, port, pass, auth, otr_cfg)
                 | Some x -> (ver, jid, prio, host, port, pass, Some (`Trust_anchor x), otr_cfg) )
             | _ -> raise (Invalid_argument "unexpected element: trust_anchor") )
          | Sexp.List [ Sexp.Atom "tls_fingerprint" ; tls_fp ] ->
            assert (auth = None) ;
            ( match ver with
              | Some 1 ->
                ( match option_of_sexp string_of_sexp tls_fp with
                  | None -> (ver, jid, prio, host, port, pass, auth, otr_cfg)
                  | Some x -> (ver, jid, prio, host, port, pass, Some (`Fingerprint x), otr_cfg) )
              | _ -> raise (Invalid_argument "unexpected element: tls_fingerprint") )
          | Sexp.List [ Sexp.Atom "authenticator" ; authenticator ] ->
            assert (auth = None) ;
            (ver, jid, prio, host, port, pass, Some (auth_of_sexp authenticator), otr_cfg)
          | Sexp.List [ Sexp.Atom "otr_config" ; v ] ->
            assert (otr_cfg = None) ;
            (ver, jid, prio, host, port, pass, auth, Some (Otr.State.config_no_dsa_of_sexp dsa v) )
          | _ -> assert false)
        (None, None, None, None, None, None, None, None) l
     with
     | Some version, Some jid, Some priority, Some hostname, Some port, password, Some authenticator, Some otr_config ->
       { version ; jid ; priority ; hostname ; port ; password ; authenticator ; otr_config }
     | Some version, Some jid, None, Some hostname, Some port, password, Some authenticator, Some otr_config ->
       { version ; jid ; priority = None ; hostname ; port ; password ; authenticator ; otr_config }
     | Some version, Some jid, Some priority, None, Some port, password, Some authenticator, Some otr_config ->
       { version ; jid ; priority ; hostname = None ; port ; password ; authenticator ; otr_config }
     | Some version, Some jid, None, None, Some port, password, Some authenticator, Some otr_config ->
       { version ; jid ; priority = None ; hostname = None ; port ; password ; authenticator ; otr_config }
     | _ -> raise (Invalid_argument "broken config") )
  | _ -> raise (Invalid_argument "broken config")

let record kvs =
  Sexp.List List.(map (fun (k, v) -> (Sexp.List [Sexp.Atom k; v])) kvs)

let sexp_of_t t =
  record [
    "version"       , sexp_of_int t.version ;
    "jid"           , sexp_of_string (JID.string_of_jid t.jid) ;
    "hostname"      , sexp_of_option sexp_of_string t.hostname ;
    "port"          , sexp_of_option sexp_of_int t.port ;
    "authenticator" , sexp_of_auth t.authenticator ;
    "otr_config"    , Otr.State.sexp_of_config_no_dsa t.otr_config ;
  ]

let load_config dsa bytes =
  t_of_sexp dsa (Sexp.of_string bytes)

let store_config t =
  Sexp.to_string_hum (sexp_of_t t)
