
open Sexplib
open Sexplib.Conv

type auth = [
  | `Fingerprint of string
  | `Trust_anchor of string
] with sexp

type t = {
  version : int ;
  jid : JID.t ;
  port : int ;
  password : string ;
  authenticator : auth ;
  otr_config : Otr.State.config ;
}

let current_version = 2

let t_of_sexp t =
  match t with
  | Sexp.List l ->
    (match
      List.fold_left (fun (ver, jid, port, pass, auth, otr_cfg) v -> match v with
          | Sexp.List [ Sexp.Atom "version" ; v ] ->
            assert (ver = None) ;
            let version = int_of_sexp v in
            (Some version, jid, port, pass, auth, otr_cfg)
          | Sexp.List [ Sexp.Atom "jid" ; Sexp.Atom v ] ->
            assert (jid = None) ;
            let jid =
              try JID.of_string v
              with _ -> raise (Invalid_argument "parse error in jid")
            in
            (ver, Some jid, port, pass, auth, otr_cfg)
          | Sexp.List [ Sexp.Atom "port" ; p ] ->
            assert (port = None) ;
            (ver, jid, Some (int_of_sexp p), pass, auth, otr_cfg)
          | Sexp.List [ Sexp.Atom "password" ; Sexp.Atom password ] ->
            assert (pass = None) ;
            (ver, jid, port, Some password, auth, otr_cfg)
          | Sexp.List [ Sexp.Atom "trust_anchor" ; trust_anchor ] ->
            assert (auth = None) ;
            (match ver with
             | Some 0 ->
               let auth = Some (`Trust_anchor (string_of_sexp trust_anchor)) in
               (ver, jid, port, pass, auth, otr_cfg)
             | Some 1 -> ( match option_of_sexp string_of_sexp trust_anchor with
                 | None -> (ver, jid, port, pass, auth, otr_cfg)
                 | Some x -> (ver, jid, port, pass, Some (`Trust_anchor x), otr_cfg) )
             | _ -> raise (Invalid_argument "unexpected element: trust_anchor") )
          | Sexp.List [ Sexp.Atom "tls_fingerprint" ; tls_fp ] ->
            assert (auth = None) ;
            ( match ver with
              | Some 1 ->
                ( match option_of_sexp string_of_sexp tls_fp with
                  | None -> (ver, jid, port, pass, auth, otr_cfg)
                  | Some x -> (ver, jid, port, pass, Some (`Fingerprint x), otr_cfg) )
              | _ -> raise (Invalid_argument "unexpected element: tls_fingerprint") )
          | Sexp.List [ Sexp.Atom "authenticator" ; authenticator ] ->
            assert (auth = None) ;
            (ver, jid, port, pass, Some (auth_of_sexp authenticator), otr_cfg)
          | Sexp.List [ Sexp.Atom "otr_config" ; v ] ->
            assert (otr_cfg = None) ;
            (ver, jid, port, pass, auth, Some (Otr.State.config_of_sexp v) )
          | _ -> assert false)
        (None, None, None, None, None, None) l
     with
     | Some version, Some jid, Some port, Some password, Some authenticator, Some otr_config ->
       { version ; jid ; port ; password ; authenticator ; otr_config }
     | _ -> raise (Invalid_argument "broken config") )
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
  Sexp.to_string_hum (sexp_of_t t)
