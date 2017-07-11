
open Sexplib
open Sexplib.Conv

type auth = [
  | `Fingerprint of string
  | `Trust_anchor of string
] [@@deriving sexp]

type t = {
  version : int ;
  jid : Xjid.full_jid ;
  priority : int option ;
  hostname : string option ;
  port : int option ;
  password : string option ;
  authenticator : auth ;
  otr_config : Otr.State.config ;
  dsa : Nocrypto.Dsa.priv ;
  certificate_hostname : string option ;
  notification_callback : string option ;
  log_top : bool ;
}

let current_version = 3

let dsa_of_cfg_sexp t =
  match t with
  | Sexp.List l ->
    (List.fold_left (fun (dsa, cfg) -> function
         | Sexp.List [ Sexp.Atom "otr_config" ; Sexp.List data ] ->
           List.fold_left (fun (dsa, cfg) -> function
               | Sexp.List [ Sexp.Atom "policies" ; _ ] as p -> (dsa, p :: cfg)
               | Sexp.List [ Sexp.Atom "versions" ; _ ] as v -> (dsa, v :: cfg)
               | Sexp.List [ Sexp.Atom "dsa" ; d ] -> (Some (Nocrypto.Dsa.priv_of_sexp d), cfg)
               | _ -> raise (Invalid_argument "broken sexp while trying to find dsa"))
             (dsa, cfg) data
         | _ -> (dsa, cfg))
        (None, []) l)
  | _ -> raise (Invalid_argument "couldn't parse sexp (dsa)")

let t_of_sexp dsa t =
  let dsa, otr_cfg = match dsa, dsa_of_cfg_sexp t with
    | None, (Some dsa, cfg) -> dsa, cfg
    | Some x, (_, cfg) -> x, cfg
    | _ -> raise (Invalid_argument "broken config")
  in
  let otr_config = Otr.State.config_of_sexp (Sexp.List otr_cfg) in
  match t with
  | Sexp.List l ->
    (match
      List.fold_left (fun (ver, jid, prio, host, port, pass, auth, cert, notification, log_top) -> function
          | Sexp.List [ Sexp.Atom "version" ; v ] ->
            assert (ver = None) ;
            let version = int_of_sexp v in
            (Some version, jid, prio, host, port, pass, auth, cert, notification, log_top)
          | Sexp.List [ Sexp.Atom "jid" ; Sexp.Atom v ] ->
            assert (jid = None) ;
            let jid =
              match Xjid.string_to_full_jid v with
              | None -> raise (Invalid_argument "parse error in jid")
              | Some x -> x
            in
            (ver, Some jid, prio, host, port, pass, auth, cert, notification, log_top)
          | Sexp.List [ Sexp.Atom "priority" ; p ] ->
            assert (prio = None) ;
            let prio = option_of_sexp int_of_sexp p
            in
            (ver, jid, Some prio, host, port, pass, auth, cert, notification, log_top)
          | Sexp.List [ Sexp.Atom "hostname" ; h ] ->
            assert (host = None) ;
            let host = option_of_sexp string_of_sexp h in
            (ver, jid, prio, Some host, port, pass, auth, cert, notification, log_top)
          | Sexp.List [ Sexp.Atom "port" ; p ] ->
            assert (port = None) ;
            let port =
              match ver with
              | Some x when x < 3 ->
                let p = int_of_sexp p in
                if p = 5222 then None else Some p
              | _ -> option_of_sexp int_of_sexp p
            in
            (ver, jid, prio, host, Some port, pass, auth, cert, notification, log_top)
          | Sexp.List [ Sexp.Atom "password" ; password ] ->
            assert (pass = None) ;
            let pass = match ver with
              | Some x when x < 3 -> string_of_sexp password
              | _ -> raise (Invalid_argument "password stored in V3+ file")
            in
            (ver, jid, prio, host, port, Some pass, auth, cert, notification, log_top)
          | Sexp.List [ Sexp.Atom "trust_anchor" ; trust_anchor ] ->
            assert (auth = None) ;
            (match ver with
             | Some 0 ->
               let auth = Some (`Trust_anchor (string_of_sexp trust_anchor)) in
               (ver, jid, prio, host, port, pass, auth, cert, notification, log_top)
             | Some 1 -> ( match option_of_sexp string_of_sexp trust_anchor with
                 | None -> (ver, jid, prio, host, port, pass, auth, cert, notification, log_top)
                 | Some x -> (ver, jid, prio, host, port, pass, Some (`Trust_anchor x), cert, notification, log_top) )
             | _ -> raise (Invalid_argument "unexpected element: trust_anchor") )
          | Sexp.List [ Sexp.Atom "tls_fingerprint" ; tls_fp ] ->
            assert (auth = None) ;
            ( match ver with
              | Some 1 ->
                ( match option_of_sexp string_of_sexp tls_fp with
                  | None -> (ver, jid, prio, host, port, pass, auth, cert, notification, log_top)
                  | Some x -> (ver, jid, prio, host, port, pass, Some (`Fingerprint x), cert, notification, log_top) )
              | _ -> raise (Invalid_argument "unexpected element: tls_fingerprint") )
          | Sexp.List [ Sexp.Atom "authenticator" ; authenticator ] ->
            assert (auth = None) ;
            (ver, jid, prio, host, port, pass, Some (auth_of_sexp authenticator), cert, notification, log_top)
          | Sexp.List [ Sexp.Atom "otr_config" ; _ ] ->
            (ver, jid, prio, host, port, pass, auth, cert, notification, log_top)
          | Sexp.List [ Sexp.Atom "certificate_hostname" ; c ] ->
            assert (cert = None) ;
            let cert = option_of_sexp string_of_sexp c in
            (ver, jid, prio, host, port, pass, auth, cert, notification, log_top)
          | Sexp.List [ Sexp.Atom "notification_callback" ; c ] ->
            assert (notification = None) ;
            let notification = option_of_sexp string_of_sexp c in
            (ver, jid, prio, host, port, pass, auth, cert, notification, log_top)
          | Sexp.List [ Sexp.Atom "log_top" ; c ] ->
            let log_top = bool_of_sexp c in
            (ver, jid, prio, host, port, pass, auth, cert, notification, log_top)
          | _ -> assert false)
        (None, None, None, None, None, None, None, None, None, false) l
     with
     | Some version, Some jid, Some priority, Some hostname, Some port, password, Some authenticator, certificate_hostname, notification_callback, log_top ->
       { version ; jid ; priority ; hostname ; port ; password ; authenticator ; otr_config ; dsa ; certificate_hostname ; notification_callback ; log_top }
     | Some version, Some jid, None, Some hostname, Some port, password, Some authenticator, certificate_hostname, notification_callback, log_top ->
       { version ; jid ; priority = None ; hostname ; port ; password ; authenticator ; otr_config ; dsa ; certificate_hostname ; notification_callback ; log_top }
     | Some version, Some jid, Some priority, None, Some port, password, Some authenticator, certificate_hostname, notification_callback, log_top ->
       { version ; jid ; priority ; hostname = None ; port ; password ; authenticator ; otr_config ; dsa ; certificate_hostname ; notification_callback ; log_top }
     | Some version, Some jid, None, None, Some port, password, Some authenticator, certificate_hostname, notification_callback, log_top ->
       { version ; jid ; priority = None ; hostname = None ; port ; password ; authenticator ; otr_config ; dsa ; certificate_hostname ; notification_callback ; log_top }
     | _ -> raise (Invalid_argument "broken config") )
  | _ -> raise (Invalid_argument "broken config")

let record kvs =
  Sexp.List List.(map (fun (k, v) -> (Sexp.List [Sexp.Atom k; v])) kvs)

let sexp_of_t t =
  record [
    "version"              , sexp_of_int t.version ;
    "jid"                  , sexp_of_string (Xjid.full_jid_to_string t.jid) ;
    "hostname"             , sexp_of_option sexp_of_string t.hostname ;
    "port"                 , sexp_of_option sexp_of_int t.port ;
    "priority"             , sexp_of_option sexp_of_int t.priority ;
    "authenticator"        , sexp_of_auth t.authenticator ;
    "otr_config"           , Otr.State.sexp_of_config t.otr_config ;
    "certificate_hostname" , sexp_of_option sexp_of_string t.certificate_hostname ;
    "notification_callback", sexp_of_option sexp_of_string t.notification_callback ;
    "log_top"              , sexp_of_bool t.log_top ;
  ]

let load_config dsa bytes =
  t_of_sexp dsa (Sexp.of_string bytes)

let store_config t =
  Bytes.of_string (Sexp.to_string_hum (sexp_of_t t))
