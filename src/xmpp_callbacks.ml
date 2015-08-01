open StanzaError

open Xmpp_connection

module ID =
struct
  type t = string
  let compare = Pervasives.compare
end
module IDCallback = Map.Make(ID)

module XMPPClient = XMPP.Make (Lwt) (Xmlstream.XmlStream) (IDCallback)

open XMPPClient

module Version = XEP_version.Make (XMPPClient)
module Disco = XEP_disco.Make (XMPPClient)
module Roster = Roster.Make (XMPPClient)

open Lwt

type user_data = {
  log            : User.direction -> string -> unit ;
  remove         : User.Jid.t -> unit ;
  message        : User.Jid.t -> User.direction -> bool -> string -> unit ;
  user           : User.Jid.t -> User.user ;
  session        : User.Jid.t -> User.session ;
  update_user    : User.user -> bool -> unit ;
  update_session : User.Jid.t -> User.session -> unit ;
  receipt        : User.Jid.t -> string -> unit ;
  inc_fp         : User.Jid.t -> string -> (bool * int * bool) ;
  failure        : exn -> unit Lwt.t ;
}

(*
let request_server_disco t =
  let callback ev _jid_from _jid_to _lang () =
    match ev with
      | IQResult el ->
        ( match el with
          | Some (Xml.Xmlelement ((ns, "query"), _, els)) when ns = Disco.ns_disco_info ->
            let fs = List.fold_left (fun acc -> function
                | Xml.Xmlelement ((_, "feature"), attrs, _) ->
                  let feature = Xml.safe_get_attr_value "var" attrs in
                  feature :: acc
                | _ -> "bla" :: acc) [] els
            in
            t.user_data.log (`Local "disco") (String.concat ", " fs) ;
            return_unit
          | _ ->  return_unit)
      | IQError _ -> return_unit
  in
  let jid_to = JID.of_string (t.myjid.JID.ldomain) in
  (try_lwt
     make_iq_request t ~jid_to (IQGet (Disco.make_disco_query [])) callback
   with e -> t.user_data.failure e)
*)

let request_disco t jid =
  let callback ev jid_from _jid_to _lang () =
    let receipt = match ev with
      | IQError _ -> `Unsupported
      | IQResult el ->
         match el with
         | Some (Xml.Xmlelement ((ns, "query"), _, els)) when ns = Disco.ns_disco_info ->
            (* pick el with ns_receipts *)
            let receipt = match ns_receipts with None -> assert false | Some x -> x in
            if List.exists (function
                             | Xml.Xmlelement ((_, "feature"), attrs, _) ->
                                Xml.safe_get_attr_value "var" attrs = receipt
                             | _ -> false) els
            then
              `Supported
            else
              `Unsupported
         | _ ->  `Unsupported
      in
      (match jid_from with
        | None -> fail BadRequest
        | Some x -> return x ) >>= fun jid_from ->
      match User.Jid.string_to_jid jid_from with
      | None -> fail BadRequest
      | Some jid ->
         let session = t.user_data.session jid in
         t.user_data.update_session jid { session with User.receipt = receipt } ;
         return_unit
  in
  let session = t.user_data.session jid in
  t.user_data.update_session jid { session with User.receipt = `Requested } ;
  let jid_to = User.Jid.jid_to_xmpp_jid jid in
  (try_lwt
     make_iq_request t ~jid_to (IQGet (Disco.make_disco_query [])) callback
   with e -> t.user_data.failure e)

let ping_urn = "urn:xmpp:ping"

let keepalive_running : bool ref = ref false

let keepalive_ping t =
  let callback _ev _jid_from _jid_to _lang () =
    keepalive_running := false ;
    return_unit
  in
  if !keepalive_running then
    (let module S = (val t.socket : Socket) in
     S.close S.socket >>= fun () ->
     (* this raises and lets the async_exception hook handle things *)
     fail (Invalid_argument "ping timeout"))
  else
    let jid_to = JID.of_string (t.myjid.JID.ldomain) in
    keepalive_running := true ;
    (try_lwt
       make_iq_request t ~jid_to (IQGet (Xml.make_element (Some ping_urn, "ping") [] [])) callback
     with e -> t.user_data.failure e)

let keepalive : Lwt_engine.event option ref = ref None

let cancel_keepalive () =
  match !keepalive with
  | None -> ()
  | Some x ->
    Lwt_engine.stop_event x ;
    keepalive := None

let rec restart_keepalive t =
  cancel_keepalive () ;
  let doit () =
    keepalive_ping t >|= fun () ->
    restart_keepalive t
  in
  keepalive := Some (Lwt_engine.on_timer 45. false (fun _ -> Lwt.async doit))

let send_msg t jid id body failure =
  let x, req = match jid with
    | `Full _ ->
       let session = t.user_data.session jid in
       (match id, session.User.receipt with
        | None, _ -> ([], false)
        | Some _, `Supported -> ([Xml.Xmlelement ((ns_receipts, "request"), [], [])], false)
        | Some _, `Unsupported -> ([], true)
        | Some _, `Unknown
        | Some _, `Requested -> ([], false))
    | `Bare _ -> ([], false)
  in
  let jid_to = User.Jid.jid_to_xmpp_jid jid in
  restart_keepalive t ;
  (try_lwt
     send_message t ~kind:Chat ~jid_to ~body ~x ?id ()
   with e -> failure e) >>= fun () ->
  if req then
    request_disco t jid
  else
    return_unit

let validate_utf8 txt =
  let open CamomileLibrary.UPervasives in
  let check_uchar c = match int_of_uchar c with
    | 0x09 | 0x0a -> Some c (* allow tab (0x09) and newline (0x0a) *)
    | 0x0d -> None (* ignore carriage return *)
    | c when c < 0x20 -> Some (uchar_of_int 0xFFFD)
    (* replace characters < 0x20 with unicode replacement character (0xFFFD) *)
    | _ -> Some c
  in
  try
    Zed_utf8.filter_map check_uchar txt
  with
  | Zed_utf8.Invalid (err, esc) -> err ^ ": " ^ esc

let message_callback (t : user_data session_data) stanza =
  restart_keepalive t ;
  match stanza.jid_from with
  | None -> t.user_data.log (`Local "error") "no from in stanza" ; return_unit
  | Some jidt ->
    let jid = User.Jid.xmpp_jid_to_jid jidt in
    let msg dir enc txt =
      let data =
        let txt = validate_utf8 txt in
        let txt = Escape.strip_tags txt in
        Escape.unescape txt
      in
      t.user_data.message jid dir enc data ;
    in
    List.iter (function
        | Xml.Xmlelement ((ns_rec, "received"), attrs, _) when ns_rec = ns_receipts ->
          (match Xml.safe_get_attr_value "id" attrs with
           | "" -> ()
           | id -> t.user_data.receipt jid id)
        | _ -> ()) stanza.x ;
    match stanza.content.body with
    | None -> return_unit
    | Some v ->
      let session = t.user_data.session jid in
      let ctx, out, ret = Otr.Engine.handle session.User.otr v in
      t.user_data.update_session jid { session with User.otr = ctx } ;
      let from = `From jid in
      List.iter (function
          | `Established_encrypted_session ssid ->
            msg (`Local "OTR") false ("encrypted connection established (ssid " ^ ssid ^ ")") ;
            let raw_fp = match User.otr_fingerprint ctx with Some fp -> fp | _ -> assert false in
            let verify = "verify /fingerprint [fp] over second channel" in
            let otrmsg =
              match t.user_data.inc_fp jid raw_fp with
              | true, _, _ -> "verified OTR key"
              | false, 0, true -> "POSSIBLE BREAKIN ATTEMPT! new unverified key with a different verified key on disk! " ^ verify
              | false, n, true -> "unverified key (used " ^ (string_of_int n) ^ " times) with a different verified key on disk! please " ^ verify
              | false, 0, false -> "new unverified key! please " ^ verify
              | false, n, false -> "unverified key (used " ^ (string_of_int n) ^ " times). please " ^ verify
            in
            msg (`Local "OTR key") false otrmsg
          | `Warning w               -> msg (`Local "OTR warning") false w
          | `Received_error e        -> msg from false e
          | `Received m              -> msg from false m
          | `Received_encrypted e    -> msg from true e
          | `SMP_awaiting_secret     -> msg (`Local "OTR SMP") false "awaiting SMP secret, answer with /smp answer [secret]"
          | `SMP_received_question q -> msg (`Local "OTR SMP") false ("received SMP question (answer with /smp answer [secret]) " ^ q)
          | `SMP_success             -> msg (`Local "OTR SMP") false "successfully verified!"
          | `SMP_failure             -> msg (`Local "OTR SMP") false "failure" )
        ret ;
      (Lwt_list.iter_s (function
          | Xml.Xmlelement ((ns_rec, "request"), _, _) when
              ns_rec = ns_receipts ->
            (match stanza.id with
              | None -> return_unit
              | Some id ->
                let x = [Xml.Xmlelement ((ns_receipts, "received"), [Xml.make_attr "id" id], [])] in
                (try_lwt
                   send_message t
                   ?jid_to:stanza.jid_from
                   ~kind:Chat
                   ~x
                    ()
                 with _ -> return_unit))
          | _ -> return_unit) stanza.x) >>= fun () ->
      match out with
      | None -> return_unit
      | Some body ->
        (try_lwt
           send_message t
             ?jid_to:stanza.jid_from
             ~kind:Chat
             ~body ()
         with e -> t.user_data.failure e) >>= fun () ->
        if (t.user_data.session jid).User.receipt = `Unknown then
          try_lwt request_disco t jid
          with e -> t.user_data.failure e
        else
          return_unit

let message_error t ?id ?jid_from ?jid_to ?lang error =
  restart_keepalive t ;
  ignore id ; ignore jid_to ; ignore lang ;
  let jid = match jid_from with
    | None -> `Bare ("unknown", "host")
    | Some x -> User.Jid.xmpp_jid_to_jid x
  in
  let msg =
    let con = "error; reason: " ^ (string_of_condition error.err_condition) in
    match error.err_text with
    | x when x = "" -> con
    | x -> con ^ ", message: " ^ x
  in
  t.user_data.log (`From jid) msg ;
  return_unit

let presence_callback t stanza =
  restart_keepalive t ;
  let log = t.user_data.log in
  (match stanza.jid_from with
   | None     -> log (`Local "error") "presence received without sending jid, ignoring"
   | Some jidt ->
     let jid = User.Jid.xmpp_jid_to_jid jidt in
     let status, statstring = match stanza.content.status with
       | None -> (None, "")
       | Some x when x = "" -> (None, "")
       | Some x -> let data = validate_utf8 x in (Some data, " - " ^ data)
     in
     let handle_presence presence =
       let session = t.user_data.session jid in
       let priority = match stanza.content.priority with
         | None -> 0
         | Some x -> x
       in
       match
         session.User.presence = presence,
         session.User.status = status
       with
       | true, true -> ()
       | _ ->
          let old = User.presence_to_char session.User.presence in
          let session = { session with User.presence ; status ; priority } in
          t.user_data.update_session jid session ;

          let n = User.presence_to_char presence
          and nl = User.presence_to_string presence
          in
          let info =
            "presence changed: [" ^ old ^ ">" ^ n ^ "] (now " ^ nl ^ ")" ^ statstring
          in
          log (`From jid) info
     in
     let handle_subscription txt hlp =
       t.user_data.message jid (`Local txt) false hlp
     in
     match stanza.content.presence_type with
     | None ->
       begin
         match stanza.content.show with
         | None          -> handle_presence `Online
         | Some ShowChat -> handle_presence `Free
         | Some ShowAway -> handle_presence `Away
         | Some ShowDND  -> handle_presence `DoNotDisturb
         | Some ShowXA   -> handle_presence `ExtendedAway
       end
     | Some Probe        -> handle_subscription "probed" statstring
     | Some Subscribe    -> handle_subscription "subscription request" ("(use /authorization allow cancel) to accept/deny" ^ statstring)
     | Some Subscribed   -> handle_subscription "you successfully subscribed to their presence update" statstring
     | Some Unsubscribe  -> handle_subscription "wants to unsubscribe from your presence" ("(use /authorization cancel allow) to accept/deny" ^ statstring)
     | Some Unsubscribed -> handle_subscription "you have been unsubscribed from their buddy list" statstring
     | Some Unavailable  -> handle_presence `Offline
  ) ;
  return_unit

let presence_error t ?id ?jid_from ?jid_to ?lang error =
  restart_keepalive t ;
  ignore id ; ignore jid_to ; ignore lang ;
  let jid = match jid_from with
    | None -> `Bare ("unknown", "host")
    | Some x -> User.Jid.xmpp_jid_to_jid x
  in
  let msg =
    let con = "presence error; reason: " ^ (string_of_condition error.err_condition) in
    match error.err_text with
    | x when x = "" -> con
    | x -> con ^ ", message: " ^ x
  in
  t.user_data.log (`From jid) msg ;
  return_unit


let roster_callback user item =
  try
    let subscription =
      match item.Roster.subscription with
      | Roster.SubscriptionRemove -> `Remove
      | Roster.SubscriptionBoth   -> `Both
      | Roster.SubscriptionNone   -> `None
      | Roster.SubscriptionFrom   -> `From
      | Roster.SubscriptionTo     -> `To
    in
    let properties =
      let app = if item.Roster.approved then [`PreApproved ] else [] in
      let ask = match item.Roster.ask with | Some _ -> [ `Pending ] | None -> [] in
      app @ ask
    in
    let name = if item.Roster.name = "" then None else Some item.Roster.name in
    let groups = item.Roster.group in
    let jid = User.Jid.xmpp_jid_to_jid item.Roster.jid in
    let user = user jid in
    Some { user with User.name = name ;
                     User.groups = groups ;
                     User.subscription = subscription ;
                     User.properties = properties }
  with
  _ -> None

let session_callback ?priority t =
  let err txt = t.user_data.log (`Local "handling error") txt in

  register_iq_request_handler t Roster.ns_roster
    (fun ev jid_from jid_to lang () ->
       ignore lang ;
       match ev with
       | IQGet _el -> fail BadRequest
       | IQSet el ->
         ( match jid_from, jid_to with
           | None, _        -> return ()
           | Some x, Some y ->
             ( try
                 let from_jid = JID.of_string x
                 and to_jid   = JID.of_string y
                 in
                 if JID.is_bare from_jid && JID.equal (JID.bare_jid to_jid) from_jid then
                   return ()
                 else
                   fail BadRequest
               with _ -> fail BadRequest )
           | _ -> fail BadRequest ) >>= fun () ->
         match el with
         | Xml.Xmlelement ((ns_roster, "query"), attrs, els) when ns_roster = Roster.ns_roster ->
           let _, items = Roster.decode attrs els in
           if List.length items = 1 then
             let mods = List.map (roster_callback t.user_data.user) items in
             List.iter
               (function
                 | None -> ()
                 | Some x when x.User.subscription = `Remove ->
                    let jid = `Bare x.User.bare_jid in
                    t.user_data.log (`From jid) "Removed from roster" ;
                    t.user_data.remove jid
                 | Some x -> t.user_data.update_user x true)
               mods ;
             return (IQResult None)
           else
             fail BadRequest
         | _ -> fail BadRequest ) ;

  register_stanza_handler t (ns_client, "message")
    (fun t attrs eles ->
       (try
          parse_message
            ~callback:message_callback
            ~callback_error:message_error
            t attrs eles
        with _ -> err "during message parsing, ignoring" ; return_unit ));

  register_stanza_handler t (ns_client, "presence")
    (fun t attrs eles ->
       (try
          parse_presence
            ~callback:presence_callback
            ~callback_error:presence_error
            t attrs eles
        with _ -> err "during presence parsing, ignoring" ; return_unit ));

  register_iq_request_handler t Disco.ns_disco_info
    (fun ev _jid_from _jid_to _lang () ->
       match ev with
       | IQSet _el -> fail BadRequest
       | IQGet _ ->
         match ns_receipts with
         | None -> fail BadRequest
         | Some x ->
           let feature = Disco.make_feature_var x in
           let query = Disco.make_disco_query [feature]
           in
           return (IQResult (Some query)) ) ;

  Roster.get t (fun ?jid_from ?jid_to ?lang ?ver items ->
      ignore jid_from ; ignore jid_to ; ignore lang ; ignore ver ;
      let mods = List.map (roster_callback t.user_data.user) items in
      List.iter (function None -> () | Some x -> t.user_data.update_user x false) mods ;
      return () ) >>= fun () ->

  try_lwt send_presence t ?priority ()
  with e -> t.user_data.failure e

let tls_epoch_to_line t =
  let open Tls in
  match Tls_lwt.Unix.epoch t with
  | `Ok epoch ->
    let version = epoch.Core.protocol_version
    and cipher = epoch.Core.ciphersuite
    in
    Sexplib.Sexp.(to_string_hum (List [
        Core.sexp_of_tls_version version ;
        Ciphersuite.sexp_of_ciphersuite cipher ]))
  | `Error -> "error while fetching TLS parameters"

let resolve (hostname : string option) (port : int option) (jid_idn : string) =
  (* resolving logic:
     - prefer user-supplied hostname & port [default to 5222]
     - TODO: if not available, use DNS SRV record of jid_idn (find a lib which does SRV)
     - if not available, use A record of jid_domain (and supplied port / 5222)
  *)
  let open Unix in
  let resolve host =
    match
      try Some (List.hd (getaddrinfo host "xmpp-client"
                           [AI_SOCKTYPE SOCK_STREAM ; AI_FAMILY PF_INET])).ai_addr
      with _ -> None
    with
    | None -> fail (Invalid_argument ("could not resolve hostname " ^ host))
    | Some (ADDR_UNIX _) -> fail (Invalid_argument "received unix address")
    | Some (ADDR_INET (ip, _) as x) -> match port with
      | None -> return x
      | Some p -> return (ADDR_INET (ip, p))
  in
  let to_ipv4 str =
    try (let ip = inet_addr_of_string str in
         match port with
         | Some x -> return (ADDR_INET (ip, x))
         | None -> return (ADDR_INET (ip, 5222)))
    with _ -> resolve str
  in
  match hostname with
  | Some x -> to_ipv4 x
  | None -> resolve jid_idn

let connect ?out sockaddr myjid certname password priority authenticator user_data =
  debug_out := out ;

  let err_log msg = user_data.log (`Local "error") msg
  and info info data = user_data.log (`Local info) data
  in

  (try_lwt PlainSocket.open_connection sockaddr >>= fun s -> return (Some s)
   with _ -> return None) >>= function
   | None -> err_log "failed to connect" ; return None
   | Some socket_data ->
     let module Socket_module = struct type t = PlainSocket.socket
       let socket = socket_data
       include PlainSocket
     end
     in

     let make_tls () =
       TLSSocket.switch (PlainSocket.get_fd socket_data) certname authenticator >>= fun socket_data ->
       info "TLS session info" (tls_epoch_to_line socket_data) ;
       let module TLS_module = struct type t = Tls_lwt.Unix.t
         let socket = socket_data
         include TLSSocket
       end in
       return (module TLS_module : XMPPClient.Socket)
     in

     let myjid = User.Jid.jid_to_xmpp_jid (`Full myjid) in
     XMPPClient.setup_session
       ~user_data
       ~myjid
       ~plain_socket:(module Socket_module : XMPPClient.Socket)
       ~tls_socket:make_tls
       ~password
       (session_callback ?priority) >|= fun session ->
     Some session

let close session_data =
  let module S = (val session_data.socket : Socket) in
  S.close S.socket

let parse_loop session_data =
  XMPPClient.parse session_data >>= fun () ->
  close session_data
