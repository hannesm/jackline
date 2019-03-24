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

let stanza_error_to_str se =
  String.concat " "
    [ string_of_error_type se.err_type ;
      string_of_condition se.err_condition ;
      se.err_text ;
      se.err_lang ]

let presence_to_xmpp = function
  | `Offline      -> (Some Unavailable, None)
  | `Online       -> (None, None)
  | `Free         -> (None, Some ShowChat)
  | `Away         -> (None, Some ShowAway)
  | `DoNotDisturb -> (None, Some ShowDND)
  | `ExtendedAway -> (None, Some ShowXA)

let xmpp_to_presence = function
  | None          -> `Online
  | Some ShowChat -> `Free
  | Some ShowAway -> `Away
  | Some ShowDND  -> `DoNotDisturb
  | Some ShowXA   -> `ExtendedAway


module Version = XEP_version.Make (XMPPClient)
module Disco = XEP_disco.Make (XMPPClient)
module Roster = Roster.Make (XMPPClient)
module Xep_muc = XEP_muc.Make (XMPPClient)

open Lwt.Infix

type user_data = {
  log                  : ?kind:User.chatkind -> User.direction -> string -> unit Lwt.t ;
  locallog             : ?kind:User.chatkind -> string -> string -> unit Lwt.t ;

  message              : Xjid.bare_jid -> string option -> ?timestamp:Ptime.t -> string -> unit Lwt.t ;
  group_message        : Xjid.t -> Ptime.t option -> string option -> string option -> Xep_muc.User.data option -> string option -> unit Lwt.t ;

  received_receipts    : Xjid.t -> string list -> unit Lwt.t ;
  update_receipt_state : Xjid.t -> User.receipt_state -> unit Lwt.t ;

  subscription         : Xjid.t -> User.subscription_mod -> string option -> unit Lwt.t ;
  presence             : Xjid.t -> User.presence -> int -> string option -> unit Lwt.t ;
  group_presence       : Xjid.t -> User.presence -> string option -> Xep_muc.User.data -> unit Lwt.t ;

  create_room          : Xjid.bare_jid -> bool -> unit Lwt.t ;

  (* this is to initialise all subscription information to `None before fetching the roster --
     this needs to be done, since a roster get will not return those not in your roster (and might have been modified by different client) *)
  reset_users          : unit -> unit Lwt.t ;
  update_users         : (Xjid.t * string option * string list * User.subscription * User.property list) list -> bool -> unit Lwt.t ;
}

let request_disco t jid =
  let callback ev jid_from _jid_to _lang () =
    let receipt = match ev with
      | IQError _ -> `Unsupported
      | IQResult el ->
        match el with
        | Some (Xml.Xmlelement ((ns, "query"), _, els)) when ns = Disco.ns_disco_info ->
          (* pick el with ns_receipts *)
          let receipt = match ns_receipts with None -> assert false | Some x -> x in
          let tst = function
            | Xml.Xmlelement ((_, "feature"), attrs, _) ->
              Xml.safe_get_attr_value "var" attrs = receipt
            | _ -> false
          in
          if List.exists tst els then `Supported else `Unsupported
        | _ ->  `Unsupported
    in
    match jid_from with
    | None -> fail BadRequest
    | Some x ->
      match Xjid.string_to_jid x with
      | None -> fail BadRequest
      | Some jid -> t.user_data.update_receipt_state jid receipt
  in
  t.user_data.update_receipt_state jid `Requested >>= fun () ->
  let jid_to = Xjid.jid_to_xmpp_jid jid in
  make_iq_request t ~jid_to (IQGet (Disco.make_disco_query [])) callback

module Keepalive = struct
  let ping_urn = "urn:xmpp:ping"

  let keepalive_running : bool ref = ref false

  let keepalive_ping t =
    let callback _ev _jid_from _jid_to _lang () =
      keepalive_running := false ;
      Lwt.return_unit
    in
    if !keepalive_running then
      (* this raises and lets the async_exception hook handle things *)
      fail (Invalid_argument "ping timeout")
    else
      let jid_to = JID.of_string (t.myjid.JID.ldomain) in
      keepalive_running := true ;
      make_iq_request t ~jid_to (IQGet (Xml.make_element (Some ping_urn, "ping") [] [])) callback

  let keepalive : Lwt_engine.event option ref = ref None

  let cancel_keepalive () =
    match !keepalive with
    | None -> ()
    | Some x -> Lwt_engine.stop_event x ; keepalive := None

  let rec restart_keepalive t =
    cancel_keepalive () ;
    let doit () =
      keepalive_ping t >|= fun () ->
      restart_keepalive t
    in
    keepalive := Some (Lwt_engine.on_timer 45. false (fun _ -> Lwt.async doit))
end

let send_msg t ?(kind=Chat) jid receipt id body =
  let x =
    match receipt, id with
    | true, Some _ -> [Xml.Xmlelement ((ns_receipts, "request"), [], [])]
    | _ -> []
  in
  let jid_to = Xjid.jid_to_xmpp_jid jid in
  send_message t ~kind ~jid_to ~body ~x ?id ()

let delayed_timestamp = function
  | None -> None
  | Some delay ->
    match Ptime.of_rfc3339 delay.delay_stamp with
    | Result.Ok (time, _, _) -> Some time
    | Result.Error _ -> None (* XXX: report properly *)

let receipt_id = function
  | Xml.Xmlelement ((ns_rec, "received"), attrs, _) when ns_rec = ns_receipts ->
    (match Xml.safe_get_attr_value "id" attrs with
     | "" -> []
     | id -> [id])
  | _ -> []

let answer_receipt stanza_id = function
  | Xml.Xmlelement ((ns_rec, "request"), _, _) when ns_rec = ns_receipts ->
    let qname = (ns_receipts, "received") in
    [Xml.Xmlelement (qname, [Xml.make_attr "id" stanza_id], [])]
  | _ -> []

let maybe_element qname x =
  try Some (Xml.get_element qname x)
  with Not_found -> None

let message_callback (t : user_data session_data) stanza =
  Keepalive.restart_keepalive t ;
  match stanza.jid_from with
  | None -> t.user_data.locallog "error" "no from in stanza"
  | Some jidt ->
     let jid = Xjid.xmpp_jid_to_jid jidt in
     match stanza.content.message_type with
     | Some Groupchat ->
        let timestamp = delayed_timestamp stanza.content.message_delay
        and data =
          match maybe_element (Xep_muc.ns_muc_user, "x") stanza.x with
          | None -> None
          | Some e -> Some (Xep_muc.User.decode e)
        and id = stanza.id
        in
        t.user_data.group_message jid timestamp stanza.content.subject stanza.content.body data id
     | _ ->
       let receipts = List.flatten (List.map receipt_id stanza.x) in
       t.user_data.received_receipts jid receipts >>= fun () ->
       match
         stanza.content.body,
         delayed_timestamp stanza.content.message_delay,
         jid
       with
       | None, _, _ -> Lwt.return_unit
       | Some v, timestamp, `Bare b ->
         (* this is likely a message from the jabber server itself *)
         t.user_data.message b None ?timestamp v
       | Some v, Some timestamp, `Full (bare, r) ->
         (* some delayed message (offline storage), don't send out receipt / OTR thingies *)
         t.user_data.message bare (Some r) ~timestamp v
       | Some v, None, `Full (bare, r) ->
         t.user_data.message bare (Some r) v >>= fun () ->
         let answer_receipts =
           match stanza.id with
           | None -> []
           | Some id -> List.flatten (List.map (answer_receipt id) stanza.x)
         in
         let jid_to = stanza.jid_from in
         Lwt_list.iter_s
           (fun x -> send_message t ?jid_to ~kind:Chat ~x:[x] ())
           answer_receipts

let message_error t ?id ?jid_from ?jid_to ?lang error =
  Keepalive.restart_keepalive t ;
  ignore id ; ignore jid_to ; ignore lang ;
  let jid = match jid_from with
    | None -> `Bare ("unknown", "host")
    | Some x -> Xjid.xmpp_jid_to_jid x
  in
  let msg =
    let con = "error; reason: " ^ (string_of_condition error.err_condition) in
    match error.err_text with
    | x when x = "" -> con
    | x -> con ^ ", message: " ^ x
  in
  t.user_data.log (`From jid) msg

let presence_callback t stanza =
  Keepalive.restart_keepalive t ;
  match stanza.jid_from with
   | None     -> t.user_data.locallog "error" "presence received without sending jid, ignoring"
   | Some jidt ->
     let jid = Xjid.xmpp_jid_to_jid jidt
     and status = match stanza.content.status with
       | None -> None
       | Some x when x = "" -> None
       | Some x -> Some x
     in
     match maybe_element (Xep_muc.ns_muc_user, "x") stanza.x with
     | Some el ->
       let pres =
         match stanza.content.presence_type with
         | Some Unavailable -> `Offline
         | None -> xmpp_to_presence stanza.content.show
         | _ -> assert false (* XXX: really? *)
       and data = Xep_muc.User.decode el
       in
       t.user_data.group_presence jid pres status data
     | None ->
       let priority = match stanza.content.priority with
         | None -> 0
         | Some x -> x
       and to_u = function
         | Unavailable -> assert false
         | Probe -> `Probe
         | Subscribe -> `Subscribe
         | Subscribed -> `Subscribed
         | Unsubscribe -> `Unsubscribe
         | Unsubscribed -> `Unsubscribed
       in
       match stanza.content.presence_type with
       | None             -> t.user_data.presence jid (xmpp_to_presence stanza.content.show) priority status
       | Some Unavailable -> t.user_data.presence jid `Offline priority status
       | Some x           -> t.user_data.subscription jid (to_u x) status

let presence_error t ?id ?jid_from ?jid_to ?lang error =
  Keepalive.restart_keepalive t ;
  ignore id ; ignore jid_to ; ignore lang ;
  let jid = match jid_from with
    | None -> `Bare ("unknown", "host")
    | Some x -> Xjid.xmpp_jid_to_jid x
  in
  let msg =
    let con = "presence error; reason: " ^ (string_of_condition error.err_condition) in
    match error.err_text with
    | x when x = "" -> con
    | x -> con ^ ", message: " ^ x
  in
  t.user_data.log (`From jid) msg

let roster_callback item =
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
    let jid = Xjid.xmpp_jid_to_jid item.Roster.jid in
    Some (jid, name, groups, subscription, properties)
  with
    _ -> None (* XXX: handle error! *)

let session_callback (kind, show, status, priority) mvar t =
  let err txt = t.user_data.locallog "handling error" txt in

  register_iq_request_handler t Roster.ns_roster
    (fun ev jid_from jid_to _lang () ->
       match ev with
       | IQGet _el -> fail BadRequest
       | IQSet el ->
         (match jid_from, jid_to with
          | None, _ -> Lwt.return_unit
          | Some x, Some y ->
            (try
               let from_jid = JID.of_string x
               and to_jid   = JID.of_string y
               in
               if JID.is_bare from_jid && JID.equal (JID.bare_jid to_jid) from_jid then
                 Lwt.return_unit
               else
                 fail BadRequest
             with _ -> fail BadRequest)
          | _ -> fail BadRequest) >>= fun () ->
         match el with
         | Xml.Xmlelement ((ns_roster, "query"), attrs, els) when ns_roster = Roster.ns_roster ->
           begin match snd (Roster.decode attrs els) with
             | [ item ] ->
               let users = match roster_callback item with
                 | None -> []
                 | Some x -> [x]
               in
               t.user_data.update_users users true >|= fun () ->
               IQResult None
             | _ -> fail BadRequest
           end
         | _ -> fail BadRequest) ;

  register_stanza_handler t (ns_client, "message")
    (fun t attrs eles ->
       (try
          parse_message
            ~callback:message_callback
            ~callback_error:message_error
            t attrs eles
        with _ -> err "during message parsing, ignoring"));

  register_stanza_handler t (ns_client, "presence")
    (fun t attrs eles ->
       (try
          parse_presence
            ~callback:presence_callback
            ~callback_error:presence_error
            t attrs eles
        with _ -> err "during presence parsing, ignoring"));

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
      let mods = List.map roster_callback items in
      t.user_data.reset_users () >>= fun () ->
      let users = List.fold_left (fun acc -> function None -> acc | Some x -> x :: acc) [] mods in
      t.user_data.update_users users false) >>= fun () ->

  send_presence t ?kind ?show ?status ?priority () >>= fun () ->

  Keepalive.restart_keepalive t ;
  mvar t

let tls_epoch_to_line t =
  let open Tls in
  match Tls_lwt.Unix.epoch t with
  | `Ok epoch ->
    let version = epoch.Core.protocol_version
    and cipher = epoch.Core.ciphersuite
    in
    `Ok Sexplib.Sexp.(to_string_hum (List [
        Core.sexp_of_tls_version version ;
        Ciphersuite.sexp_of_ciphersuite cipher ]))
  | `Error -> `Error "error while fetching TLS parameters"

let resolve ~(selflog:?kind:User.chatkind -> string -> string -> unit Lwt.t) (hostname : string option) (port : int option) (jid_idn : string) =
  (* this is a mess, only to resolve a hostname
      - if an IP is provided, don't leak that to getaddrinfo
      - Unix.getaddrinfo and Unix.inet_addr_of_string may throw -> need to catch exceptions
      - Unix.getaddrinfo "xmpp-client" may not work (e.g. on some MacOSX versions), but Unix.getaddrinfo "" does
      - TODO: if not available, use DNS SRV record of jid_idn
      - TODO: use some result type here *)
  let udns_getaddrinfo host : Lwt_unix.inet_addr option Lwt.t =
    match Domain_name.of_string host with
    | Result.Error _ -> Lwt.return None
    | Ok host -> Udns_client_lwt.(
        gethostbyname @@ create
          ~nameserver:(`UDP, (Unix.inet_addr_of_string "91.239.100.100", 53)) ()
      ) host >|= function Result.Error _ -> None |
        Ok ip -> Some (Ipaddr.V4.to_string ip |> Unix.inet_addr_of_string)
  in
  let lwt_getaddrinfo host : Lwt_unix.inet_addr option Lwt.t =
    Lwt.async (fun() ->
        selflog ~kind:`Warning "connecting"
          "DEPRECATION: You are using LD_PRELOAD, but since jackline's recent \
           switch to the pure OCaml implementation for DNS resolution, \
           LD_PRELOAD-based tools like 'torsocks' will not be able to \
           intercept the getaddrinfo() library calls like they used to. For \
           now we will detect the use of LD_PRELOAD and revert to the old \
           method of using libc's resolver, but in the future this will change.") ;
    Lwt.try_bind
      (fun()-> Lwt_unix.getaddrinfo host "" [])
      (function Unix.{ ai_addr = Lwt_unix.ADDR_INET (addr,_) ; _ } :: _ ->
         Lwt.return (Some addr) |
         _ -> Lwt.return None)
      (fun _ -> Lwt.return None)
  in
  let rec resolve ?(service = "xmpp-client") host =
    (if Sys.getenv_opt "LD_PRELOAD" = None then
       udns_getaddrinfo else lwt_getaddrinfo
    ) host >>= function
    | None when service = "" -> Lwt.fail (Invalid_argument ("could not resolve hostname " ^ host))
    | None -> resolve ~service:"" host (* TODO when/how often do we want to retry? *)
    | Some ip -> Lwt.return ip
  in
  (match hostname with
   | Some x -> (try Lwt.return (Unix.inet_addr_of_string x) with _ -> resolve x)
   | None -> resolve jid_idn) >|= fun ip ->
  Lwt_unix.ADDR_INET (ip, match port with Some x -> x | _ -> 5222)

let connect socket_data myjid certname password presence authenticator user_data mvar =
  let module Socket_module =
    struct type t = PlainSocket.socket
           let socket = socket_data
           include PlainSocket
    end
  in

  let tls_socket () =
    TLSSocket.switch socket_data certname authenticator >>= fun socket_data ->
    (match tls_epoch_to_line socket_data with
     | `Ok str -> user_data.locallog ~kind:`Success "TLS session info" str
     | `Error str -> user_data.locallog ~kind:`Error "TLS session info" str) >>= fun () ->
    let module TLS_module =
      struct type t = Tls_lwt.Unix.t
             let socket = socket_data
             include TLSSocket
      end in
    return (module TLS_module : XMPPClient.Socket)
  in

  let myjid = Xjid.jid_to_xmpp_jid (`Full myjid) in
  XMPPClient.setup_session
    ~user_data
    ~myjid
    ~plain_socket:(module Socket_module : XMPPClient.Socket)
    ~tls_socket
    ~password
    (session_callback presence mvar)

let close session_data =
  let module S = (val session_data.socket : Socket) in
  S.close S.socket

let parse_loop session_data =
  XMPPClient.parse session_data >>= fun () ->
  close session_data
