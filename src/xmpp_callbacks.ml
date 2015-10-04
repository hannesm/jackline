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

open Lwt

type user_data = {
  log                  : User.direction -> string -> unit ;
  locallog             : string -> string -> unit ;
  remove               : Xjid.t -> unit ;
  message              : Xjid.t -> ?timestamp:float -> User.direction -> bool -> string -> unit ;
  user                 : Xjid.t -> User.user ;
  session              : Xjid.t -> User.session ;
  update_user          : User.user -> bool -> unit ;
  update_otr           : Xjid.t -> User.session -> Otr.State.session -> unit ;
  update_presence      : Xjid.t -> User.session -> User.presence -> string option -> int -> unit ;
  update_receipt_state : Xjid.t -> User.receipt_state -> unit ;
  receipt              : Xjid.t -> string -> unit ;
  inc_fp               : Xjid.t -> string -> (User.verification_status * int * bool) ;
  failure              : exn -> unit Lwt.t ;
  group_message        : Xjid.t -> float option -> string option -> string option -> Xep_muc.User.data option -> string option -> unit ;
  group_presence       : Xjid.t -> User.presence -> string option -> Xep_muc.User.data -> unit ;
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
            t.user_data.locallog "disco" (String.concat ", " fs) ;
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
            let Some receipt = ns_receipts in
            if
              List.exists
                (function
                  | Xml.Xmlelement ((_, "feature"), attrs, _) ->
                     Xml.safe_get_attr_value "var" attrs = receipt
                  | _ -> false)
                els
            then
              `Supported
            else
              `Unsupported
         | _ ->  `Unsupported
    in
    match jid_from with
    | None -> fail BadRequest
    | Some x -> match Xjid.string_to_jid x with
                | None -> fail BadRequest
                | Some jid ->
                   t.user_data.update_receipt_state jid receipt ;
                   return_unit
  in
  t.user_data.update_receipt_state jid `Requested ;
  let jid_to = Xjid.jid_to_xmpp_jid jid in
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
     (* this raises and lets the async_exception hook handle things *)
     fail (Invalid_argument "ping timeout")
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

let send_msg t ?(kind=Chat) jid id body failure =
  let x, req = match jid with
    | `Full _ ->
       let session = t.user_data.session jid in
       (match id, session.User.receipt with
        | None, _ -> ([], false)
        | Some _, `Supported -> ([Xml.Xmlelement ((ns_receipts, "request"), [], [])], false)
        | Some _, `Unknown -> ([], true)
        | Some _, `Unsupported
        | Some _, `Requested -> ([], false))
    | `Bare _ -> ([], false)
  in
  let jid_to = Xjid.jid_to_xmpp_jid jid in
  (try_lwt
     send_message t ~kind ~jid_to ~body ~x ?id ()
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

let delayed_timestamp = function
  | None -> None
  | Some delay ->
     match Ptime.of_rfc3339 delay.delay_stamp with
     | Rresult.Ok (time, tz) -> Some (float_of_int tz +. Ptime.to_float_s time)
     | Rresult.Error _ -> None

let process_receipt cb = function
  | Xml.Xmlelement ((ns_rec, "received"), attrs, _) when ns_rec = ns_receipts ->
     (match Xml.safe_get_attr_value "id" attrs with
      | "" -> ()
      | id -> cb id)
  | _ -> ()

let answer_receipt jid_to t stanza_id = function
  | Xml.Xmlelement ((ns_rec, "request"), _, _) when ns_rec = ns_receipts ->
     Utils.option
       return_unit
       (fun id ->
        let x =
          let qname = (ns_receipts, "received") in
          [Xml.Xmlelement (qname, [Xml.make_attr "id" id], [])]
        in
        (try_lwt
           send_message t ?jid_to ~kind:Chat ~x ()
         with _ -> return_unit))
       stanza_id
  | _ -> return_unit

let notify_user msg jid ctx inc_fp = function
  | `Established_encrypted_session ssid ->
     msg (`Local (jid, "OTR")) false ("encrypted connection established (ssid " ^ ssid ^ ")") ;
     let raw_fp = match User.otr_fingerprint ctx with Some fp -> fp | _ -> assert false in
     let verify = "verify /fingerprint [fp] over second channel"
     and used n = "(used " ^ (string_of_int n) ^ " times)"
     in
     let otrmsg =
       match inc_fp jid raw_fp with
       | `Verified, _, _ -> "verified OTR key"
       | `Unverified, 0, true -> "POSSIBLE BREAKIN ATTEMPT! new unverified key with a different verified key on disk! " ^ verify
       | `Unverified, n, true -> "unverified key " ^ (used n) ^ " with a different verified key on disk! please " ^ verify
       | `Unverified, 0, false -> "new unverified key! please " ^ verify
       | `Unverified, n, false -> "unverified key " ^ (used n) ^ ". please " ^ verify
       | `Revoked, 0, false -> "REVOKED key (never used before)"
       | `Revoked, n, false -> "REVOKED key " ^ (used n)
       | `Revoked, 0, true -> "REVOKED key (never used before), but a verified is available"
       | `Revoked, n, true -> "REVOKED key " ^ (used n) ^ ", but a verified is available"
     in
     msg (`Local (jid, "OTR key")) false otrmsg
  | `Warning w               -> msg (`Local (jid, "OTR warning")) false w
  | `Received_error e        -> msg (`From jid) false e
  | `Received m              -> msg (`From jid) false m
  | `Received_encrypted e    -> msg (`From jid) true e
  | `SMP_awaiting_secret     -> msg (`Local (jid, "SMP")) false "awaiting SMP secret, answer with /smp answer [secret]"
  | `SMP_received_question q -> msg (`Local (jid, "SMP")) false ("received SMP question (answer with /smp answer [secret]) " ^ q)
  | `SMP_success             -> msg (`Local (jid, "OTR SMP")) false "successfully verified!"
  | `SMP_failure             -> msg (`Local (jid, "OTR SMP")) false "failure"

let maybe_element qname x =
  try Some (Xml.get_element qname x)
  with Not_found -> None

let message_callback (t : user_data session_data) stanza =
  restart_keepalive t ;
  match stanza.jid_from with
  | None -> t.user_data.locallog "error" "no from in stanza" ; return_unit
  | Some jidt ->
     let jid = Xjid.xmpp_jid_to_jid jidt in
     match stanza.content.message_type with
     | Some Groupchat ->
        (* XXX separate private messages somewhere in here *)
        let timestamp = delayed_timestamp stanza.content.message_delay
        and data =
          Utils.option
            None
            (fun e -> Some (Xep_muc.User.decode e))
            (maybe_element (Xep_muc.ns_muc_user, "x") stanza.x)
        and id = stanza.id in
        t.user_data.group_message jid timestamp stanza.content.subject stanza.content.body data id ;
        Lwt.return_unit
     | _ ->
        let msg ?timestamp dir enc txt =
          let data =
            if enc then
              let txt = validate_utf8 txt in
              let txt = Escape.strip_tags txt in
              Escape.unescape txt
            else
              txt
          in
          t.user_data.message jid ?timestamp dir enc data ;
        in
        List.iter (process_receipt (t.user_data.receipt jid)) stanza.x ;
        match
          stanza.content.body,
          delayed_timestamp stanza.content.message_delay,
          jid
        with
        | None, _, _ -> return_unit
        | Some v, timestamp, `Bare _ ->
           (* this is likely a message from the jabber server itself *)
           msg ?timestamp (`From jid) false v ;
           return_unit
        | Some v, Some timestamp, `Full _ ->
           (* some delayed message (offline storage), don't send out receipt / OTR thingies *)
           let session = t.user_data.session jid in
           let ctx, _, ret = Otr.Engine.handle session.User.otr v in
           t.user_data.update_otr jid session ctx ;
           List.iter (notify_user (msg ~timestamp) jid ctx t.user_data.inc_fp) ret ;
           return_unit
        | Some v, None, `Full _ ->
           let session = t.user_data.session jid in
           let ctx, out, ret = Otr.Engine.handle session.User.otr v in
           t.user_data.update_otr jid session ctx ;
           List.iter (notify_user msg jid ctx t.user_data.inc_fp) ret ;
           let jid_to = stanza.jid_from in
           Lwt_list.iter_s (answer_receipt jid_to t stanza.id) stanza.x >>= fun () ->
           match out with
           | None -> return_unit
           | Some body ->
              (try_lwt
                 send_message t ?jid_to ~kind:Chat ~body ()
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
    | Some x -> Xjid.xmpp_jid_to_jid x
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
  (match stanza.jid_from with
   | None     -> t.user_data.locallog "error" "presence received without sending jid, ignoring"
   | Some jidt ->
     let jid = Xjid.xmpp_jid_to_jid jidt in
     let status, statstring = match stanza.content.status with
       | None -> (None, "")
       | Some x when x = "" -> (None, "")
       | Some x -> let data = validate_utf8 x in (Some data, " - " ^ data)
     in
     match maybe_element (Xep_muc.ns_muc_user, "x") stanza.x with
     | Some el ->
        let pres =
          match stanza.content.presence_type with
          | Some Unavailable -> `Offline
          | None -> xmpp_to_presence stanza.content.show
        and data = Xep_muc.User.decode el
        in
        t.user_data.group_presence jid pres status data
     | None ->
        let handle_presence presence =
          let ppart old =
            let presence_char = User.presence_to_char presence
            and presence_string = User.presence_to_string presence
            in
            "[" ^ old ^ ">" ^ presence_char ^ "] (now " ^ presence_string ^ ")" ^ statstring
          in
          match jid with
          | `Bare _ ->
             let info = "presence " ^ ppart "_" in
             t.user_data.log (`From jid) info
          | `Full _ ->
             let session = t.user_data.session jid in
             let priority = match stanza.content.priority with
               | None -> 0
               | Some x -> x
             in
             if User.presence_unmodified session presence status priority then
               ()
             else
               let old = User.presence_to_char session.User.presence in
               t.user_data.update_presence jid session presence status priority ;
               let info = "presence changed: " ^ ppart old in
               t.user_data.log (`From jid) info
        in
        let handle_subscription txt hlp =
          t.user_data.message jid (`Local (jid, txt)) false hlp
        in
        match stanza.content.presence_type with
        | None              -> handle_presence (xmpp_to_presence stanza.content.show)
        | Some Unavailable  -> handle_presence `Offline
        | Some Probe        -> handle_subscription "probed" statstring
        | Some Subscribe    -> handle_subscription "subscription request" ("(use /authorization allow cancel) to accept/deny" ^ statstring)
        | Some Subscribed   -> handle_subscription "you are now subscribed to their presence" statstring
        | Some Unsubscribe  -> handle_subscription "wants to unsubscribe from your presence" ("(use /authorization cancel allow) to accept/deny" ^ statstring)
        | Some Unsubscribed -> handle_subscription "you have been unsubscribed from their presence" statstring
  ) ;
  return_unit

let presence_error t ?id ?jid_from ?jid_to ?lang error =
  restart_keepalive t ;
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
    let jid = Xjid.xmpp_jid_to_jid item.Roster.jid in
    let user = user jid in
    Some { user with User.name = name ;
                     User.groups = groups ;
                     User.subscription = subscription ;
                     User.properties = properties }
  with
  _ -> None

let session_callback (kind, show, status, priority) mvar t =
  let err txt = t.user_data.locallog "handling error" txt in

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

  (try_lwt send_presence t ?kind ?show ?status ?priority ()
   with e -> t.user_data.failure e) >>= fun () ->

  restart_keepalive t ;
  mvar t

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

let connect ?out sockaddr myjid certname password presence authenticator user_data mvar =
  debug_out := out ;

  PlainSocket.open_connection sockaddr >>= fun socket_data ->
  let module Socket_module =
    struct type t = PlainSocket.socket
           let socket = socket_data
           include PlainSocket
    end
  in

  let make_tls () =
    TLSSocket.switch (PlainSocket.get_fd socket_data) certname authenticator >>= fun socket_data ->
    user_data.locallog "TLS session info" (tls_epoch_to_line socket_data) ;
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
    ~tls_socket:make_tls
    ~password
    (session_callback presence mvar)

let close session_data =
  let module S = (val session_data.socket : Socket) in
  S.close S.socket

let parse_loop session_data =
  XMPPClient.parse session_data >>= fun () ->
  close session_data
