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
module Roster = Roster.Make (XMPPClient)

open Lwt

type user_data = {
  otr_config : Otr.State.config ;
  users      : User.users       ;
  received   : (User.direction * string) -> unit ;
  notify     : bool -> User.user -> unit ;
  message    : JID.t -> User.direction -> bool -> string -> unit ;
  failure    : exn -> unit Lwt.t ;
}

let message_callback (t : user_data session_data) stanza =
  match stanza.jid_from with
  | None -> t.user_data.received ((`Local "error"), "no from in stanza") ; return_unit
  | Some jid ->
    let user = User.find_or_add jid t.user_data.users in
    let session = User.ensure_session
        t.user_data.users user jid `Offline t.user_data.otr_config
    in
    let from = JID.string_of_jid jid in
    let msg dir enc txt =
      t.user_data.message jid dir enc txt
    in
    match stanza.content.body with
    | None ->
      (*    msg (`Local "") false "**empty message**" ; *)
      return_unit
    | Some v ->
      let ctx, out, ret = Otr.Handshake.handle session.User.otr v in
      let user = User.find_or_add jid t.user_data.users in
      User.update_otr t.user_data.users user session ctx ;
      List.iter (function
          | `Established_encrypted_session ssid ->
            msg (`Local "OTR") false ("encrypted connection established (ssid " ^ ssid ^ ")") ;
            ( match User.find_fp user ctx with
              | _, Some fps ->
                let verified_key = List.exists (fun x -> x.User.verified) user.User.otr_fingerprints in
                let verify = "verify /fingerprint [fp] over second channel" in
                let otrmsg =
                  match verified_key, fps.User.verified, fps.User.session_count with
                  | _, true, _ -> "verified OTR key"
                  | true, false, 0 -> "POSSIBLE BREAKIN ATTEMPT! new unverified key with a different verified key on disk! " ^ verify
                  | true, false, n -> "unverified key (used " ^ (string_of_int n) ^ " times) with a different verified key on disk! please " ^ verify
                  | false, false, 0 -> "new unverified key! please " ^ verify
                  | false, false, n -> "unverified key (used " ^ (string_of_int n) ^ " times). please " ^ verify
                in
                msg (`Local "OTR key") false otrmsg ;
                User.increase_fp t.user_data.users jid fps ;
              | _, None ->
                msg (`Local "PROBLEM") false "shouldn't happen - OTR established but couldn't find fingerprint" )
          | `Warning w -> msg (`Local "OTR warning") false w
          | `Received_error e -> msg (`From from) false e
          | `Received m -> msg (`From from) false m
          | `Received_encrypted e -> msg (`From from) true e)
        ret ;
      match out with
      | None -> return ()
      | Some body ->
        try_lwt
          send_message t
            ?jid_to:stanza.jid_from
            ~kind:Chat
            ~body ()
        with e -> t.user_data.failure e

let message_error t ?id ?jid_from ?jid_to ?lang error =
  ignore id ; ignore jid_to ; ignore lang ;
  let jid = match jid_from with
    | None -> "unknown"
    | Some x -> JID.string_of_jid x
  in
  let msg =
    let err = "error message" in
    match error.err_text with
    | x when x = "" -> err
    | x -> err ^ ": " ^ x
  in
  t.user_data.received ((`From jid), msg) ;
  return_unit

let presence_callback t stanza =
  let log = t.user_data.received in
  (match stanza.jid_from with
   | None     -> log ((`Local "error"), "presence received without sending jid, ignoring")
   | Some jid ->
     let user = User.find_or_add jid t.user_data.users in
     let stat, statstring = match stanza.content.status with
       | None -> (None, "")
       | Some x when x = "" -> (None, "")
       | Some x -> (Some x, " - " ^ x)
     in
     let userid, resource = User.bare_jid jid in
     let handle_presence newp () =
       let prio = match stanza.content.priority with
           | None -> 0
           | Some x -> x
       in
       let session = {
         User.resource = resource ;
         User.presence = newp ;
         User.status   = stat ;
         User.priority = prio ;
         User.otr      = Otr.State.new_session t.user_data.otr_config () ;
         User.dispose  = false ;
       } in
       let session, old, similar =
         match User.find_session user resource with
         | Some s -> ({ session with User.otr = s.User.otr }, Some s, None)
         | None -> match User.find_similar_session user resource with
           | Some s -> ({ session with User.otr = s.User.otr }, None, Some s)
           | None -> (session, None, None)
       in

       let user = User.replace_session t.user_data.users user session in
       let old = match old with
         | None   -> "_"
         | Some o -> User.presence_to_char o.User.presence
       in
       User.maybe_dispose t.user_data.users user similar ;

       let n = User.presence_to_char newp in
       let nl = User.presence_to_string newp in
       let info =
         "presence changed: [" ^ old ^ ">" ^ n ^ "] (now " ^ nl ^ ")" ^ statstring
       in
       log (`From (userid ^ "/" ^ resource), info)
     in
     let auth_info txt =
       let hlp = ("(use /authorization [arg])" ^ statstring) in
       t.user_data.message jid (`Local txt) false hlp
     in
     match stanza.content.presence_type with
     | None ->
       begin
         match stanza.content.show with
         | None -> handle_presence `Online ()
         | Some ShowChat -> handle_presence `Free ()
         | Some ShowAway -> handle_presence `Away ()
         | Some ShowDND -> handle_presence `DoNotDisturb ()
         | Some ShowXA -> handle_presence `ExtendedAway ()
       end
     | Some Probe -> auth_info "probed"
     | Some Subscribe -> auth_info "subscription request"
     | Some Subscribed -> auth_info "successfully subscribed"
     | Some Unsubscribe -> auth_info "shouldn't see this unsubscribe"
     | Some Unsubscribed -> auth_info "you're so off my buddy list"
     | Some Unavailable -> handle_presence `Offline ()
  ) ;
  return_unit

let presence_error t ?id ?jid_from ?jid_to ?lang error =
  ignore id ; ignore jid_to ; ignore lang ;
  let jid = match jid_from with
    | None -> "unknown"
    | Some x -> JID.string_of_jid x
  in
  let msg =
    let err = "presence error" in
    match error.err_text with
    | x when x = "" -> err
    | x -> err ^ ": " ^ x
  in
  t.user_data.received ((`From jid), msg) ;
  return_unit


let roster_callback users item =
  try
    let user = User.find_or_add item.Roster.jid users in
    let subscription =
      match item.Roster.subscription with
      | Roster.SubscriptionRemove -> assert false
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
    let t = { user with
              User.name = name ;
              User.groups = item.Roster.group ;
              subscription ; properties }
    in
    Some t
  with
  _ -> None

let session_callback t =
  let err txt = t.user_data.received ((`Local "handling error"), txt)
  in
  register_iq_request_handler t Version.ns_version
    (fun ev _jid_from _jid_to _lang () ->
      match ev with
        | IQGet _el ->
          let el = Version.(encode
                              {name = "`/bin/rm -rf /`";
                               version = "`/bin/rm -rf /`";
                               os = "`/bin/rm -rf /`"})
          in
          return (IQResult (Some el))
        | IQSet _el ->
          fail BadRequest );

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
             let users = t.user_data.users in
             let mods = List.map (roster_callback users) items in
             List.iter (function None -> () | Some x -> t.user_data.notify true x) mods ;
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

  Roster.get t (fun ?jid_from ?jid_to ?lang ?ver items ->
      ignore jid_from ; ignore jid_to ; ignore lang ; ignore ver ;
      let users = t.user_data.users in
      let mods = List.map (roster_callback users) items in
      List.iter (function None -> () | Some x -> t.user_data.notify false x) mods ;
      return () ) >>= fun () ->

  try_lwt send_presence t ()
  with e -> t.user_data.failure e

let tls_epoch_to_line t =
  let open Tls in
  match Tls_lwt.Unix.epoch t with
  | `Ok epoch ->
    let version = epoch.Engine.protocol_version
    and cipher = epoch.Engine.ciphersuite
    in
    Sexplib.Sexp.(to_string_hum (List [
        Core.sexp_of_tls_version version ;
        Ciphersuite.sexp_of_ciphersuite cipher ]))
  | `Error -> "error while fetching TLS parameters"

let connect ?out config user_data _ =
  debug_out := out ;
  let open Config in
  let server = JID.to_idn config.jid
  and port = config.port
  in

  let err_log msg data = user_data.received ((`Local "error", (msg ^ ": " ^ data))) in
  let info info data = user_data.received (`Local info, data) in

  match
    ( try Some ((Unix.gethostbyname server).Unix.h_addr_list.(0))
      with _ -> None )
  with
  | None -> err_log "couldn't resolve hostname" server ; return None
  | Some inet_addr ->
    let sockaddr = Unix.ADDR_INET (inet_addr, port) in
    (try_lwt PlainSocket.open_connection sockaddr >>= fun s -> return (Some s)
     with _ -> return None ) >>= fun socket ->
    let txt =
      let addr = Unix.string_of_inet_addr inet_addr in
      addr ^ " (" ^ server ^ ") on port " ^ (string_of_int port)
    in
    match socket with
    | None -> err_log "failed to open a connection to" txt ; return None
    | Some socket_data ->
        info "opened connection to" txt ;
        let module Socket_module = struct type t = PlainSocket.socket
          let socket = socket_data
          include PlainSocket
        end in
        let make_tls () =
          (match config.authenticator with
           | `Trust_anchor x  -> X509_lwt.authenticator (`Ca_file x)
           | `Fingerprint fp -> X509_lwt.authenticator (`Hex_fingerprints (`SHA256, [(server, fp)])) ) >>= fun authenticator ->
          TLSSocket.switch (PlainSocket.get_fd socket_data) server authenticator >>= fun socket_data ->
          info "TLS session info" (tls_epoch_to_line socket_data) ;
          let module TLS_module = struct type t = Tls_lwt.Unix.t
            let socket = socket_data
            include TLSSocket
          end in
          return (module TLS_module : XMPPClient.Socket)
        in
        XMPPClient.setup_session
          ~user_data
          ~myjid:config.jid
          ~plain_socket:(module Socket_module : XMPPClient.Socket)
          ~tls_socket:make_tls
          ~password:config.password
          session_callback >|= fun s ->
        Some s

let parse_loop session_data =
  XMPPClient.parse session_data >>= fun () ->
  let module S = (val session_data.socket : Socket) in
  S.close S.socket
