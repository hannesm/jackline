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
  users : User.users ;
  received : string -> string -> unit ;
  notify : User.user -> unit ;
}

let read dir file =
  Lwt.catch (fun () ->
      Lwt_unix.access dir [ Unix.F_OK ; Unix.X_OK ] >>= fun () ->
      let f = Filename.concat dir file in
      Lwt_unix.access f [ Unix.F_OK ; Unix.R_OK ] >>= fun () ->
      Lwt_unix.openfile f [Unix.O_RDONLY] 0 >>= fun fd ->
      Lwt_unix.fstat fd >>= fun stats ->
      let size = stats.Lwt_unix.st_size in
      let buf = Bytes.create size in
      Lwt_unix.read fd buf 0 size >>= fun s ->
      Lwt_unix.close fd >|= fun () ->
      assert (s = size) ;
      Some (String.trim buf))
    (fun _ -> return None)

let write dir filename buf =
  Lwt.catch (fun () -> Lwt_unix.access dir [ Unix.F_OK ; Unix.X_OK ])
    (fun _ -> Lwt_unix.mkdir dir 0o700) >>= fun () ->
  let f = Filename.concat dir filename in
  let file = f ^ ".tmp" in
  Lwt.catch (fun () ->
      Lwt_unix.access file [ Unix.F_OK ; Unix.W_OK ] >>= fun () ->
      Lwt_unix.unlink file)
    (fun _ -> return ()) >>= fun () ->
  Lwt_unix.openfile file [Unix.O_WRONLY ; Unix.O_EXCL ; Unix.O_CREAT] 0o600 >>= fun fd ->
  Lwt_unix.write fd buf 0 (Bytes.length buf) >>= fun s ->
  Lwt_unix.close fd >>= fun () ->
  assert (s = Bytes.length buf) ;
  Lwt_unix.rename file f >>= fun () ->
  return ()

let config = "config.sexp"
let users = "users.sexp"

open Sexplib

let xmpp_config dir = Filename.concat dir "ocaml-xmpp-client"

let dump_config cfgdir cfg =
  write cfgdir config (Config.store_config cfg)

let dump_users cfgdir data =
  write cfgdir users (User.store_users data)

let load_config cfg =
  read cfg config >|= function
  | Some x ->  Some (Config.load_config x)
  | None   -> None

let load_users cfg =
  read cfg users >|= function
  | Some x ->  (try User.load_users x with _ -> User.Users.create 100)
  | None -> User.Users.create 100

let user_session stanza user_data =
  match stanza.jid_from with
    | None -> assert false
    | Some jid ->
      let user = User.find_or_add jid user_data.users in
      let session = User.ensure_session jid user_data.otr_config user in
      (user, session)

let message_callback (t : user_data session_data) stanza =
  let user, session = user_session stanza t.user_data in
  let msg dir enc txt =
    let message = (dir, enc, true, Unix.localtime (Unix.time ()), txt) in
    session.User.messages <- message :: session.User.messages ;
    t.user_data.notify user
  in
  match stanza.content.body with
  | None ->
    (*    msg `Local false "**empty message**" ; *)
    return_unit
  | Some v ->
    let ctx, out, ret = Otr.Handshake.handle session.User.otr v in
    List.iter (function
        | `Established_encrypted_session ->
          msg `Local false "encrypted OTR connection established" ;
          ( match User.find_fp user ctx with
            | fp, Some fps ->
              let verified_key = List.exists (fun x -> x.User.verified) user.User.otr_fingerprints in
              let verify = "verify over secondary channel (and type /fingerprint fp)" in
              let otrmsg =
                match verified_key, fps.User.verified, fps.User.session_count with
                | _, true, _ -> "verified OTR fingerprint"
                | true, false, 0 -> "POSSIBLE BREAKIN ATTEMPT! new unverified OTR fingerprint, verified fingerprint present for contact! " ^ verify
                | true, false, n -> "unverified OTR fingerprint (used " ^ (string_of_int n) ^ " times), verified fingerprint present for contact! please " ^ verify
                | false, false, 0 -> "new unverified key! please " ^ verify
                | false, false, n -> "unverified key (used " ^ (string_of_int n) ^ " times). please " ^ verify
              in
              msg `Local false otrmsg ;
              User.insert_inc user session.User.resource fps ;
            | fp, None ->
              msg `Local false "shouldn't happen - OTR established but couldn't find fingerprint" )
        | `Warning w -> msg `Local false w
        | `Received_error e -> msg `From false e
        | `Received m -> msg `From false m
        | `Received_encrypted e -> msg `From true e)
      ret ;
    session.User.otr <- ctx ;
    match out with
    | None -> return ()
    | Some _ ->
      send_message t ?jid_to:stanza.jid_from
        ?id:stanza.id
        ?kind:stanza.content.message_type
        ?lang:stanza.lang
        ?body:out ()

let message_error t ?id ?jid_from ?jid_to ?lang error =
  let log = t.user_data.received in
  let jid = match jid_from with
    | None -> "unknown"
    | Some x -> JID.string_of_jid x
  in
  log jid ("error message: " ^ error.err_text) ;
  return_unit

let presence_callback t stanza =
  let log = t.user_data.received in
  let user, session = user_session stanza t.user_data in
  let id = User.userid user session in
  (match stanza.content.priority with
   | None -> ()
   | Some x -> session.User.priority <- x) ;
  let open User in
  let stat = match stanza.content.status with
    | None -> session.status <- None ; ""
    | Some x when x = "" -> session.status <- None ; ""
    | Some x -> session.status <- Some x ; (" - " ^ x)
  in
  let old = User.presence_to_char session.presence in
  let logp () =
    let n = User.presence_to_char session.presence in
    let nl = User.presence_to_string session.presence in
    log id ("presence changed: [" ^ old ^ ">" ^ n ^ "] (now " ^ nl ^ ")" ^ stat)
  in
  (match stanza.content.presence_type with
   | None ->
     begin
       match stanza.content.show with
       | None -> session.presence <- `Online ; logp ()
       | Some ShowChat -> session.presence <- `Free ; logp ()
       | Some ShowAway -> session.presence <- `Away ; logp ()
       | Some ShowDND -> session.presence <- `DoNotDisturb ; logp ()
       | Some ShowXA -> session.presence <- `ExtendedAway ; logp ()
     end
   | Some Probe -> log id ("probed" ^ stat)
   | Some Subscribe -> log id ("subscription request" ^ stat)
   | Some Subscribed -> log id ("successfully subscribed" ^ stat)
   | Some Unsubscribe -> log id ("shouldn't see this unsubscribe" ^ stat)
   | Some Unsubscribed -> log id ("you're so off my buddy list" ^ stat)
   | Some Unavailable -> session.presence <- `Offline ; logp ()
  );
  return ()

let presence_error t ?id ?jid_from ?jid_to ?lang error =
  let log = t.user_data.received in
  let jid = match jid_from with
    | None -> "unknown"
    | Some x -> JID.string_of_jid x
  in
  log jid ("presence error: " ^ error.err_text) ;
  return_unit


let session_callback t =
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
          fail BadRequest
    );
  register_stanza_handler t (ns_client, "message")
    (parse_message ~callback:message_callback ~callback_error:message_error);
  register_stanza_handler t (ns_client, "presence")
    (parse_presence ~callback:presence_callback ~callback_error:presence_error);
  Roster.get t (fun ?jid_from ?jid_to ?lang ?ver items ->
      let users = t.user_data.users in
      (List.iter
         (fun item ->
            let user = User.find_or_add item.Roster.jid users in
            let subscription =
              match item.Roster.subscription with
              | Roster.SubscriptionRemove -> assert false
              | Roster.SubscriptionBoth -> `Both
              | Roster.SubscriptionNone -> `None
              | Roster.SubscriptionFrom -> `From
              | Roster.SubscriptionTo -> `To
            in
            let props =
              let app = if item.Roster.approved then [`PreApproved ] else [] in
              let ask = match item.Roster.ask with | Some _ -> [ `Pending ] | None -> [] in
              app @ ask
            in
            let name = if item.Roster.name = "" then None else Some item.Roster.name in
            let t = { user with
                      User.name = name ;
                      User.groups = item.Roster.group ;
                      subscription ; props }
            in
            User.(Users.replace users t.jid t)
         ) items );
      return ()) >>= fun () ->
  send_presence t () >>= fun () ->
  return ()

let connect config user_data _ =
  let open Config in
  let server = JID.to_idn config.jid
  and port = config.port
  in
  let inet_addr =
    try Unix.inet_addr_of_string server
    with Failure("inet_addr_of_string") ->
      (Unix.gethostbyname server).Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in

  PlainSocket.open_connection sockaddr >>= fun socket_data ->
  let module Socket_module = struct type t = PlainSocket.socket
    let socket = socket_data
    include PlainSocket
  end in
  let make_tls () =
    (match config.authenticator with
     | `Trust_anchor x  -> X509_lwt.authenticator (`Ca_file x)
     | `Fingerprint fp -> X509_lwt.authenticator (`Hex_fingerprints (`SHA256, [(server, fp)]))
     | `None -> fail (Invalid_argument "Specify a TLS authentication method: Fingerprint or path to trust anchors") ) >>= fun authenticator ->
    TLSSocket.switch (PlainSocket.get_fd socket_data) server authenticator >>= fun socket_data ->
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
    session_callback

let parse_loop session_data =
  XMPPClient.parse session_data >>= fun () ->
  let module S = (val session_data.socket : Socket) in
  S.close S.socket
