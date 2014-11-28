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

type user_data = {
  config : Config.t ;
  mutable users : User.users ;
  received : string -> unit ;
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
      Lwt_unix.close fd >>= fun () ->
      assert (s = size) ;
      return buf)
    (fun _ -> return "")

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

let dump_config dir cfg =
  let cfgdir = xmpp_config dir in
  write cfgdir config (Config.store_config cfg)

let dump_users cfgdir data =
  let cfgdir = xmpp_config cfgdir in
  write cfgdir users (User.store_users data)

let init cfgdir =
  Tls_lwt.rng_init () >>= fun () ->
  let cfg = xmpp_config cfgdir in
  read cfg config >>= fun cfgdata ->
  let config = try Config.load_config cfgdata with _ -> Config.empty in
  read cfg users >>= fun userdata ->
  let users = try User.load_users userdata with _ -> User.Users.empty in
  Printf.printf "returning from init with:\n - config: %s\n - users:\n%!"
    (Sexplib.Sexp.to_string_hum (Config.sexp_of_t config)) ;
  User.Users.iter
    (fun k u ->
       Printf.printf "  - %s -> %s\n%!" k (Sexplib.Sexp.to_string_hum (User.sexp_of_t u)))
    users;
  return (config, users)

let user_session stanza user_data =
  let log = user_data.received in
  match stanza.jid_from with
    | None -> assert false
    | Some { JID.lnode ; JID.ldomain ; JID.lresource } ->
      let jid = lnode ^ "@" ^ ldomain in
      let user = User.find_or_get jid user_data.users in
      user_data.users <- User.Users.(add jid user user_data.users) ;
      let session =
        try
          List.find
            (fun s -> s.User.resource = lresource)
            user.User.active_sessions
        with Not_found ->
          (let otr = user_data.config.Config.otr_config in
           let sess = User.empty_session lresource otr () in
           user.User.active_sessions <- sess :: user.User.active_sessions ;
           sess)
in
      log (jid ^ "/" ^ lresource ^ " (" ^ (string_of_int (List.length user.User.active_sessions)) ^ ") ") ;
      (user, session)

let message_callback (t : user_data session_data) stanza =
  let log = t.user_data.received in
  let user, session = user_session stanza t.user_data in
  match stanza.content.body with
  | None -> log "nothing received\n" ; return ()
  | Some v ->
    let ctx, out, warn, received, plain = Otr.Handshake.handle session.User.otr v in
    (match plain with
     | None -> ()
     | Some p -> log ("plain message: " ^ p ^ "\n")) ;
    (match warn with
     | None -> ()
     | Some w -> log ("warning: " ^ w ^ "\n")) ;
    (match received with
     | None -> ()
     | Some c -> log ("received encrypted: " ^ c ^ "\n")) ;
    (match plain, warn, received with
     | None, None, None -> log "nothing...\n"
     | _ -> ()) ;
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
  print_endline ("message error: " ^ error.err_text);
  return ()

let presence_callback t stanza =
  let log = t.user_data.received in
  let user, session = user_session stanza t.user_data in
  (match stanza.content.priority with
   | None -> ()
   | Some x -> session.User.priority <- x) ;
  let open User in
  (match stanza.content.presence_type with
   | None ->
     begin
       match stanza.content.show with
       | None -> session.presence <- `Online ; log "available"
       | Some ShowChat -> session.presence <- `Free ; log "free"
       | Some ShowAway -> session.presence <- `Away ; log "away"
       | Some ShowDND -> session.presence <- `DoNotDisturb ; log "dnd"
       | Some ShowXA -> session.presence <- `ExtendedAway ; log "extended away"
     end
   | Some Probe -> log "probed"
   | Some Subscribe -> log "subscription request"
   | Some Subscribed -> log "successfully subscribed"
   | Some Unsubscribe -> log "shouldn't see this unsubscribe"
   | Some Unsubscribed -> log "you're so off my buddy list"
   | Some Unavailable -> session.presence <- `Offline ; log "offline"
  );
  (match stanza.content.status with
   | None -> session.status <- None
   | Some x when x = "" -> session.status <- None
   | Some x -> session.status <- Some x ; log (" - " ^ x));
  log "\n" ;
  return ()

let presence_error t ?id ?jid_from ?jid_to ?lang error =
  print_endline ("presence error: " ^ error.err_text);
  return ()


let session_callback t =
  print_endline "in session callback" ;
  register_iq_request_handler t Version.ns_version
    (fun ev _jid_from _jid_to _lang () ->
      match ev with
        | IQGet _el ->
          let el = Version.encode {Version.name = "xmpptest";
                                   Version.version = "2.0";
                                   Version.os = Sys.os_type} in
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
      let users = List.fold_left
          (fun s item ->
             let jid =
               let { JID.lnode ; JID.ldomain } = item.Roster.jid in
               lnode ^ "@" ^ ldomain
             in
             let elt = User.find_or_get jid s in
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
             let t = { elt with
                       User.name = item.Roster.name ;
                       User.groups = item.Roster.group ;
                       subscription ; props }
             in
             User.Users.add jid t s
          ) users items
      in
      Printf.printf "users is now: %s\n%!" (User.store_users users) ;
      t.user_data.users <- users ;
      return ()) >>= fun () ->
  print_endline "sending presence" ;
  send_presence t () >>= fun () ->
  print_endline "returning" ;
  return ()

let connect user_data _ =
  Printf.printf "connecting\n%!" ;
  let open Config in
  let config = user_data.config in
  let server = JID.to_idn config.jid
  and port = config.port
  in
  Printf.printf "connecting to %s %d\n%!" server port ;
  let inet_addr =
    try Unix.inet_addr_of_string server
    with Failure("inet_addr_of_string") ->
      (Unix.gethostbyname server).Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in

  Lwt.async (fun () ->
    PlainSocket.open_connection sockaddr >>= fun socket_data ->
    let module Socket_module = struct type t = PlainSocket.socket
      let socket = socket_data
      include PlainSocket
    end in
    let make_tls () =
      TLSSocket.switch (PlainSocket.get_fd socket_data) server config.trust_anchor >>= fun socket_data ->
      let module TLS_module = struct type t = Tls_lwt.Unix.t
        let socket = socket_data
        include TLSSocket
      end in
      return (module TLS_module : XMPPClient.Socket)
    in
    print_endline "setting up" ;
    XMPPClient.setup_session
      ~user_data
      ~myjid:config.jid
      ~plain_socket:(module Socket_module : XMPPClient.Socket)
      ~tls_socket:make_tls
      ~password:config.password
      session_callback >>= fun session_data ->
    XMPPClient.parse session_data >>= fun () ->
    let module S = (val session_data.socket : Socket) in
    S.close S.socket
  )
