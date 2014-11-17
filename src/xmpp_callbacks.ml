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

type data = {
  mutable config : Config.t ;
  mutable users : User.t list ;
}

type callbacks = {
  received : string -> unit
}

type user_data = data * callbacks

let config d = Filename.concat d "config.sexp"
let user d = Filename.concat d "users.sexp"

let read file =
  Lwt_unix.access file [ Unix.F_OK ; Unix.R_OK ] >>= fun () ->
  Lwt_unix.openfile file [Unix.O_RDONLY] 0 >>= fun fd ->
  Lwt_unix.fstat fd >>= fun stats ->
  let size = stats.Lwt_unix.st_size in
  let buf = Bytes.create size in
  Lwt_unix.read fd buf 0 size >>= fun s ->
  Lwt_unix.close fd >>= fun () ->
  assert (s = size) ;
  return buf

let write filename buf =
  let file = filename ^ ".tmp" in
  Lwt_unix.access file [ Unix.F_OK ; Unix.W_OK ] >>= fun () ->
  Lwt_unix.openfile file [Unix.O_WRONLY] 0o600 >>= fun fd ->
  Lwt_unix.write fd buf 0 (Bytes.length buf) >>= fun s ->
  Lwt_unix.close fd >>= fun () ->
  assert (s = Bytes.length buf) ;
  Lwt_unix.rename file filename >>= fun () ->
  return ()

let read_config dir = read (config dir)
let read_users dir = read (user dir)

let write_config dir = write (config dir)
let write_users dir = write (user dir)

open Sexplib

let xmpp_config dir = Filename.concat dir "ocaml-xmpp-client"

let init cfgdir =
  Tls_lwt.rng_init () >>= fun () ->
  let cfg = xmpp_config cfgdir in
  read_config cfg >>= fun cfgdata ->
  let config = try Config.load_config cfgdata with _ -> Config.empty in
  read_users cfg >>= fun userdata ->
  let users = try User.load_users userdata with _ -> [] in
  Printf.printf "returning from init with:\n - config: %s\n - users:\n   %s\n"
    (Sexplib.Sexp.to_string_hum (Config.sexp_of_t config))
    (String.concat "\n   "
       (List.map
          (fun u -> Sexplib.Sexp.to_string_hum (User.sexp_of_t u))
          users)) ;
  return { config ; users }

let message_callback (t : user_data session_data) stanza =
  let data, callbacks = t.user_data in
  match stanza.content.body with
  | None -> callbacks.received "nothing received\n" ; return ()
  | Some v ->
    callbacks.received (v ^ "\n") ;
    let out = "Nothing to send" in
    send_message t ?jid_to:stanza.jid_from
      ?id:stanza.id
      ?kind:stanza.content.message_type
      ?lang:stanza.lang
      ?body:(Some out) ()

let message_error t ?id ?jid_from ?jid_to ?lang error =
  print_endline ("message error: " ^ error.err_text);
  return ()

let presence_callback t stanza =
  (match stanza.content.presence_type with
    | None -> print_endline "available"
    | Some _ -> print_endline "something"
  ); return ()

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
      let open Roster in
      Printf.printf "%d items\n" (List.length items) ;
      List.iter (fun x -> Printf.printf "jid %s\n%!" (JID.string_of_jid x.jid)) items;
      return ()) >>= fun () ->
  print_endline "sending presence" ;
  send_presence t () >>= fun () ->
  print_endline "returning" ;
  return ()

let connect (data, callbacks) _ =
  Printf.printf "connecting\n%!" ;
  let open Config in
  let config = data.config in
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
      ~user_data:(data, callbacks)
      ~myjid:config.jid
      ~plain_socket:(module Socket_module : XMPPClient.Socket)
      ~tls_socket:make_tls
      ~password:config.password
      session_callback >>= fun session_data ->
    XMPPClient.parse session_data >>= fun () ->
    let module S = (val session_data.socket : Socket) in
    S.close S.socket
  )
