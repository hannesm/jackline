open StanzaError

module PlainSocket =
struct
  open Lwt

  type 'a z = 'a Lwt.t

  type fd = Lwt_unix.file_descr

  type socket = fd

  let get_fd fd = fd

  let open_connection sockaddr =
    let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt_unix.connect fd sockaddr >>= fun () ->
    return fd

  let read fd buf start len =
    Lwt_unix.read fd buf start len >>=
      (fun size ->
        if size > 0 then
          print_string "IN: "; print_endline (String.sub buf start size);
        return size
      )

  let write fd str =
    print_string "OUT: ";
    print_endline str;
    let len = String.length str in
    let rec aux_send start =
      Lwt_unix.write fd str start (len - start) >>= fun sent ->
    if sent = 0 then
      return ()
    else
      aux_send (start + sent)
    in
      aux_send 0

  let close fd =
    Lwt_unix.close fd

end

module TLSSocket =
struct
  open Lwt

  let read s buf start len =
    (if start > 0 then
       print_endline "start != 0") ;
    let cs = Cstruct.of_string buf in
    Tls_lwt.Unix.read s cs >>= fun size ->
      if size > 0 then
        (for i = 0 to size do
           String.set buf i (Cstruct.get_char cs i)
         done ;
         print_string "IN TLS: "; print_endline (String.sub buf start size)
         ) ;
      return size

  let write s str =
    print_string "OUT TLS: ";
    print_endline str;
    Tls_lwt.Unix.write s (Cstruct.of_string str)

  let switch fd host =
    Tls_lwt.rng_init () >>= fun () ->
    X509_lwt.authenticator (`Ca_dir "certificates") >>= fun authenticator ->
    let config = Tls.Config.client ~authenticator () in
    Tls_lwt.Unix.client_of_fd config ~host fd

  let close s =
    Tls_lwt.Unix.close s

end

module ID =
struct
  type t = string
  let compare = Pervasives.compare
end
module IDCallback = Map.Make(ID)

module XMPPClient = XMPP.Make (Lwt) (Xmlstream.XmlStream) (IDCallback)

open XMPPClient

module Version = XEP_version.Make (XMPPClient)

type otr = { mutable state : Otr.State.session }

let message_callback otr t stanza =
  let send = match stanza.content.body with
  | None -> print_endline "received nothing :/" ; []
  | Some v ->
    let ctx, out, warn, received, plain = Otr.Handshake.handle otr.state v in
    (match plain with
     | None -> print_endline "no plaintext received!"
     | Some p -> print_endline ("plain message: " ^ p)) ;
    (match warn with
     | None -> print_endline "no warn"
     | Some w -> print_endline ("warning: " ^ w)) ;
    (match received with
     | None -> print_endline "no text received"
     | Some c -> print_endline ("received encrypted: " ^ c)) ;
    otr.state <- ctx ;
    let send msg =
      let ctx, out, warn = Otr.Handshake.send_otr otr.state msg in
      ( match warn with
        | None -> ()
        | Some t -> Printf.printf "warning from send_otr %s\n" t );
      otr.state <- ctx ;
      match out with
      | Some x -> [ x ]
      | None -> []
    in
    match out with
    | None ->
      ( match received with
        | Some x when x = "bla" ->
          []
        | Some x when x = "bla2" ->
          let msg1 = send "bla" in
          let msg2 = send "bla" in
          msg1 @ msg2
        | Some x when x = "fin" ->
          let ctx, out, warn = Otr.Handshake.end_otr otr.state in
          ( match warn with
            | None -> ()
            | Some t -> Printf.printf "warning from end_otr %s\n" t );
          otr.state <- ctx ;
          ( match out with
            | Some x -> [ x ]
            | None -> [ "end_otr didn't want me to send anything" ] )
        | Some x ->
          send x
        | None ->
          send "nothing to send" )
    | Some c -> [ c ]
  in
  Lwt_list.iter_s (fun out ->
      send_message t ?jid_to:stanza.jid_from
        ?id:stanza.id
        ?kind:stanza.content.message_type
        ?lang:stanza.lang
        ?body:(Some out) ()) send

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


let session t =
  print_endline "in session" ;
  let dsa = Nocrypto.Dsa.generate `Fips1024 in
  Printf.printf "my fp" ; Cstruct.hexdump (Otr.Crypto.OtrDsa.fingerprint (Nocrypto.Dsa.pub_of_priv dsa)) ;
  let otr = { state = (Otr.State.empty_session ~dsa ~policies:[`REQUIRE_ENCRYPTION] ()) } in
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
    (parse_message ~callback:(message_callback otr) ~callback_error:message_error);
  register_stanza_handler t (ns_client, "presence")
    (parse_presence ~callback:presence_callback ~callback_error:presence_error);
  print_endline "sending presence" ;
  send_presence t () >>= fun () ->
  print_endline "returning" ;
  return ()

let connect _ =
  let server = "jwchat.org"
  and username = "testbot2"
  and password = "fnord"
  and resource = "xmpp3.0"
  and port = 5222
  in

  let myjid = JID.make_jid username server resource in
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
      TLSSocket.switch (PlainSocket.get_fd socket_data) server >>= fun socket_data ->
      let module TLS_module = struct type t = Tls_lwt.Unix.t
        let socket = socket_data
        include TLSSocket
      end in
      return (module TLS_module : XMPPClient.Socket)
    in
    print_endline "setting up" ;
    XMPPClient.setup_session
      ~user_data:()
      ~myjid
      ~plain_socket:(module Socket_module : XMPPClient.Socket)
      ~tls_socket:make_tls
      ~password session >>=
    fun session_data -> XMPPClient.parse session_data >>= fun () ->
    let module S = (val session_data.socket : Socket) in
    S.close S.socket
  )
