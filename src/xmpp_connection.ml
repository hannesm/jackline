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
    let cs = Cstruct.create len in
    Tls_lwt.Unix.read s cs >>= fun size ->
    if size > 0 then
      (String.blit (Cstruct.to_string cs) 0 buf start size ;
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
