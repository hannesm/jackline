
open Lwt.Infix

let (debug_out : Lwt_unix.file_descr option ref) = ref None

let dbg data =
  match !debug_out with
  | None -> Lwt.return ()
  | Some x ->
    let now = Unix.localtime (Unix.time ()) in
    let msg =
      Printf.sprintf "[%02d:%02d:%02d] %s\n"
        now.Unix.tm_hour now.Unix.tm_min now.Unix.tm_sec data
    in
    Persistency.write_data x msg

module PlainSocket =
struct
  type 'a z = 'a Lwt.t

  type fd = Lwt_unix.file_descr

  type socket = fd

  let get_fd fd = fd

  let open_connection sockaddr =
    let open Lwt_unix in
    let fd = socket PF_INET SOCK_STREAM 0 in
    connect fd sockaddr >>= fun () ->
    Lwt.return fd

  let read fd buf start len =
    Lwt_unix.read fd buf start len >>= fun size ->
    dbg ("IN: " ^ (String.sub buf start size)) >|= fun () ->
    size

  let write fd str =
    dbg ("OUT: " ^ str) >>= fun () ->
    let len = String.length str in
    let rec aux_send start =
      Lwt_unix.write fd str start (len - start) >>= fun sent ->
      if sent = 0 then
        Lwt.return ()
      else
        aux_send (start + sent)
    in
      aux_send 0

  let close fd =
    Lwt_unix.close fd

end

module TLSSocket =
struct
  let read s buf start len =
    let cs = Cstruct.create len in
    Tls_lwt.Unix.read s cs >>= fun size ->
    ( if size > 0 then
        (String.blit (Cstruct.to_string cs) 0 buf start size ;
         dbg ("IN TLS: " ^ (String.sub buf start size)))
      else
        Lwt.return () ) >|= fun () ->
    size

  let write s str =
    dbg ("OUT TLS: " ^ str) >>= fun () ->
    Tls_lwt.Unix.write s (Cstruct.of_string str)

  let switch fd host authenticator =
    let config = Tls.Config.client ~authenticator () in
    Tls_lwt.Unix.client_of_fd config ~host fd

  let close s =
    Tls_lwt.Unix.close s

end
