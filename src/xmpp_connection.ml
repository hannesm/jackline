
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
    Persistency.write_data x (Bytes.of_string msg)

module PlainSocket =
struct
  type 'a z = 'a Lwt.t

  type fd = Lwt_unix.file_descr

  type socket = fd

  let read fd buf start len =
    Lwt_unix.read fd buf start len >>= fun size ->
    dbg ("IN: " ^ Bytes.to_string (Bytes.sub buf start size)) >|= fun () ->
    size

  let write fd str =
    dbg ("OUT: " ^ Bytes.to_string str) >>= fun () ->
    let len = Bytes.length str in
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
    Tls_lwt.Unix.read s ~off:start buf >>= fun size ->
    ( if size > 0 then
        dbg ("IN TLS: " ^ Bytes.sub_string buf start size)
      else
        Lwt.return () ) >|= fun () ->
    assert (len >= size);
    size

  let write s bs =
    let str = Bytes.to_string bs in
    dbg ("OUT TLS: " ^ str) >>= fun () ->
    Tls_lwt.Unix.write s str

  let switch fd ?host authenticator =
    let config = Result.get_ok (Tls.Config.client ~authenticator ()) in
    Tls_lwt.Unix.client_of_fd config ?host fd

  let close s =
    Tls_lwt.Unix.close s

end
