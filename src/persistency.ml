open Lwt
open Sexplib

let read_data fd =
  Lwt_unix.fstat fd >>= fun stats ->
  let size = stats.Lwt_unix.st_size in
  let buf = Bytes.create size in
  let rec read start =
    let len = size - start in
    Lwt_unix.read fd buf start len >>= function
    | x when x = len -> return buf
    | x -> read (start + x)
  in
  read 0

let read dir file =
  Lwt.catch (fun () ->
      Lwt_unix.access dir [ Unix.F_OK ; Unix.X_OK ] >>= fun () ->
      let f = Filename.concat dir file in
      Lwt_unix.access f [ Unix.F_OK ; Unix.R_OK ] >>= fun () ->
      Lwt_unix.openfile f [Unix.O_RDONLY] 0 >>= fun fd ->
      read_data fd >>= fun buf ->
      Lwt_unix.close fd >|= fun () ->
      Some (String.trim buf))
    (fun _ ->
       Lwt.catch
         (fun () ->
            Lwt_unix.access dir [ Unix.F_OK ] >|= fun () ->
            Some dir)
         (fun _ -> return None) >>= function
       | Some f ->
         Lwt_unix.stat f >>= fun stat ->
         if stat.Lwt_unix.st_kind = Lwt_unix.S_DIR then
           return None
         else
           fail (Invalid_argument "given path is not a directory")
       | None -> return None )

let write_data fd data =
  let rec write start =
    let len = Bytes.length data - start in
    Lwt_unix.write fd data start len >>= function
    | n when n = len -> return ()
    | n              -> write (start + n)
  in
  write 0

let ensure_create dir =
  Lwt.catch (fun () -> Lwt_unix.access dir [ Unix.F_OK ; Unix.X_OK ])
    (fun _ -> Lwt_unix.mkdir dir 0o700)

let open_append dir file =
  ensure_create dir >>= fun () ->
  let file = Filename.concat dir file in
  Lwt_unix.openfile file Unix.([O_WRONLY ; O_APPEND; O_CREAT]) 0o600

let append dir file buf =
  open_append dir file >>= fun fd ->
  write_data fd buf >>= fun () ->
  Lwt_unix.close fd

let write dir filename buf =
  ensure_create dir >>= fun () ->
  let f = Filename.concat dir filename in
  let file = f ^ ".tmp" in
  Lwt.catch (fun () ->
      Lwt_unix.access file [ Unix.F_OK ; Unix.W_OK ] >>= fun () ->
      Lwt_unix.unlink file)
    (fun _ -> return ()) >>= fun () ->
  Lwt_unix.openfile file [Unix.O_WRONLY ; Unix.O_EXCL ; Unix.O_CREAT] 0o600 >>= fun fd ->
  write_data fd buf >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  Lwt_unix.rename file f >>= fun () ->
  return ()

let config = "config.sexp"
let users = "users.sexp"
let history_dir dir = Filename.concat dir "histories"

let dump_config cfgdir cfg =
  write cfgdir config (Config.store_config cfg)

let dump_users cfgdir data =
  let userdb, histories = User.store_users data in
  write cfgdir users userdb >>= fun () ->
  let histo = history_dir cfgdir in
  Lwt_list.iter_p (fun (id, data) -> append histo id data) histories

let load_config cfg =
  read cfg config >|= function
  | Some x ->  Some (Config.load_config x)
  | None   -> None

let load_users cfg =
  read cfg users >|= function
  | Some x ->  (try User.load_users (history_dir cfg) x with _ -> User.Users.create 100)
  | None -> User.Users.create 100

