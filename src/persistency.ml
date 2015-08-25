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

let delete file =
  Lwt.catch (fun () ->
      Lwt_unix.access file [ Unix.F_OK ; Unix.W_OK ] >>= fun () ->
      Lwt_unix.unlink file)
    (fun _ -> return ())

let write dir filename buf =
  ensure_create dir >>= fun () ->
  let f = Filename.concat dir filename in
  let file = f ^ ".tmp" in
  delete file >>= fun () ->
  Lwt_unix.openfile file [Unix.O_WRONLY ; Unix.O_EXCL ; Unix.O_CREAT] 0o600 >>= fun fd ->
  write_data fd buf >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  Lwt_unix.rename file f >>= fun () ->
  return ()

let config = "config.sexp"
let users = "users.sexp"

let maybe_create_dir dir =
  Lwt.catch (fun () -> Lwt_unix.access dir [Unix.F_OK ; Unix.R_OK])
            (fun _ -> Lwt_unix.mkdir dir 0o700)

let history = "histories"
let message_history_dir dir =
  let name = Filename.concat dir history in
  maybe_create_dir name >|= fun () ->
  name

let user_dir dir =
  let name = Filename.concat dir "users" in
  maybe_create_dir name >|= fun () ->
  name

let dump_config cfgdir cfg =
  write cfgdir config (Config.store_config cfg)

let load_config dsa cfg =
  read cfg config >|= function
  | Some x ->  Some (Config.load_config dsa x)
  | None   -> None

let dump_user cfgdir user =
  user_dir cfgdir >>= fun userdir ->
  match User.store_user user with
  | None ->
     let file = Filename.concat userdir (User.jid user) in
     delete file
  | Some sexp ->
     write userdir (User.jid user) sexp

let notify_user cfgdir =
  let mvar = Lwt_mvar.create_empty () in
  let rec loop () =
    Lwt_mvar.take mvar >>= fun user ->
    dump_user cfgdir user >>= fun () ->
    loop ()
  in
  Lwt.async loop ;
  mvar

let load_user dir file =
  read dir file >|= function
  | Some x -> User.load_user x
  | None -> None

let load_user_dir cfgdir users =
  message_history_dir cfgdir >>= fun hist_dir ->
  user_dir cfgdir >>= fun dir ->
  Lwt_unix.opendir dir >>= fun dh ->
  Lwt_unix.readdir dh >>= fun _ -> (* skip . *)
  Lwt_unix.readdir dh >>= fun _ -> (* skip .. *)
  let rec loadone () =
    try_lwt
      (Lwt_unix.readdir dh >>= fun f ->
       load_user dir f >>= fun x ->
       (match x with
        | None -> Printf.printf "something went wrong while loading %s/%s\n" dir f
        | Some x ->
           let message_history =
             User.load_history
               (`Bare x.User.bare_jid)
               (Filename.concat hist_dir (User.jid x))
               x.User.preserve_messages
           in
           let user = { x with User.message_history } in
           User.add_or_replace users user) ;
       loadone ())
    with End_of_file -> Lwt_unix.closedir dh
  in
  loadone ()

let dump_history cfgdir user =
  match User.marshal_history user with
  | None -> Lwt.return_unit (* should remove if user.User.preserve_messages is not set *)
  | Some (_, sexp) ->
     message_history_dir cfgdir >>= fun history_dir ->
     append history_dir (User.jid user) sexp

let dump_histories cfgdir users =
  let users = User.Users.fold (fun _ v acc -> v :: acc) users [] in
  Lwt_list.iter_p (dump_history cfgdir) users

let load_users cfg =
  message_history_dir cfg >>= fun histo ->
  read cfg users >|= function
  | Some x ->  (try User.load_users histo x
                with _ -> User.Users.create 100)
  | None -> User.Users.create 100

let pass_file = "password"

let dump_password cfgdir password =
  write cfgdir pass_file password

let load_password cfgdir =
  read cfgdir pass_file

let otr_dsa = "otr_dsa.sexp"

let dump_dsa cfgdir dsa =
  write cfgdir otr_dsa (Sexp.to_string_hum (Nocrypto.Dsa.sexp_of_priv dsa))

let load_dsa cfgdir =
  read cfgdir otr_dsa >|= function
  | None -> None
  | Some x -> Some (Nocrypto.Dsa.priv_of_sexp (Sexp.of_string x))
