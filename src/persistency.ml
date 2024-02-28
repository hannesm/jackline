open Lwt.Infix
open Sexplib

let read_data fd =
  Lwt_unix.fstat fd >>= fun stats ->
  let size = stats.Lwt_unix.st_size in
  let buf = Bytes.create size in
  let rec read start =
    let len = size - start in
    Lwt_unix.read fd buf start len >>= function
    | x when x = len -> Lwt.return buf
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
      Some (String.trim (Bytes.to_string buf)))
    (fun _ ->
       Lwt.catch
         (fun () ->
            Lwt_unix.access dir [ Unix.F_OK ] >|= fun () ->
            Some dir)
         (fun _ -> Lwt.return None) >>= function
       | Some f ->
         Lwt_unix.stat f >>= fun stat ->
         if stat.Lwt_unix.st_kind = Lwt_unix.S_DIR then
           Lwt.return None
         else
           Lwt.fail (Invalid_argument "given path is not a directory")
       | None -> Lwt.return None )

let write_data fd data =
  let rec write start =
    let len = Bytes.length data - start in
    Lwt_unix.write fd data start len >>= function
    | n when n = len -> Lwt.return ()
    | n              -> write (start + n)
  in
  write 0

let rec ensure_create dir =
  Lwt.catch
    (fun () -> Lwt_unix.access dir [ Unix.F_OK ; Unix.X_OK ])
    (fun _ ->
       Lwt.catch
         (fun () -> Lwt_unix.mkdir dir 0o700)
         (fun _ ->
            ensure_create (Filename.dirname dir) >>= fun () ->
            ensure_create dir))

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
    (fun _ -> Lwt.return ())

let write dir filename buf =
  ensure_create dir >>= fun () ->
  let f = Filename.concat dir filename in
  let file = f ^ ".tmp" in
  delete file >>= fun () ->
  Lwt_unix.openfile file [Unix.O_WRONLY ; Unix.O_EXCL ; Unix.O_CREAT] 0o600 >>= fun fd ->
  write_data fd buf >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  Lwt_unix.rename file f >>= fun () ->
  Lwt.return ()

let config = "config.sexp"
let colours = "colours.sexp"
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
  write cfgdir config (Xconfig.store_config cfg)

let load_config dsa cfg =
  read cfg config >|= function
  | Some x ->  Some (Xconfig.load_config dsa x)
  | None   -> None


let load_colours cfg =
  read cfg colours >|= function
  | Some x -> Some (Sexplib.Conv.(list_of_sexp (pair_of_sexp User.chatkind_of_sexp string_of_sexp)) (Sexplib.Sexp.of_string x))
  | None   -> None

let dump_user cfgdir user =
  user_dir cfgdir >>= fun userdir ->
  let out = Xjid.bare_jid_to_string (Contact.bare user) in
  match Contact.marshal user with
  | None ->
     let file = Filename.concat userdir out in
     delete file
  | Some sexp ->
     let data = Bytes.of_string (Sexplib.Sexp.to_string_hum sexp) in
     write userdir out data

let notify_user cfgdir =
  let mvar = Lwt_mvar.create_empty () in
  let rec loop () =
    Lwt_mvar.take mvar >>= fun user ->
    dump_user cfgdir user >>= fun () ->
    loop ()
  in
  Lwt.async loop ;
  mvar

let load_user_dir cfgdir users =
  user_dir cfgdir >>= fun dir ->
  Lwt_unix.opendir dir >>= fun dh ->
  let rec loadone () =
    Lwt.catch (fun () ->
        Lwt_unix.readdir dh >>= fun f ->
        if f = "." || f = ".." then
          loadone ()
        else
          (read dir f >>= fun data ->
           (match data with
            | None -> Printf.printf "couldn't read file %s" f
            | Some x -> match Contact.load x with
              | None -> Printf.printf "something went wrong while loading %s/%s\n" dir f
              | Some x -> Contact.replace_contact users x) ;
           loadone ()))
      (function
        | End_of_file -> Lwt_unix.closedir dh
        | e -> Printf.printf "problem while loading a user %s\n" (Printexc.to_string e) ; Lwt.return_unit)
  in
  loadone ()

let dump_history cfgdir buddy =
  match Contact.marshal_history buddy with
  | None -> Lwt.return_unit (* should remove if user.User.preserve_messages is not set *)
  | Some sexp ->
     message_history_dir cfgdir >>= fun history_dir ->
     append history_dir (Xjid.bare_jid_to_string (Contact.bare buddy)) (Bytes.of_string sexp)

let dump_histories cfgdir users =
  let users = Contact.fold (fun _ v acc -> v :: acc) users [] in
  Lwt_list.iter_p (dump_history cfgdir) users

let load_users cfg =
  let data = Contact.create () in
  read cfg users >|= function
  | Some x ->  (try
                   let us = User.load_users x in
                   List.iter (Contact.replace_user data) us ;
                   data
                with _ -> data)
  | None -> data

let load_histories cfg contacts =
  message_history_dir cfg >|= fun histo ->
  let contact_list =
    Contact.fold
      (fun _ v acc -> Contact.load_history histo v :: acc)
      contacts
      []
  in
  List.iter (Contact.replace_contact contacts) contact_list

let pass_file = "password"

let dump_password cfgdir password =
  write cfgdir pass_file password

let load_password cfgdir =
  read cfgdir pass_file

let otr_dsa = "otr_dsa.sexp"

let sexp_of_priv Mirage_crypto_pk.Dsa.{ p ; q ; gg ; x ; y } =
  let open Conv in
  let sexp_of_z z = sexp_of_string (Z.to_string z) in
  sexp_of_list (sexp_of_pair sexp_of_string sexp_of_z)
    [ "p", p; "q", q; "gg", gg; "x", x; "y", y ]

let dump_dsa cfgdir dsa =
  write cfgdir otr_dsa (Bytes.of_string (Sexp.to_string_hum (sexp_of_priv dsa)))

let load_dsa cfgdir =
  read cfgdir otr_dsa >|= function
  | None -> None
  | Some x -> Some (Xconfig.dsa_priv_of_sexp (Sexp.of_string x))
