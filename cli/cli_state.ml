
type display_mode =
  | BuddyList
  | FullScreen
  | Raw

let next_display_mode = function
  | BuddyList  -> FullScreen
  | FullScreen -> Raw
  | Raw        -> BuddyList

type notify_v =
  | Disconnected
  | Connected
  | Notifications
  | Clear
  | Quit

type ui_state = {
  config_directory            : string                    ; (* set initially *)
  user                        : string                    ; (* set initially *)
  resource                    : string                    ; (* set initially *)

  notify_mvar                 : notify_v Lwt_mvar.t       ; (* set initially *)

  users                       : User.users                ; (* read from disk, extended by xmpp callbacks *)

  mutable active_contact      : string                    ; (* modified by scrolling *)
  mutable last_active_contact : string                    ; (* modified by scrolling *)

  mutable notifications       : string list               ; (* list to blink *)

  mutable show_offline        : bool                      ; (* F5 stuff *)
  mutable window_mode         : display_mode              ; (* F12 stuff *)
  mutable scrollback          : int                       ; (* scroll-pgup/down state *)

  mutable last_status         : (User.direction * string) ; (* internal use only *)

  mutable log_height          : int                       ;
  mutable buddy_width         : int                       ;
}

module Notify = struct
  type notify_writer_s = Q | D | C | D_N | C_N

  let to_string = function
    | Q -> "quit"
    | D -> "disconnected"
    | C -> "connected"
    | D_N -> "disconnected_notifications"
    | C_N -> "connected_notifications"

  let notify_writer user resource cb fname =
    let open Lwt.Infix in
    let mvar = Lwt_mvar.create Disconnected in
    let write_file buf =
      let open Lwt_unix in
      Lwt.catch (fun () ->
          openfile fname [O_WRONLY ; O_TRUNC ; O_CREAT ] 0o600 >>= fun fd ->
          Persistency.write_data fd buf >>= fun () ->
          close fd)
        (fun _ -> Lwt.return_unit) >>= fun () ->
      match cb with
      | None -> Lwt.return_unit
      | Some x ->
        Lwt.catch (fun () ->
            system (x ^ " " ^ user ^ "/" ^ resource ^ " " ^ buf) >>= fun _ ->
            Lwt.return_unit)
          (fun _ -> Lwt.return_unit)
    in
    let rec loop s0 =
      Lwt_mvar.take mvar >>= fun v ->
      let s1 =
        match v, s0 with
        | Quit, _ -> Q
        | Disconnected, C -> D
        | Disconnected, C_N -> D_N
        | Connected, D -> C
        | Connected, D_N -> C_N
        | Notifications, D -> D_N
        | Notifications, C -> C_N
        | Clear, C_N -> C
        | Clear, D_N -> D
        | _, _ -> s0
      in
      match s1 with
      | Q -> write_file (to_string Q)
      | s when s == s0 -> loop s
      | _ -> write_file (to_string s1) >>= fun () -> loop s1
    in
    Lwt.async (fun () -> loop C) ;
    mvar
end

let empty_ui_state config_directory notify_callback user resource users =
  let notify_mvar =
    let file = Filename.concat config_directory "notification.state" in
    Notify.notify_writer user resource notify_callback file in
  let last_status = (`Local "", "") in
  {
    config_directory                ;
    user                            ;
    resource                        ;

    notify_mvar                     ;

    users                           ;

    active_contact      = user      ;
    last_active_contact = user      ;

    notifications       = []        ;

    show_offline        = true      ;
    window_mode         = BuddyList ;
    scrollback          = 0         ;

    last_status                     ;

    log_height          = 6         ;
    buddy_width         = 24        ;
}


let add_status state dir msg =
  let self = User.Users.find state.users state.user in
  let user = User.insert_message self dir false true msg in
  User.Users.replace state.users state.user user

let (xmpp_session : Xmpp_callbacks.user_data Xmpp_callbacks.XMPPClient.session_data option ref) = ref None

let send s contact session id body fail =
  let (>>=) = Lwt.(>>=) in
  Xmpp_callbacks.send_msg s contact session id body fail >>= fun () ->
  Xmpp_callbacks.request_disco s contact.User.jid session.User.resource

let random_string () =
  let open Nocrypto in
  let rnd = Rng.generate 12 in
  Cstruct.to_string (Base64.encode rnd)

let cleanups users =
  User.reset_receipt_requests users ;
  Xmpp_callbacks.cancel_keepalive () ;
  Xmpp_callbacks.keepalive_running := false ;
  xmpp_session := None
