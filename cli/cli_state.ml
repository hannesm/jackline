
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
  myjid                       : Jid.full_jid               ; (* set initially *)

  state_mvar                  : notify_v Lwt_mvar.t       ; (* set initially *)
  user_mvar                   : User.user Lwt_mvar.t      ; (* set initially *)

  users                       : User.users                ; (* read from disk, extended by xmpp callbacks *)

  mutable active_contact      : Jid.t                     ; (* modified by scrolling *)
  mutable last_active_contact : Jid.t                     ; (* modified by scrolling *)

  mutable notifications       : Jid.t list                ; (* list to blink *)

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

  let notify_writer jid cb fname =
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
            system (x ^ " " ^ Jid.full_jid_to_string jid ^ " " ^ buf) >>= fun _ ->
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

let empty_ui_state config_directory notify_callback myjid users =
  let state_mvar =
    let file = Filename.concat config_directory "notification.state" in
    Notify.notify_writer myjid notify_callback file
  and user_mvar = Persistency.notify_user config_directory
  and last_status = (`Local "", "")
  and active = `Full myjid
  in
  {
    config_directory                ;
    myjid                           ;

    state_mvar                      ;
    user_mvar                       ;

    users                           ;

    active_contact      = active    ;
    last_active_contact = active    ;

    notifications       = []        ;

    show_offline        = true      ;
    window_mode         = BuddyList ;
    scrollback          = 0         ;

    last_status                     ;

    log_height          = 6         ;
    buddy_width         = 24        ;
}


let add_status state dir msg =
  User.add_message state.users (`Full state.myjid) dir false true msg

let (xmpp_session : Xmpp_callbacks.user_data Xmpp_callbacks.XMPPClient.session_data option ref) = ref None

let send s jid id body fail =
  Xmpp_callbacks.send_msg s jid id body fail

let random_string () =
  let open Nocrypto in
  let rnd = Rng.generate 12 in
  Cstruct.to_string (Base64.encode rnd)

let cleanups users =
  User.reset_receipt_requests users ;
  Xmpp_callbacks.cancel_keepalive () ;
  Xmpp_callbacks.keepalive_running := false ;
  xmpp_session := None

let notify state jid =
  if List.exists (fun x -> Jid.jid_matches x jid) state.notifications ||
       (Jid.jid_matches state.active_contact jid && state.scrollback = 0)
  then
    ()
  else
    state.notifications <- jid :: state.notifications ;
  Lwt.async (fun () -> Lwt_mvar.put state.state_mvar Notifications)

let notified state jid =
  state.notifications <- List.filter
                           (fun x -> not (Jid.jid_matches jid x))
                           state.notifications ;
  if List.length state.notifications = 0 then
    Lwt.async (fun () -> Lwt_mvar.put state.state_mvar Clear)
