
type display_mode =
  | BuddyList
  | FullScreen
  | Raw

let next_display_mode = function
  | BuddyList  -> FullScreen
  | FullScreen -> Raw
  | Raw        -> BuddyList

type ui_state = {
  config_directory            : string                    ; (* set initially *)
  user                        : string                    ; (* set initially *)
  resource                    : string                    ; (* set initially *)

  users                       : User.users                ; (* read from disk, extended by xmpp callbacks *)

  mutable active_contact      : string                    ; (* modified by scrolling *)
  mutable last_active_contact : string                    ; (* modified by scrolling *)

  mutable notifications       : string list               ; (* list to blink *)

  mutable show_offline        : bool                      ; (* F5 stuff *)
  mutable window_mode         : display_mode              ; (* F12 stuff *)
  mutable scrollback          : int                       ; (* scroll-pgup/down state *)

  mutable last_status         : (User.direction * string) ; (* internal use only *)
}

let empty_ui_state config_directory user resource users =
  let last_status = (`Local "", "") in
  {
    config_directory                ;
    user                            ;
    resource                        ;

    users                           ;

    active_contact      = user      ;
    last_active_contact = user      ;

    notifications       = []        ;

    show_offline        = true      ;
    window_mode         = BuddyList ;
    scrollback          = 0         ;

    last_status                     ;
}

let add_status state dir msg =
  let self = User.Users.find state.users state.user in
  let user = User.insert_message self dir false true msg in
  User.Users.replace state.users state.user user

let (xmpp_session : Xmpp_callbacks.user_data Xmpp_callbacks.XMPPClient.session_data option ref) = ref None

let send s users contact session id body fail =
  let jid = User.userid contact session in
  let x =
    match session.User.receipt with
    | `Supported ->
      let received = Xml.Xmlelement
          ((Xmpp_callbacks.XMPPClient.ns_receipts, "request"), [], [])
      in
      [received]
    | `Unsupported | `Unknown | `Requested -> []
  in
  let jid_to = JID.of_string jid in
  let find_user_session () =
    let id, r = User.bare_jid jid_to in
    match User.find users id with
    | Some u -> ( match User.find_session u r with
        | Some s -> (u, s)
        | None -> assert false )
    | None -> assert false
  in
  let (>>=) = Lwt.(>>=) in
  (try_lwt
     Xmpp_callbacks.XMPPClient.(send_message s ~kind:Chat ~jid_to ~body ~x ~id ())
   with e -> fail e) >>= fun () ->
  (try_lwt
     match session.User.receipt with
     | `Unknown ->
       let callback ev _jid_from _jid_to _lang () =
         let user, session = find_user_session () in
         let receipt = match ev with
           | Xmpp_callbacks.XMPPClient.IQResult el ->
             ( match el with
               | Some (Xml.Xmlelement ((Some "http://jabber.org/protocol/disco#info", "query"), _, els)) ->
               (* pick el with ns_receipts *)
                 if
                   List.exists (function
                       | Xml.Xmlelement ((_, "feature"), attrs, _) when
                           Xml.safe_get_attr_value "var" attrs = "urn:xmpp:receipts" -> true
                       | _ -> false) els
                 then
                   `Supported
                 else
                   `Unsupported
               | _ ->  `Unsupported )
           | Xmpp_callbacks.XMPPClient.IQError _ ->
             `Unsupported
         in
         let session = { session with User.receipt = receipt } in
         User.replace_session users user session ;
         Lwt.return_unit
       in
       let user, session = find_user_session () in
       let session = { session with User.receipt = `Requested } in
       User.replace_session users user session ;
       Xmpp_callbacks.XMPPClient.(make_iq_request s ~jid_to
                                    (IQGet (Xml.make_element (Some "http://jabber.org/protocol/disco#info", "query") [] [])) callback)
     | _ -> Lwt.return_unit
   with e -> fail e)


let random_string () =
  let open Nocrypto in
  let rnd = Rng.generate 12 in
  Cstruct.to_string (Base64.encode rnd)
