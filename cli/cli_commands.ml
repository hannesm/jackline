open Cli_state

open Lwt.Infix

let string_normalize_fingerprint fpstr =
  let fpstr = Astring.String.Ascii.lowercase fpstr in
  Astring.String.fold_right
    (fun c acc -> if Astring.Char.Ascii.is_hex_digit c then
                    Astring.String.of_char c ^ acc
                  else
                    acc)
    fpstr ""

type command = {
  name : string ;
  command_line : string ;
  documentation : string ;
  subcommands : state -> string list ;
}

module StringHash =
  struct
    type t = string
    let equal a b = a = b
    let hash = Hashtbl.hash
  end
module Commands = Hashtbl.Make(StringHash)
type commands = command Commands.t

let commands = Commands.create 10

let keys () = List.sort compare (Commands.fold (fun k _ acc -> k :: acc) commands [])

let new_command name command_line documentation subcommands =
  Commands.add commands name { name ; command_line ; documentation ; subcommands }

let _ =
  (* global *)
  new_command
    "connect" "/connect" "connects to the server" (fun _ -> []) ;
  new_command
    "disconnect" "/disconnect" "disconnects from the server" (fun _ -> []) ;
  new_command
    "quit" "/quit" "exits this client" (fun _ -> []) ;
  new_command
    "filter" "/filter [val]" "display only messages which contain val (empty string resets filter)" (fun _ -> []) ;
  new_command
    "ignore_presence" "/ignore_presence" "hide presence updates (join, away, etc)" (fun _ -> []) ;

  new_command
    "logheight" "/logheight [number]" "adjusts height of log to n" (fun _ -> []) ;
  new_command
    "buddywidth" "/buddywidth [number]" "adjusts width of buddy list to n" (fun _ -> []) ;

  (* global roster commands *)
  new_command
    "add" "/add [jid]"
    "adds jid to your contact list, and sends a subscription request" (fun _ -> []) ;

  new_command
    "go" "/go [jid]" "jump to contact jid" (fun s -> List.map Xjid.jid_to_string (all_jids s)) ;

  (* things affecting you *)
  new_command
    "status" "/status [presence] [message]"
    "sets your presence (free/away/dnd/xa/offline/online) and status message"
    (fun _ -> [ "free" ; "away" ; "dnd" ; "xa" ; "offline" ; "online" ]) ;
  new_command
    "priority" "/priority [number]" "set your presence priority to number" (fun _ -> []) ;

  (* user as context *)
  new_command
    "otrpolicy" "/otrpolicy +-[policy version]" "prints (without argument) or adjusts (prefix with add (+) or remove (-)) the otr policies and versions: require_encryption, send_whitespace_tag, whitespace_start_ake, error_start_ake, reveal_macs, v2, v3"
    (fun _ -> [ "+REQUIRE_ENCRYPTION" ; "+SEND_WHITESPACE_TAG" ; "+WHITESPACE_START_AKE" ; "+ERROR_START_AKE" ; "+REVEAL_MACS" ; "+V2" ; "+V3" ; "-REQUIRE_ENCRYPTION" ; "-SEND_WHITESPACE_TAG" ; "-WHITESPACE_START_AKE" ; "-ERROR_START_AKE" ; "-REVEAL_MACS" ; "-V2" ; "-V3" ]) ;
  new_command
    "log" "/log [on|off]" "enable or disable logging for current contact" (fun _ -> [ "on" ; "off" ]) ;
  new_command
    "clear" "/clear" "clears the active window chat backlog" (fun _ -> []) ;
  new_command
    "authorization" "/authorization [argument] [jid]"
    "changes presence subscription of jid or the current contact to argument -- one of 'allow', 'cancel', 'request', 'request_unsubscribe'"
    (fun _ -> [ "allow" ; "cancel" ; "request" ; "request_unsubscribe" ]) ;
  new_command
    "fingerprint" "/fingerprint [fp]"
    "marks the given OTR fingerprint verified for the current contact ; prints own and session fingerprint if no argument is given"
    (fun _ -> []) ;
  new_command
    "revoke" "/revoke [fp]"
    "revokes the given OTR fingerprint" (fun _ -> []) ;
  new_command
    "info" "/info" "displays information about the current session" (fun _ -> []) ;
  new_command
    "otr" "/otr [argument]" "manages OTR session by argument -- one of 'start' 'stop' or 'info'"
    (fun _ -> [ "start" ; "stop" ; "info" ]) ;
  new_command
    "smp" "/smp [argument]" "manages SMP session by argument -- one of 'shared [secret]', 'question [question]', 'answer' or 'abort'"
    (fun _ -> [ "shared" ; "question" ; "answer" ; "abort" ]) ;
  new_command
    "remove" "/remove" "remove current user from roster" (fun _ -> []) ;
  new_command
    "alias" "/alias [nick]"
    "add/remove local alias" (fun _ -> []) ;

  (* multi user chat *)
  new_command
    "join" "/join [?muc/nick] [password]" "join chatroom using nick (defaults to username) and password"
    (fun s ->
       " " :: List.map Xjid.bare_jid_to_string
         (Contact.fold (fun _ c acc ->
              match c with `Room r -> r.Muc.room_jid :: acc | _ -> acc)
             s.contacts [])) ;
  new_command
    "leave" "/leave [?reason]" "leaves active multi-user chat (with reason)" (fun _ -> []) ;
  new_command
    "rooms" "/rooms [server]" "queries chatrooms on server" (fun _ -> []) ;

  (* nothing below here, please *)
  new_command
    "help" "/help [cmd]" "shows available commands or detailed help for cmd"
    (fun _ -> keys ())

let split_ws s =
  match Astring.String.cut ~sep:" " s with
  | None -> (s, None)
  | Some (x, y) when y = "" -> (x, None)
  | Some (x, y) -> (x, Some y)

let cmd_arg input =
  let open String in
  let l = length input in
  assert (l > 0) ;
  assert (get input 0 = '/') ;
  split_ws (sub input 1 (pred l))

let might_match cmd prefix =
  let upper = min (String.length cmd) (String.length prefix) in
  let rec cmp_i idx =
    if idx < upper then
      if String.get cmd idx = String.get prefix idx then
        cmp_i (succ idx)
      else
        false
    else
      (String.length prefix <= String.length cmd)
  in
  cmp_i 0

let completion s input =
  let open Astring.String in
  let l = length input
  and ws = try String.index input ' ' > 0 with Not_found -> false
  in
  if l > 0 && get input 0 = '/' then
    match cmd_arg input with
    | (cmd, None) when ws && Commands.mem commands cmd ->
      (Commands.find commands cmd).subcommands s
    | (cmd, None) ->
      let cmds = keys () in
      (match List.filter (fun f -> might_match f cmd) cmds with
       | [] -> []
       | [_] when Commands.mem commands cmd -> (Commands.find commands cmd).subcommands s
       | xs -> List.map (drop ~max:(pred l)) xs)
    | (cmd, Some arg) when Commands.mem commands cmd ->
      let command = Commands.find commands cmd in
      List.map (fun x -> drop ~max:(pred l) (cmd ^ " " ^ x))
        (List.filter (fun f -> might_match f arg) (command.subcommands s))
    | _ -> []
  else if l > 0 then
    match active s with
    | `Room r ->
      List.map (drop ~max:l)
        (List.filter
           (Astring.String.is_prefix ~affix:input)
           (List.map (fun m -> m.Muc.nickname)
              (List.filter (fun m -> m.Muc.presence <> `Offline) r.Muc.members)))
    | _ -> []
  else
    []

let handle_help = function
  | Some arg when Commands.mem commands arg ->
    let cmd = Commands.find commands arg in
    (cmd.command_line, cmd.documentation)
  | _ ->
    let cmds = String.concat " " (keys ()) in
    ("available commands (/help [cmd])", cmds)

let notify_user user jid ctx inc_fp verify_fp = function
  | `Established_encrypted_session ssid ->
     let raw_fp = match User.otr_fingerprint ctx with Some fp -> fp | _ -> assert false in
     let user, otrmsg, kind =
       let verify = " verify /fingerprint [fp] over second channel"
       and tos x =
         let stat = User.verification_status_to_string x in
         stat ^ " key"
       and other x =
         if x then
           ", but a verified is available"
         else
           ""
       and count n =
         if n = 0 then
           " (never used before)"
         else
           " (used " ^ (string_of_int n) ^ " times)"
       in
       let user, v, c, o = inc_fp user raw_fp in
       match v with
       | `Verified _ -> user, tos v, `Success
       | _ when c = 0 && o -> user, "POSSIBLE BREAKIN ATTEMPT! new " ^ tos v ^ other o ^ verify, `Error
       | `Revoked _ -> user, tos v ^ count c ^ other o ^ verify, `Error
       | _ -> user, tos v ^ count c ^ other o ^ verify, `Warning
     in
     user,
     [ ((`Local (jid, "OTR")), `Success, false, ("encrypted connection established (ssid " ^ ssid ^ ")")) ;
       ((`Local (jid, "OTR key")), kind, false, otrmsg) ]
  | `Warning w               -> user, [ ((`Local (jid, "OTR warning")), `Warning, false, w) ]
  | `Received_error e        -> user, [ ((`From jid), `Error, false, e) ]
  | `Received m              -> user, [ ((`From jid), `Chat, false, m) ]
  | `Received_encrypted e    -> user, [ ((`From jid), `Chat, true, e) ]
  | `SMP_awaiting_secret     -> user, [ ((`Local (jid, "SMP")), `Info, false, "awaiting SMP secret, answer with /smp answer [secret]") ]
  | `SMP_received_question q -> user, [ ((`Local (jid, "SMP")), `Info, false, ("received SMP question (answer with /smp answer [secret]) " ^ q)) ]
  | `SMP_success             ->
     let raw_fp = match User.otr_fingerprint ctx with Some fp -> fp | _ -> assert false in
     verify_fp user raw_fp,
     [ ((`Local (jid, "OTR SMP")), `Success, false, "successfully verified!") ]
  | `SMP_failure             -> user, [ ((`Local (jid, "OTR SMP")), `Error, false, "failure") ]

let handle_connect p c_mvar =
  let find_session state user resource =
    match
      User.find_session user resource,
      User.find_similar_session user resource
    with
    | Some s, _ -> (state, s)
    | _, Some s ->
      let new_session = { s with User.resource ; presence = `Offline ; priority = 0 ; status = None }
      and similar = { s with User.dispose = true }
      in
      let state, u =
        let u, removed = User.replace_session user similar in
        let u, removed' = User.replace_session u new_session in
        if removed || removed' then
          (update_notifications state u similar.User.resource new_session.User.resource, u)
        else
          (state, u)
      in
      Contact.replace_user state.contacts u ;
      (state, new_session)
    | _ ->
      let otr_config = otr_config user state in
      let u, s = User.create_session user resource otr_config state.config.Xconfig.dsa in
      Contact.replace_user state.contacts u ;
      (state, s)
  in

  let log ?(kind = `Warning) dir msg =
    let exec state = add_status ~kind state dir (Utils.validate_utf8 msg) ; Lwt.return (`Ok state) in
    p exec
  and locallog ?(kind = `Warning) str msg =
    let exec state =
      let dir = `Local (state.active_contact, str) in
      add_status ~kind state dir (Utils.validate_utf8 msg) ;
      Lwt.return (`Ok state)
    in
    p exec

  and message bare res ?timestamp txt =
    let exec state =
      match Contact.find_user state.contacts bare with
      | None ->
        let from = match res with None -> `Bare bare | Some r -> `Full (bare, r) in
        add_status ?timestamp ~kind:`Warning state (`From from) (Utils.validate_utf8 txt) ;
        Lwt.return (`Ok state)
      | Some user ->
        let res = match res with None -> "" | Some r -> r in
        let state, session = find_session state user res in
        let ctx, out, ret = Otr.Engine.handle session.User.otr txt in
        let user = User.update_otr user session ctx in
        let inc_fp user raw_fp =
          let fp = User.find_raw_fp user raw_fp in
          let u = User.used_fp user fp res in
          let isverified fp =
            match fp.User.verified with
            | `Verified _ -> true
            | _ -> false
          in
          (u,
           fp.User.verified,
           fp.User.session_count,
           List.exists isverified user.User.otr_fingerprints)
        and verify_fp user raw_fp =
          let fp = User.find_raw_fp user raw_fp in
          User.verify_fp user fp `SMP
        in
        let user, msgs = List.fold_left (fun (user, msgs) data ->
            let user, msg = notify_user user (`Full (bare, res)) ctx inc_fp verify_fp data in
            user, msgs@msg)
            (user, []) ret
        in
        let user, mark = List.fold_left
            (fun (u, n) (dir, kind, enc, m) ->
               let m =
                 let m = Utils.validate_utf8 m in
                 let m = Escape.strip_tags m in
                 Escape.unescape m
               in
               (User.insert_message ?timestamp ~kind u dir enc true m,
                match dir with
                | `Local (_, s) when Astring.String.is_prefix ~affix:"OTR" s -> n
                | _ -> true))
            (user, false)
            msgs
        in
        Contact.replace_user state.contacts user ;
        Lwt_mvar.put state.contact_mvar (`User user) >>= fun () ->
        (match timestamp, out, !xmpp_session with
         | Some _, _, _ -> Lwt.return_unit
         | _, None, _ -> Lwt.return_unit
         | _, _, None -> Lwt.return_unit
         | _, Some body, Some s ->
           send s (Some session) ~kind:Xmpp_callbacks.XMPPClient.Chat (`Full (bare, res)) None body) >>= fun () ->
        let state = if mark then notify state (`Full (bare, res)) else state in
        Lwt.return (`Ok state)
    in
    p exec
  and group_message jid timestamp topic body _data id =
    let exec state =
      let body = Utils.option None (fun x -> Some (Utils.validate_utf8 x)) body in
      match Contact.find_room state.contacts (Xjid.t_to_bare jid) with
      | None ->
        add_status ?timestamp ~kind:`Warning state (`From jid) (match body with Some x -> x | None -> "") ;
        Lwt.return (`Ok state)
      | Some room ->
        let room = Utils.option room (fun x -> { room with Muc.topic = Some (Utils.validate_utf8 x) }) topic in
        let state, room = match timestamp, body with
          | _, None -> (state, room)
          | Some timestamp, Some msg ->
            (* this is some chat history *)
            let dir = match Xjid.resource jid with
              | Some x when x = room.Muc.my_nick -> `To (`Bare room.Muc.room_jid, "")
              | _ -> `From jid
            in
            let msg = User.message ~timestamp ~kind:`GroupChat dir false true msg in
            (state, Muc.new_message room msg)
          | None, Some msg ->
            match id, Xjid.resource jid with
            | Some id, Some x when x = room.Muc.my_nick ->
              (match Contact.received (`Room room) id with
               | `Room r -> (state, r)
               | _ -> assert false)
            | None, Some x when x = room.Muc.my_nick ->
              (* no need to repeat ourselves -- we should attempt to mark the correct message as delivered
                 but (for slack) there are some escape issues:
                     our &quot; and &apos; are translated to "" and ''
                     --> String.compare won't do *)
              (state, room)
            | _ ->
              let state = notify state (`Bare room.Muc.room_jid) in
              let msg = User.message ?timestamp ~kind:`GroupChat (`From jid) false true msg in
              (state, Muc.new_message room msg)
        in
        Contact.replace_room state.contacts room ;
        Lwt.return (`Ok state)
    in
    p exec

  and received_receipts jid ids =
    let exec state =
      match Contact.find_user state.contacts (Xjid.t_to_bare jid) with
      | None -> Lwt.return (`Ok state)
      | Some user ->
        let buddy = List.fold_left Contact.received (`User user) ids in
        Contact.replace_contact state.contacts buddy ;
        Lwt.return (`Ok state)
    in
    p exec
  and update_receipt_state jid receipt =
    let exec state =
      match jid with
      | `Bare _ -> Lwt.return (`Ok state)
      | `Full (b, r) ->
        match Contact.find_user state.contacts b with
        | None -> Lwt.return (`Ok state)
        | Some user ->
          let state, session = find_session state user r in
          let u, _ = User.replace_session user { session with User.receipt } in
          Contact.replace_user state.contacts u ;
          Lwt.return (`Ok state)
    in
    p exec

  and subscription jid smod status =
    let exec state =
      let status = Utils.option None (fun x -> Some (Utils.validate_utf8 x)) status in
      let bare = Xjid.t_to_bare jid in
      let post = Utils.option "" (fun x -> " - " ^ x) status in
      let x, txt = match smod with
        | `Probe -> "probed", post
        | `Subscribe -> "subscription request", ("/authorization allow|cancel" ^ post)
        | `Subscribed -> "you have been subscribed to their presence", post
        | `Unsubscribe -> "unsubscription request", ("/authorization allow|cancel" ^ post)
        | `Unsubscribed -> "you have been unsubscribed from their presence", post
      in
      match Contact.find_user state.contacts bare with
      | None ->
        (* I've the feeling we need something special here *)
        add_status ~kind:`Warning state (`From jid) (x ^ txt) ;
        Lwt.return (`Ok state)
      | Some user ->
        let user = User.insert_message user (`Local (jid, x)) false true txt in
        Contact.replace_user state.contacts user ;
        let state = notify state (`Bare bare) in
        Lwt.return (`Ok state)
    in
    p exec
  in
  let presence_msg old now status =
    let pre = match old, now with
      | `Offline, `Online -> "joined"
      | `Offline, `Free -> "joined"
      | `Offline, x -> "joined (" ^ User.presence_to_string x ^ ")"
      | _, `Offline -> "left"
      | x, y ->
        let oldc = User.presence_to_char x
        and nowc = User.presence_to_char y
        and nows = User.presence_to_string y
        in
        oldc ^ "->" ^ nowc ^ " (" ^ nows ^ ")"
    in
    let status = Utils.option "" (fun x -> " - " ^ x) status in
    pre ^ status
  in
  let presence jid presence priority status =
    let exec state =
      let status = Utils.option None (fun x -> Some (Utils.validate_utf8 x)) status in
      let update_session session =
        { session with User.presence ; status ; priority }
      in
      let maybe_update_presence state user session =
        if User.presence_unmodified session presence status priority then
          Lwt.return (`Ok state)
        else
          let old = session.User.presence in
          let session = update_session session in
          let user, removed = User.replace_session user session in
          Contact.replace_user state.contacts user ;
          add_status ~kind:`Presence state (`From jid) (presence_msg old session.User.presence status) ;
          let state =
            if removed then
              Utils.option
                state
                (fun x -> update_notifications state user x.User.resource session.User.resource)
                (User.find_similar_session user session.User.resource)
            else
              state
          in
          Lwt.return (`Ok state)
      in
      match jid with
        | `Bare b ->
          (* this happens in several situations:
             - if a@foo.com and b@bar.com are subscribed, a logs in: bar.com sends a "b is offline" presence
             - in slack's xmpp server (likely others), nobody has a resource, but everything originates from the bare jid *)
          begin match Contact.find_user state.contacts b with
            | Some user ->
              let user, session = match User.sorted_sessions user with
                | x::_ -> (user, x)
                | [] ->
                  let otr_config = otr_config user state in
                  let u, s = User.create_session user "" otr_config state.config.Xconfig.dsa in
                  Contact.replace_user state.contacts u ;
                  (u, s)
              in
              let active_sessions = List.map update_session user.User.active_sessions in
              let user = { user with User.active_sessions } in
              Contact.replace_user state.contacts user ;
              maybe_update_presence state user session
            | None ->
              add_status ~kind:`Warning state (`From jid) (presence_msg `Offline presence status) ;
              Lwt.return (`Ok state)
          end
        | `Full (b, r) ->
          begin match Contact.find_user state.contacts b with
            | Some user ->
              let state, session = find_session state user r in
              maybe_update_presence state user session
            | None ->
              add_status ~kind:`Warning state (`From jid) (presence_msg `Offline presence status) ;
              Lwt.return (`Ok state)
          end
    in
    p exec
  and group_presence jid presence status data =
    let exec state =
      let status = Utils.option None (fun x -> Some (Utils.validate_utf8 x)) status in
      match jid with
      | `Bare _ -> Lwt.return (`Ok state)
      | `Full (bare, nickname) ->
        match Contact.find_room state.contacts bare with
        | None -> Lwt.return (`Ok state)
        | Some r ->
          let real_jid, nick, affiliation, role =
            let open Xmpp_callbacks.Xep_muc in
            let to_affiliation = function
              | AffiliationOwner -> `Owner
              | AffiliationAdmin -> `Admin
              | AffiliationMember -> `Member
              | AffiliationOutcast -> `Outcast
              | AffiliationNone -> `None
            and to_role = function
              | RoleModerator -> `Moderator
              | RoleParticipant -> `Participant
              | RoleVisitor -> `Visitor
              | RoleNone -> `None
            in
            match data.User.item with
            | None -> assert false
            | Some x ->
              (Utils.option None (fun x -> Some (Xjid.xmpp_jid_to_jid x)) x.User.jid,
               Utils.option None (fun x -> Some x) x.User.nick,
               Utils.option None (fun x -> Some (to_affiliation x)) x.User.affiliation,
               Utils.option None (fun x -> Some (to_role x)) x.User.role)
          in
          let nick = Utils.option nickname (fun x -> x) nick in
          let affiliation = Utils.option `None (fun x -> x) affiliation
          and role = Utils.option `None (fun x -> x) role
          in
          (if nickname = r.Muc.my_nick && presence = `Offline then
             let r = Muc.reset_room r in
             Contact.replace_room state.contacts r
           else
             let old, members = match Muc.member r jid with
               | None ->
                 `Offline,
                 Muc.new_member nick ~jid:real_jid affiliation role presence status :: r.Muc.members
               | Some m ->
                 m.Muc.presence,
                 { Muc.nickname = nick ; jid = real_jid ; affiliation ; role ; presence ; status } ::
                 List.filter (fun m -> m.Muc.nickname <> nick) r.Muc.members
             in
             let r = { r with Muc.members } in
             let r =
               if old = presence then
                 r
               else
                 let msg = presence_msg old presence status in
                 let msg = User.message ~kind:`Presence (`From jid) false true msg in
                 Muc.new_message r msg
             in
             Contact.replace_room state.contacts r );
          Lwt_mvar.put state.contact_mvar (`Room r) >>= fun () ->
          Lwt.return (`Ok state)
    in
    p exec

  and reset_users () =
    let exec state =
      let all_users =
        Contact.fold
          (fun _ c acc ->
             match c with
             | `User u -> { u with User.subscription = `None } :: acc
             | `Room _ -> acc)
          state.contacts
          []
      in
      List.iter (fun u ->
          Contact.replace_user state.contacts u ;
          Lwt.async (fun () -> Lwt_mvar.put state.contact_mvar (`User u)))
        all_users ;
      Lwt.return (`Ok state)
    in
    p exec
  and update_users users alert =
    let exec state =
      let single (jid, name, groups, subscription, properties) =
        let user =
          let jid = Xjid.t_to_bare jid in
          match Contact.find_user state.contacts jid with
          | Some user -> user
          | None ->
            let u = User.new_user ~jid () in
            Contact.replace_user state.contacts u ;
            u
        in
        let user = { user with User.name ; groups ; subscription ; properties } in
        Contact.replace_user state.contacts user ;
        Lwt_mvar.put state.contact_mvar (`User user) >|= fun () ->
        `Bare user.User.bare_jid
      in
      Lwt_list.map_s single users >>= fun ids ->
      let state =
        if alert then
          List.fold_left notify state ids
        else
          state
      in
      Lwt.return (`Ok state)
    in
    p exec
  and create_room jid autojoin =
    let exec state =
      let state = match Contact.find_room state.contacts jid with
        | Some _ -> state
        | None ->
          let my_nick = fst (fst state.config.Xconfig.jid) in
          let room = Muc.new_room ~jid ~my_nick ~autojoin () in
          Contact.replace_room state.contacts room ;
          state
      in
      Lwt.return (`Ok state)
    in
    p exec
  in
  let (user_data : Xmpp_callbacks.user_data) = {
      Xmpp_callbacks.log ;
      locallog ;

      message ;
      group_message ;

      received_receipts ;
      update_receipt_state ;

      subscription ;
      presence ;
      group_presence ;

      create_room ;

      reset_users ;
      update_users ;
  }
  in
  Lwt_mvar.put c_mvar (Connect user_data)

let handle_disconnect err =
  Connect.disconnect () >>= fun () ->
  err "disconnected"

let send_status s (presence, status, priority) =
  let kind, show = Xmpp_callbacks.presence_to_xmpp presence in
  Xmpp_callbacks.XMPPClient.send_presence s ?kind ?show ?status ?priority ()

let handle_status session arg =
  let p, status = split_ws arg in
  let priority = match session.User.priority with 0 -> None | x -> Some x in
  match User.string_to_presence p with
  | None   -> None
  | Some x -> Some (x, status, priority)

let handle_priority session p =
  try
    let prio = int_of_string p in
    assert (prio >= -128 && prio <= 127) ; (* RFC 6121 4.7.2.3 *)
    let priority = match prio with 0 -> None | x -> Some x in
    Some (session.User.presence, session.User.status, priority)
  with
    _ -> None

let handle_add state s a (msg : (?kind:User.chatkind -> string -> string -> unit)) =
  try
    (* TODO: validate input here *)
    match Xjid.string_to_bare_jid a with
    | None ->
      msg ~kind:`Error "error" "parsing of jid failed (should be user@node)" ;
      Lwt.return (`Ok state)
    | Some jid ->
      let jid_to = Xjid.jid_to_xmpp_jid (`Bare jid) in
      Xmpp_callbacks.XMPPClient.(send_presence s ~jid_to ~kind:Subscribe ()) >|= fun () ->
      msg a "has been subscribed (approval pending)" ;
      let user = User.new_user ~jid () in
      Contact.replace_user state.contacts user ;
      `Ok state
  with _ ->
    msg ~kind:`Error "error" "parsing of jid failed (user@node)" ;
    Lwt.return (`Ok state)

let handle_fingerprint readonly_state user err fp =
  let manual_fp = string_normalize_fingerprint fp in
  if String.length manual_fp = 40 then
    if `Hex manual_fp = Hex.of_string (Otr.Utils.own_fingerprint readonly_state.config.Xconfig.dsa) then
      err "You tried to add your own OTR fingerprint, not your contact's."
    else
    let fp = User.find_raw_fp user manual_fp in
    let user = User.verify_fp user fp `Manual in
    (["verified " ^ manual_fp], Some (`User user), None)
  else
    err "not a hex-encoded OTR fingerprint"

let handle_revoke user err fp =
  let manual_fp = string_normalize_fingerprint fp in
  if String.length manual_fp = 40 then
    let fp = User.find_raw_fp user manual_fp in
    let user = User.revoke_fp user fp in
    (["revoked " ^ manual_fp], Some (`User user), None)
  else
    err "not a hex-encoded OTR fingerprint"

let history cfgdir contact =
  let jid = Xjid.bare_jid_to_string (Contact.bare contact)
  and dir = Persistency.history
  and cwd = Sys.getcwd ()
  in
  Filename.(concat (concat (concat cwd cfgdir) dir) jid)

let handle_log buddy v a cfgdir =
  if Contact.preserve_messages buddy <> v then
    let buddy = Contact.set_preserve_messages buddy v in
    let msg =
      let file = history cfgdir buddy in
      ("logging turned " ^ a) ::
        if not v && Sys.file_exists file then
          [ "please delete the log file (" ^ file ^ ") manually" ]
        else
          []
    in
    (msg, Some buddy, None)
  else
    ([], None, None)

let handle_authorization arg =
  let parse_a =
    let open Xmpp_callbacks.XMPPClient in
    function
    | "allow"               -> Some (Subscribed, "is now allowed to receive your presence updates")
    | "cancel"              -> Some (Unsubscribed, "won't receive your presence updates anymore")
    | "request"             -> Some (Subscribe, "has been asked to sent presence updates to you")
    | "request_unsubscribe" -> Some (Unsubscribe, "has been asked to no longer sent presence updates to you")
    | _                     -> None
  in
  let a, jid = split_ws arg in
  let jid = match jid with None -> None | Some jid -> Xjid.string_to_bare_jid jid in
  parse_a a, jid

let dump_otr_fps fps =
  "otr fingerprints:" :: List.map User.fingerprint_to_string fps

let current_otr_fp session =
  Utils.option
    []
    (fun s -> Utils.option
                ["no active OTR session"]
                (fun fp -> ["their otr fingerprint: " ^ (User.pp_fingerprint fp)])
                (User.otr_fingerprint s.User.otr))
    session

let handle_otr_info user session =
  let sessions =
    List.map (fun s ->
      let act = match session with
        | Some x when x = s -> "(active) "
        | _ -> ""
      in
      act ^ s.User.resource ^ ": " ^ Otr.State.session_to_string s.User.otr)
      (User.sorted_sessions user)
  in
  sessions @ dump_otr_fps user.User.otr_fingerprints

let handle_own_otr_info dsa =
  let otr_fp = Otr.Utils.own_fingerprint dsa in
  ["your otr fingerprint:  " ^ (User.pp_binary_fingerprint otr_fp)]

let common_info user cfgdir =
  let jid = Xjid.bare_jid_to_string (Contact.bare user) in
  let name = Utils.option [] (fun x -> [x]) (Contact.name user)
  and pres =
    let history = history cfgdir user in
    if Sys.file_exists history then
      ["persistent history in: " ^ history]
    else
      []
  and logging =
    if Contact.preserve_messages user then
      ["communication is LOGGED TO DISK"]
    else
      ["communication is not logged to disk"]
  in
  [ "jid: " ^ jid ] @ name @ pres @ logging

let handle_info buddy resource cfgdir =
  common_info buddy cfgdir @ Contact.info buddy resource

let handle_own_info user session cfgdir dsa =
  let ci = common_info (`User user) cfgdir
  and otr_fp = handle_own_otr_info dsa
  and sessions = User.info user (Some session)
  in
  ci @ otr_fp @ sessions

let handle_otr_start user session otr_cfg dsa =
  match session with
  | None ->
    (* no OTR context, but we're sending only an OTR query anyways
       (and if we see a reply, we'll get some resource from the other side) *)
    let ctx = Otr.State.new_session otr_cfg dsa () in
    let _, out = Otr.Engine.start_otr ctx in
    let clos state s =
      send s None (`Bare user.User.bare_jid) None out >|= fun () -> `Ok state
    in
    ([ "starting OTR session" ], None, Some clos)
  | Some session when User.encrypted session.User.otr ->
    ([ "session is already encrypted, please finish first (/otr stop)!" ], None, None)
  | Some session ->
    let ctx, out = Otr.Engine.start_otr session.User.otr in
    let user = User.update_otr user session ctx in
    let clos state s =
      send s (Some session) (`Full (user.User.bare_jid, session.User.resource)) None out >|= fun () -> `Ok state
    in
    ([ "starting OTR session" ], Some (`User user), Some clos)

let handle_otr_stop user session err =
  match session with
  | None -> err "no active session"
  | Some session ->
    let ctx, out = Otr.Engine.end_otr session.User.otr in
    let user = User.update_otr user session ctx in
    let datas, clos = match out with
      | None   -> ([], None)
      | Some body ->
         let clos state s =
           let jid = `Full (user.User.bare_jid, session.User.resource) in
           send s (Some session) jid None body >|= fun () ->
           `Ok state
         in
         ([ "finished OTR session" ], Some clos)
    in
    (datas, Some (`User user), clos)

let handle_smp_abort user session =
  let ctx, out, ret = Otr.Engine.abort_smp session.User.otr in
  let user = User.update_otr user session ctx in
  let datas = List.fold_left (fun ds -> function
      | `Warning x -> ("SMP abort warning: " ^ x) :: ds
      | _ -> ds)
    []
    (List.rev ret)
  in
  let clos = match out with
   | None -> None
   | Some out ->
      Some (fun state s ->
            let jid = `Full (user.User.bare_jid, session.User.resource) in
            send s (Some session) jid None out >|= fun () -> `Ok state)
  in
  (datas, Some (`User user), clos)

let handle_smp_shared user session secret =
  let sec = Astring.String.trim secret in
  let ctx, out, ret = Otr.Engine.start_smp session.User.otr sec in
  let user = User.update_otr user session ctx in
  let datas = List.fold_left (fun ds -> function
      | `Warning x -> ("SMP start warning: " ^ x) :: ds
      | _ -> ds )
    []
    (List.rev ret)
  in
  let datas = if sec <> secret then "trimmed secret" :: datas else datas in
  let clos =
    match out with
    | None   -> None
    | Some body ->
       Some (fun state s ->
             let jid = `Full (user.User.bare_jid, session.User.resource) in
             send s (Some session) jid None body >|= fun () -> `Ok state)
  in
  (datas @ ["initiated SMP"], Some (`User user), clos)

let handle_smp_question user session question =
  let p = "shared secret:" in
  let clos _state s =
    let jid = `Full (user.User.bare_jid, session.User.resource) in
    let handle state input_mvar ui_mvar =
      reading := false ;
      Lwt.async (fun () ->
          Lwt_mvar.take input_mvar >>= fun secret ->
          reading := true ;
          let sec =
            let s =
              if Astring.String.is_prefix ~affix:p secret then
                Astring.String.drop ~max:(Astring.String.length p) secret
              else
                secret
            in
            Astring.String.trim s
          in
          let handle state =
            match Contact.find_user state.contacts user.User.bare_jid with
            | None -> assert false
            | Some user -> match User.find_session user session.User.resource with
              | None -> assert false
              | Some session ->
                let ctx, out, ret = Otr.Engine.start_smp session.User.otr ~question sec in
                let user = User.update_otr user session ctx in
                let add_msg u m = User.insert_message ~kind:`Info u (`Local (jid, "")) false false m in
                let user = add_msg user ("asked SMP " ^ question) in
                let user = List.fold_left (fun c -> function
                    | `Warning x -> add_msg c ("SMP question warning: " ^ x)
                    | _ ->  c)
                    user (List.rev ret)
                in
                Contact.replace_user state.contacts user ;
                match out with
                | None      -> Lwt.return (`Ok state)
                | Some body -> send s (Some session) jid None body >|= fun () -> `Ok state
          in
          Lwt_mvar.put ui_mvar handle) ;
      Lwt.return { state with input = (Cli_support.str_to_char_list (p ^ " "), []) }
    in
    Lwt.return (`Ask handle)
  in
  ([], None, Some clos)


let handle_smp_answer user session secret =
  let sec = Astring.String.trim secret in
  let ctx, out, ret = Otr.Engine.answer_smp session.User.otr sec in
  let user = User.update_otr user session ctx in
  let datas = List.fold_left (fun ds -> function
      | `Warning x -> ("SMP answer warning: " ^ x) :: ds
      | _ -> ds)
    []
    (List.rev ret)
  in
  let datas = if sec <> secret then "trimmed secret" :: datas else datas in
  let clos =
    match out with
    | None   -> None
    | Some body ->
       Some (fun state s ->
             let jid = `Full (user.User.bare_jid, session.User.resource) in
             send s (Some session) jid None body >|= fun () ->
             `Ok state)
  in
  (datas, Some (`User user), clos)

let handle_remove user =
  fun state s ->
    let bare_jid = Xjid.bare_jid_to_string user.User.bare_jid in
    Xmpp_callbacks.(Roster.put ~remove:() s bare_jid
      ~error_callback:(fun se ->
        let str = stanza_error_to_str se in
        s.XMPPClient.user_data.log (`From (`Bare user.User.bare_jid)) ("Failed to remove: " ^ str))
      (fun ?jid_from ?jid_to ?lang el ->
        ignore jid_to ; ignore lang ; ignore el ;
        match jid_from with
        | None -> Lwt.fail XMPPClient.BadRequest
        | Some x -> match Xjid.string_to_jid x with
          | None -> Lwt.fail XMPPClient.BadRequest
          | Some jid ->
            s.XMPPClient.user_data.log ~kind:`Info (`From jid) ("Removal of " ^ bare_jid ^ " successful"))) >|= fun () -> `Ok state

let print_otr_policy cfg =
  let policies = String.concat ", "
      (List.map Otr.State.policy_to_string cfg.Otr.State.policies)
  and versions = String.concat ", "
      (List.map Otr.State.version_to_string cfg.Otr.State.versions)
  in
  ["OTR versions: " ^ versions ^ " policies: " ^ policies]

let adjust_otr_policy default_cfg cfg contact data err =
  let try_decode str =
    Otr.State.string_to_policy str, Otr.State.string_to_version str
  in
  let rec parse_elements pols vers left =
    if String.length left > 0 then
      let arg, rest = split_ws left in
      let first, arg = String.get arg 0, String.sub arg 1 (pred (String.length arg)) in
      let pols, vers = match first, try_decode (Astring.String.Ascii.uppercase arg) with
        | '+', (Some pol, None) when List.mem pol pols -> pols, vers
        | '+', (Some pol, None) -> pol :: pols, vers
        | '-', (Some pol, None) -> List.filter (fun x -> x <> pol) pols, vers
        | '+', (None, Some ver) when List.mem ver vers -> pols, vers
        | '+', (None, Some ver) -> pols, ver :: vers
        | '-', (None, Some ver) -> pols, List.filter (fun x -> x <> ver) vers
        | _ -> assert false
      in
      match rest with
      | None -> pols, vers
      | Some x -> parse_elements pols vers x
    else
      pols, vers
  in
  try
    let old_p = cfg.Otr.State.policies
    and old_v = cfg.Otr.State.versions
    in
    let pols, vers = parse_elements old_p old_v data in
    if pols <> old_p || old_v <> vers then
      let cfg =
        if pols = default_cfg.Otr.State.policies && vers = default_cfg.Otr.State.versions then
          None
        else
          let otr_custom_config = Otr.State.config vers pols in
          Some otr_custom_config
      in
      let active_sessions =
        let cfg = match cfg with None -> default_cfg | Some x -> x in
        List.map
          (fun x ->
           let otr = Otr.State.update_config cfg x.User.otr in
           { x with User.otr })
          contact.User.active_sessions
      in
      let datas =
        match cfg with
        | None -> ["reverted to default otr policy"]
        | Some x -> print_otr_policy x
      in
      let user = { contact with User.otr_custom_config = cfg ; active_sessions } in
      (datas, Some (`User user), None)
    else
      (["/otrpolicy: nothing changed"], None, None)
  with
    _ -> err "/otrpolicy: unable to parse argument"

let handle_join ?maxstanzas nick r =
  let join ?password jid my_nick =
    let clos state s =
      let join r =
        Xmpp_callbacks.Xep_muc.enter_room s ?maxstanzas ~nick:my_nick ?password (Xjid.jid_to_xmpp_jid (`Bare jid)) >|= fun () ->
        Contact.replace_room state.contacts r ;
        `Ok state
      in
      match Contact.find_room state.contacts jid with
      | None ->
        let room = Muc.new_room ?password ~jid ~my_nick () in
        join room
      | Some r -> match Muc.member r (`Full (("",""), r.Muc.my_nick)) with
        | None -> join { r with Muc.autojoin = true }
        | Some x when x.Muc.presence = `Offline -> join { r with Muc.autojoin = true }
        | _ ->
          add_status ~kind:`Error state (`Local (state.active_contact, "")) "/join: already in that room" ;
          Lwt.return (`Ok state)
    in
    (["joining room " ^ r], None, Some clos)
  in
  let jid, password = match Astring.String.cut ~sep:" " r with
    | None -> r, None
    | Some (a, b) -> a, Some b
  in
  match Xjid.string_to_jid jid with
  | Some (`Bare jid) -> join ?password jid nick
  | Some (`Full (bare, res)) -> join ?password bare res
  | None -> ([r ^ " is not a good jid"], None, None)

let handle_leave buddy reason =
  match buddy with
  | `Room r ->
     let nick = r.Muc.my_nick
     and jid = `Bare r.Muc.room_jid
     in
     let clos state s =
       Xmpp_callbacks.Xep_muc.leave_room s ?reason ~nick (Xjid.jid_to_xmpp_jid jid) >|= fun () ->
       `Ok state
     in
     let r = { r with Muc.autojoin = false } in
     (["leaving room " ^ Xjid.bare_jid_to_string r.Muc.room_jid], Some (`Room r), Some clos)
  | _ -> ([Xjid.bare_jid_to_string (Contact.bare buddy) ^ " is not a chatroom"], None, None)

let handle_rooms host =
  let clos state s =
    let open Xmpp_callbacks in
    let callback ev jid_from _jid_to _lang () =
      let jid_from = match jid_from with
        | None -> `Bare ("", "none")
        | Some x -> match Xjid.string_to_jid x with
          | Some x -> x
          | None -> `Bare ("", x)
      in
      match ev with
      | XMPPClient.IQError _ -> s.XMPPClient.user_data.log (`From jid_from) "couldn't find any rooms"
      | XMPPClient.IQResult el ->
        match el with
        | Some (Xml.Xmlelement ((ns, "query"), _, els)) when ns = Disco.ns_disco_items ->
          let name_id = List.fold_left (fun acc ele ->
              match ele with
              | Xml.Xmlelement ((_, "item"), attrs, _) ->
                (Xml.safe_get_attr_value "name" attrs, Xml.safe_get_attr_value "jid" attrs) :: acc
              | _ -> acc) [] els
          in
          let rs, xs =
            List.fold_left (fun (r, s) (name, jid) ->
                match Xjid.string_to_bare_jid jid with
                | None -> (r, (name, jid) :: s)
                | Some bare -> ((bare, name) :: r, s))
              ([], []) name_id
          in
          let servers things =
             let str = String.concat ", " (List.map (fun (name, jid) -> name ^ " (" ^ jid ^ ")") things) in
             s.XMPPClient.user_data.log ~kind:`Info (`From jid_from) ("servers: " ^ str)
          and rooms rooms =
             let str = String.concat ", " (List.map (fun ((user, _), name) -> name ^ " (" ^ user ^ "@)") rooms) in
             s.XMPPClient.user_data.log ~kind:`Info (`From jid_from) ("rooms: " ^ str) >>= fun () ->
             Lwt_list.iter_s
               (fun id -> s.XMPPClient.user_data.create_room id false)
               (List.map fst rooms)
          in
          (match rs, xs with
           | [], xs -> servers xs
           | rs, [] -> rooms rs
           | rs, xs -> servers xs >>= fun () -> rooms rs)
        | _ -> s.XMPPClient.user_data.log (`From jid_from) "no rooms"
    in
    XMPPClient.make_iq_request s ~jid_to:(JID.of_string host)
      (XMPPClient.IQGet (Xml.make_element (Disco.ns_disco_items, "query") [] [])) callback >|= fun () ->
    `Ok state
  in
  (["querying rooms at " ^ host], None, Some clos)

let exec input state contact isself p =
  let msg ?prefix ?(kind = `Info) from msg =
    let f = Utils.option from (fun x -> x ^ "; " ^ from) prefix in
    add_status ~kind state (`Local (state.active_contact, f)) msg
  in
  let help ?err x =
    let a, b = handle_help x in
    match err with
    | None -> msg a b
    | Some err -> msg ~prefix:err ~kind:`Error a b
  in
  let err = msg ~kind:`Error "error" in
  let own_session = selfsession state in
  let real_user = match contact with
    | `User u -> Some u
    | _ -> None
  in

  let ok s = Lwt.return (`Ok s) in

  let global_things = ["add";"status";"priority"]
  and online = [ "join" ; "leave" ; "rooms" ; "authorization" ; "smp" ]
  in
  match cmd_arg input with
  (* completely independent *)
  | ("help" , x) -> help x ; ok state

  | other ->
    let err msg = err msg ; ok state in
    match other, !xmpp_session with
    (* connect *)
    | ("connect", _), None when !Cli_state.connecting -> err "/connect: already connecting"
    | ("connect", _), None -> handle_connect p state.connect_mvar >>= fun () -> ok state
    | ("connect", _), Some _ -> err "/connect: already connected"

    (* disconnect *)
    | ("disconnect", _), Some _ -> handle_disconnect err
    | ("disconnect", _), None   -> err "/disconnect: not connected"

    | ("filter", filter), _ -> ok { state with filter }

    | ("ignore_presence", _), _ -> ok { state with ignore_presence = not state.ignore_presence }

    (* need connection *)
    | (x, _), None when List.mem x online -> err ("/" ^ x ^ ": not connected")
    | (x, _), None when List.mem x global_things -> err ("/" ^ x ^ ": not connected")

    (* log height *)
    | ("logheight", Some x), _ ->
      (match try Some (int_of_string x) with Failure _ -> None with
        | Some log_height when log_height >= 0 -> ok { state with log_height }
        | _ -> err ("/logheight " ^ x ^ ": is not a positive number"))
    | ("logheight", _), _ -> err "/logheight: requires argument"

    (* buddywidth *)
    | ("buddywidth", Some x), _ ->
      (match try Some (int_of_string x) with Failure _ -> None with
        | Some buddy_width when buddy_width >= 0 -> ok { state with buddy_width }
        | _ -> err ("/buddywidth " ^ x ^ ": is not a positive number"))
    | ("buddywidth", _), _ -> err "/buddywidth: requires argument"

    | ("go", Some x), _ ->
      (match Xjid.string_to_jid x with
       | None -> err ("/go " ^ x ^ ": couldn't parse jid")
       | Some jid -> try ok (activate_contact state jid) with _ -> err ("/go " ^ x ^ ": not found"))
    | ("go", None), _ -> err "/go: requires argument"

    (* commands not using active_contact *)
    | (x, None), _ when List.mem x global_things ->
      help ~err:("/" ^ x ^ ": argument required") (Some x) ; ok state

    (* add *)
    | ("add", Some a), Some s -> handle_add state s a (msg ~prefix:("/add " ^ a ^ ": "))

    (* status *)
    | ("status", Some arg), Some s ->
      (match handle_status own_session arg with
       | None -> help ~err:("/status " ^ arg ^ ": unknown argument") (Some "status") ; ok state
       | Some p ->
         Lwt_mvar.put state.connect_mvar (Presence p) >>= fun () ->
         send_status s p >>= fun () ->
         ok state)

    (* priority *)
    | ("priority", Some p), Some s ->
      (match handle_priority own_session p with
       | None   -> help ~err:("/priority " ^ p ^ ": unknown argument") (Some "priority") ; ok state
       | Some p ->
         Lwt_mvar.put state.connect_mvar (Presence p) >>= fun () ->
         send_status s p >>= fun () ->
         ok state)

    (* commands using active_contact as context *)
    | other, s ->

      let ins_msg ?prefix ?(kind = `Info) from msg =
        let f = Utils.option from (fun x -> x ^ "; " ^ from) prefix in
        let message = User.message ~kind (`Local (state.active_contact, f)) false true msg in
        let contact = Contact.new_message contact message in
        ([], Some contact, None)
      in
      let err str =
        ins_msg ~kind:`Error "error" str
      and help ?err x =
        let a, b = handle_help x in
        match err with
        | None -> ins_msg a b
        | Some err -> ins_msg ~prefix:err ~kind:`Error a b
      in
      let need_user f =
        Utils.option (err "not applicable, need a real buddy") f real_user
      in

      let datas, u, clos = match other, s with
        | ("clear", _), _ -> ([], Some (Contact.clear_messages contact), None)

        | ("log", None), _ -> help ~err:"/log: argument required" (Some "log")
        | ("log", Some a), _ when a = "on"  -> handle_log contact true a state.config_directory
        | ("log", Some a), _ when a = "off" -> handle_log contact false a state.config_directory
        | ("log", Some x), _ -> help ~err:("/log " ^ x ^ ": unknown argument") (Some "log")

        | ("info", _), _ ->
          let datas =
            if isself then
              handle_own_info (self state) own_session state.config_directory state.config.Xconfig.dsa
            else
              handle_info contact (resource state) state.config_directory
          in
          (datas, None, None)

        (* join *)
        | ("join", Some a), Some _ ->
          let my_nick = fst (fst state.config.Xconfig.jid)
          and maxstanzas = state.config.Xconfig.muc_max_stanzas
          in
          handle_join ?maxstanzas my_nick a
        | ("join", None), Some _ ->
          (match contact with
           | `Room r ->
             let my_nick = r.Muc.my_nick
             and maxstanzas = state.config.Xconfig.muc_max_stanzas
             in
             handle_join ?maxstanzas my_nick (Xjid.bare_jid_to_string r.Muc.room_jid)
           | _ -> (["/join: active contact is not a room"], None, None))

        | ("leave", reason), Some _ -> handle_leave contact reason
        | ("rooms", Some host), Some _ -> handle_rooms host

        | ("remove", _), Some _ -> need_user (fun u -> ([], None, Some (handle_remove u)))

        | ("fingerprint", None), _ ->
          let datas =
            handle_own_otr_info state.config.Xconfig.dsa @
            current_otr_fp (session state)
          in
          (datas, None, None)
        | ("fingerprint", Some fp), _ ->
          if isself then
            err "/fingerprint: won't talk to myself"
          else
            need_user (fun u -> handle_fingerprint state u err fp)

        | ("revoke", None), _ -> help ~err:"/revoke: argument required" (Some "revoke")
        | ("revoke", Some fp), _ -> need_user (fun u -> handle_revoke u err fp)

        | ("authorization", None), _ -> help ~err:"/authorization: argument required" (Some "authorization")
        | ("authorization", Some a), Some _ ->
          need_user
            (fun u ->
               match handle_authorization a with
               | None, _   -> help ~err:"/authorization: unknown argument" (Some "authorization")
               | Some (kind, m), jid ->
                 let clos state s =
                   let jid = match jid with Some jid -> jid | None -> u.User.bare_jid in
                   let jid_to = Xjid.jid_to_xmpp_jid (`Bare jid) in
                   Xmpp_callbacks.XMPPClient.send_presence s ~jid_to ~kind () >|= fun () ->
                   `Ok state
                 in
                 ([m], None, Some clos))

        | ("alias", alias), _ ->
           if isself then
             err "/alias: cannot adjust own alias"
           else
             let c = Contact.set_alias contact alias in
             ([], Some c, None)

        | ("otrpolicy", None), _ ->
          need_user
            (fun u ->
               let cfg = otr_config u state in
               (print_otr_policy cfg, None, None))
        | ("otrpolicy", Some _), _ when isself -> err "/otrpolicy: cannot adjust own otr policy"
        | ("otrpolicy", Some z), _ ->
          need_user
            (fun u ->
               let cfg = otr_config u state in
               adjust_otr_policy state.config.Xconfig.otr_config cfg u z err)

        | ("otr", None), _ -> help ~err:"/otr: argument required" (Some "otr")
        | ("otr", Some "info"), _ when isself ->
          (handle_own_otr_info state.config.Xconfig.dsa, None, None)
        | ("otr", Some "info"), _ ->
          need_user (fun u -> (handle_otr_info u (session state), None, None))

        | ("otr", Some _), None  -> err "/otr: not connected" (* need this special since /otr info is good if disconnected *)
        | ("otr", Some _), Some _ when isself -> err "/otr: do not like to talk to myself"
        | ("otr", Some "start"), Some _ ->
          need_user
            (fun u ->
               let cfg = otr_config u state in
               handle_otr_start u (session state) cfg state.config.Xconfig.dsa)

        | ("otr", Some "stop"), Some _ ->
          need_user (fun u -> handle_otr_stop u (session state) err)

        | ("otr", Some _), _ -> help ~err:"/otr: unknown argument" (Some "otr")

        | ("smp", _), _ when isself -> err "/smp: do not like to talk to myself"
        | ("smp", None), _ -> help ~err:"/smp: argument required" (Some "smp")

        | ("smp", Some a), Some _ ->
          need_user
            (fun u ->
               match session state with
               | Some session when User.encrypted session.User.otr ->
                 (match split_ws a with
                  | "abort", _ -> handle_smp_abort u session
                  | "shared", Some arg -> handle_smp_shared u session arg
                  | "question", Some question -> handle_smp_question u session question
                  | "answer", Some arg -> handle_smp_answer u session arg
                  | x, _ -> help ~err:("/smp " ^ x ^ ": unknown argument") (Some "smp"))
               | _ -> err "need a secure session, use /otr start first")
        | (x, _), _ -> help ~err:("unknown command: " ^ x) None
      in

      let user old =
        let u = List.fold_left
            (fun c d ->
               let msg = User.message ~kind:`Info (`Local (state.active_contact, "")) false false d in
               Contact.new_message c msg)
            old datas
        in
        Contact.replace_contact state.contacts u ;
        u
      in
      (match u with
       | None   ->
         ignore (user contact) ;
         Lwt.return_unit
       | Some x ->
         let u = user x in
         (* TODO: this is slightly too eager (too many writes) *)
         Lwt_mvar.put state.contact_mvar u) >>= fun () ->
      match clos, !xmpp_session with
      | Some x, Some s -> x state s
      | Some _, None -> msg "error" "not connected" ; Lwt.return (`Ok state)
      | None, _ -> Lwt.return (`Ok state)
