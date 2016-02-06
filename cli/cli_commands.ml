open Cli_state

let string_normalize_fingerprint fpstr =
  let fpstr = String.lowercase fpstr in
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
  subcommands : string list ;
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
    "connect" "/connect" "connects to the server" [] ;
  new_command
    "disconnect" "/disconnect" "disconnects from the server" [] ;
  new_command
    "quit" "/quit" "exits this client" [] ;

  (* global roster commands *)
  new_command
    "add" "/add [jid]"
    "adds jid to your contact list, and sends a subscription request" [] ;

  (* things affecting you *)
  new_command
    "status" "/status [presence] [message]"
    "sets your presence -- one of 'free' 'away' 'dnd' 'xa' 'offline' or 'online' and status message"
    [ "free" ; "away" ; "dnd" ; "xa" ; "offline" ; "online" ] ;
  new_command
    "priority" "/priority [number]" "set your presence priority to number" [ ] ;

  (* user as context *)
  new_command
    "otrpolicy" "/otrpolicy +-[policy version]" "prints (without argument) or adjusts (prefix with add (+) or remove (-)) the otr policies and versions: require_encryption, send_whitespace_tag, whitespace_start_ake, error_start_ake, reveal_macs, v2, v3"
    [ "+REQUIRE_ENCRYPTION" ; "+SEND_WHITESPACE_TAG" ; "+WHITESPACE_START_AKE" ; "+ERROR_START_AKE" ; "+REVEAL_MACS" ; "+V2" ; "+V3" ; "-REQUIRE_ENCRYPTION" ; "-SEND_WHITESPACE_TAG" ; "-WHITESPACE_START_AKE" ; "-ERROR_START_AKE" ; "-REVEAL_MACS" ; "-V2" ; "-V3" ] ;
  new_command
    "log" "/log [on|off]" "enable or disable logging for current contact" [ "on" ; "off" ] ;
  new_command
    "clear" "/clear" "clears the active window chat backlog" [] ;
  new_command
    "authorization" "/authorization [argument]"
    "changes presence subscription of the current contact to argument -- one of 'allow', 'cancel', 'request', 'request_unsubscribe'"
    [ "allow" ; "cancel" ; "request" ; "request_unsubscribe" ] ;
  new_command
    "fingerprint" "/fingerprint [fp]"
    "marks the given OTR fingerprint verified for the current contact ; prints own and session fingerprint if no argument is given" [] ;
  new_command
    "revoke" "/revoke [fp]"
    "revokes the given OTR fingerprint" [] ;
  new_command
    "info" "/info" "displays information about the current session" [] ;
  new_command
    "otr" "/otr [argument]" "manages OTR session by argument -- one of 'start' 'stop' or 'info'"
    [ "start" ; "stop" ; "info" ] ;
  new_command
    "smp" "/smp [argument]" "manages SMP session by argument -- one of 'shared [secret]', 'question [question]', 'answer' or 'abort'"
    [ "shared" ; "question" ; "answer" ; "abort" ] ;
  new_command
    "remove" "/remove" "remove current user from roster" [] ;

  (* multi user chat *)
  new_command
    "join" "/join [chatroom]" "joins chatroom" [] ;

  new_command
    "leave" "/leave [?reason]" "leaves active chatroom (using reason)" [] ;

  (* nothing below here, please *)
  new_command
    "help" "/help [cmd]" "shows available commands or detailed help for cmd"
    (keys ())

let split_ws s =
  let l = String.length s in
  let ws = try String.index s ' ' with Not_found -> l in
  let arg =
    if ws = l then
      None
    else
      let ws' = succ ws in
      let str = String.sub s ws' (l - ws') in
      let arg = String.trim str in
      if arg = "" then
        None
      else
        Some arg
  in
  (String.sub s 0 ws, arg)

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

let completion input =
  let open Astring.String in
  let l = length input in
  if l > 0 && get input 0 = '/' then
    match cmd_arg input with
    | (cmd, None) when Commands.mem commands cmd ->
      let command = Commands.find commands cmd in
      command.subcommands
    | (cmd, None) ->
      let cmds = keys () in
      List.map (fun x -> drop ~max:(pred l) x)
        (List.filter (fun f -> might_match f cmd) cmds)
    | (cmd, Some arg) when Commands.mem commands cmd ->
      let command = Commands.find commands cmd in
      List.map (fun x -> drop ~max:(pred l) (cmd ^ " " ^ x))
        (List.filter (fun f -> might_match f arg) command.subcommands)
    | _ -> []
  else
    []

open Lwt


let handle_help msg = function
  | Some arg when Commands.mem commands arg ->
    let cmd = Commands.find commands arg in
    msg cmd.command_line cmd.documentation
  | _ ->
    let cmds = String.concat " " (keys ()) in
    msg "available commands (/help [cmd])" cmds

let validate_utf8 txt =
  let rec loop d buf = match Uutf.decode d with
    | `Await -> assert false
    | `End -> Buffer.contents buf
    | `Malformed _ -> Uutf.Buffer.add_utf_8 buf 0xFFFD ; loop d buf
    | `Uchar x when x = 0x0009 || x = 0x000A -> (* accept tab and newline *)
       Uutf.Buffer.add_utf_8 buf x ; loop d buf
    | `Uchar 0x000D -> loop d buf (* ignore carriage return *)
    | `Uchar x when x < 0x20 -> Uutf.Buffer.add_utf_8 buf 0xFFFD ; loop d buf (* replace other control characters *)
    (* clearly: DEL 0x7F, vertical tabs, bom, left-to-right, ... *)
    | `Uchar x -> Uutf.Buffer.add_utf_8 buf x ; loop d buf
  in
  let nln = `Readline 0x000A in
  loop (Uutf.decoder ~nln ~encoding:`UTF_8 (`String txt)) (Buffer.create (String.length txt))

let notify_user jid ctx inc_fp verify_fp = function
  | `Established_encrypted_session ssid ->
     let raw_fp = match User.otr_fingerprint ctx with Some fp -> fp | _ -> assert false in
     let otrmsg =
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
       let v, c, o = inc_fp raw_fp in
       match v with
       | `Verified _ -> tos v
       | _ when c = 0 && o -> "POSSIBLE BREAKIN ATTEMPT! new " ^ tos v ^ other o ^ verify
       | _ -> tos v ^ count c ^ other o ^ verify
     in
     [ ((`Local (jid, "OTR")), false, ("encrypted connection established (ssid " ^ ssid ^ ")")) ;
       ((`Local (jid, "OTR key")), false, otrmsg) ]
  | `Warning w               -> [ ((`Local (jid, "OTR warning")), false, w) ]
  | `Received_error e        -> [ ((`From jid), false, e) ]
  | `Received m              -> [ ((`From jid), false, m) ]
  | `Received_encrypted e    -> [ ((`From jid), true, e) ]
  | `SMP_awaiting_secret     -> [ ((`Local (jid, "SMP")), false, "awaiting SMP secret, answer with /smp answer [secret]") ]
  | `SMP_received_question q -> [ ((`Local (jid, "SMP")), false, ("received SMP question (answer with /smp answer [secret]) " ^ q)) ]
  | `SMP_success             ->
     let raw_fp = match User.otr_fingerprint ctx with Some fp -> fp | _ -> assert false in
     verify_fp raw_fp ;
     [ ((`Local (jid, "OTR SMP")), false, "successfully verified!") ]
  | `SMP_failure             -> [ ((`Local (jid, "OTR SMP")), false, "failure") ]

let handle_connect p c_mvar failure =
  let find_user state bare =
    match Contact.find_user state.contacts bare with
    | Some user -> (state, user)
    | None ->
      let u = User.new_user ~jid:bare () in
      Contact.replace_user state.contacts u ;
      (state, u)
  and find_room state bare =
    match Contact.find_room state.contacts bare with
    | Some room -> (state, room)
    | None ->
      let room = Muc.new_room ~jid:bare ~my_nick:(fst (fst state.config.Xconfig.jid)) () in
      Contact.replace_room state.contacts room ;
      (state, room)
  and find_session state user resource =
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

  let log dir msg =
    let exec state = add_status state dir msg ; Lwt.return (`Ok state) in
    p exec
  and locallog str msg =
    let exec state =
      let dir = `Local (state.active_contact, str) in
      add_status state dir msg ;
      Lwt.return (`Ok state)
    in
    p exec

  and message bare ?timestamp dir txt =
    let exec state =
      let state, user = find_user state bare in
      let user = User.insert_message ?timestamp user dir false true txt in
      Contact.replace_user state.contacts user ;
      let state = notify state (`Bare bare) in
      Lwt.return (`Ok state)
    in
    p exec
  and enc_message (bare, res) ?timestamp txt =
    let exec state =
      let state, user = find_user state bare in
      let state, session = find_session state user res in
      let ctx, out, ret = Otr.Engine.handle session.User.otr txt in
      let user = User.update_otr user session ctx in
      Contact.replace_user state.contacts user ;
      let inc_fp raw_fp =
        let fp = User.find_raw_fp user raw_fp in
        let u = User.used_fp user fp res in
        Contact.replace_user state.contacts u ;
        let isverified fp =
          match fp.User.verified with
          | `Verified _ -> true
          | _ -> false
        in
        (fp.User.verified,
         fp.User.session_count,
         List.exists isverified user.User.otr_fingerprints)
      and verify_fp raw_fp =
        let fp = User.find_raw_fp user raw_fp in
        let u = User.verify_fp user fp `SMP in
        Contact.replace_user state.contacts u
      in
      let msgs = List.flatten (List.map (notify_user (`Full (bare, res)) ctx inc_fp verify_fp) ret) in
      let state, user = find_user state bare in
      let user, mark = List.fold_left
          (fun (u, n) (dir, enc, m) ->
             let txt = validate_utf8 m in
             (User.insert_message ?timestamp u dir enc true txt,
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
         send s (Some session) ~kind:Xmpp_callbacks.XMPPClient.Chat (`Full (bare, res)) None body failure) >>= fun () ->
      let state = if mark then notify state (`Full (bare, res)) else state in
      Lwt.return (`Ok state)
    in
    p exec
  and group_message jid timestamp topic body _data id =
    let exec state =
      let state, room = find_room state (Xjid.t_to_bare jid) in
      let room = Utils.option room (fun x -> { room with Muc.topic = Some x }) topic in
      let state, room = match body with
        | None -> (state, room)
        | Some msg ->
          match id, Xjid.resource jid with
          | Some id, Some x when x = room.Muc.my_nick ->
            (match Contact.received (`Room room) id with
             | `Room r -> (state, r)
             | _ -> assert false)
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
      let state, user = find_user state (Xjid.t_to_bare jid) in
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
        let state, user = find_user state b in
        let state, session = find_session state user r in
        let u, _ = User.replace_session user { session with User.receipt } in
        Contact.replace_user state.contacts u ;
        Lwt.return (`Ok state)
    in
    p exec

  and subscription jid smod status =
    let exec state =
      let bare = Xjid.t_to_bare jid in
      let state, user = find_user state bare in
      let post = Utils.option "" (fun x -> " - " ^ x) status in
      let x, txt = match smod with
        | `Probe -> "probed", post
        | `Subscribe -> "subscription request", ("/authorization allow|cancel" ^ post)
        | `Subscribed -> "you have been subscribed to their presence", post
        | `Unsubscribe -> "unsubscription request", ("/authorization allow|cancel" ^ post)
        | `Unsubscribed -> "you have been unsubscribed from their presence", post
      in
      let user = User.insert_message user (`Local (jid, x)) false true txt in
      Contact.replace_user state.contacts user ;
      let state = notify state (`Bare bare) in
      Lwt.return (`Ok state)
    in
    p exec
  and presence jid presence priority status =
    let exec state =
      let msg old =
        let presence_char = User.presence_to_char presence
        and presence_string = User.presence_to_string presence
        and statstring = Utils.option "" (fun x -> " - " ^ x) status
        in
        "[" ^ old ^ ">" ^ presence_char ^ "] (now " ^ presence_string ^ ")" ^ statstring
      in
      match jid with
      | `Bare _ ->
        let log = "presence " ^ msg "_" in
        add_status state (`From jid) log ;
        Lwt.return (`Ok state)
      | `Full (b, r) ->
        let state, user = find_user state b in
        let state, session = find_session state user r in
        if User.presence_unmodified session presence status priority then
          Lwt.return (`Ok state)
        else
          let old = User.presence_to_char session.User.presence in
          let session = { session with User.presence ; status ; priority } in
          let user, removed = User.replace_session user session in
          Contact.replace_user state.contacts user ;
          add_status state (`From jid) (msg old) ;
          let state =
            if removed then
              Utils.option state (fun x -> update_notifications state user x.User.resource r)  (User.find_similar_session user r)
            else
              state
          in
          Lwt.return (`Ok state)
    in
    p exec
  and group_presence jid presence status data =
    let exec state =
      match jid with
      | `Bare _ -> Lwt.return (`Ok state) (* XXX sth more sensible *)
      | `Full (bare, nickname) ->
        let state, r = find_room state bare in
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
           let members = match Muc.member r jid with
             | None -> Muc.new_member nick ~jid:real_jid affiliation role presence status :: r.Muc.members
             | Some _ ->
               (* XXX MUC need to be a bit smarter here *)
               { Muc.nickname = nick ; jid = real_jid ; affiliation ; role ; presence ; status } ::
               List.filter (fun m -> m.Muc.nickname <> nick) r.Muc.members
           in
           Contact.replace_room state.contacts { r with Muc.members }) ;
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
        let state, user = find_user state (Xjid.t_to_bare jid) in
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
  in
  let (user_data : Xmpp_callbacks.user_data) = {
      Xmpp_callbacks.log ;
      locallog ;

      message ;
      enc_message ;
      group_message ;

      received_receipts ;
      update_receipt_state ;

      subscription ;
      presence ;
      group_presence ;

      reset_users ;
      update_users ;

      failure ;
  }
  in
  Lwt_mvar.put c_mvar (Connect user_data)

let handle_disconnect msg =
  Connect.disconnect () >|= fun () ->
  msg "session error" "disconnected"

let send_status s (presence, status, priority) failure =
  let kind, show = Xmpp_callbacks.presence_to_xmpp presence in
  (try_lwt Xmpp_callbacks.XMPPClient.send_presence s ?kind ?show ?status ?priority ()
   with e -> failure e)

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

let handle_add s a msg failure =
  try
    (* TODO: validate input here *)
    let jid_to = JID.of_string a in
    (try_lwt
       (Xmpp_callbacks.XMPPClient.(send_presence s ~jid_to ~kind:Subscribe ()) >|= fun () ->
        msg a "has been subscribed (approval pending)")
     with e -> failure e)
  with _ -> msg "error" "parsing of jid failed (user@node)" ; Lwt.return_unit

let handle_fingerprint user err fp =
  let manual_fp = string_normalize_fingerprint fp in
  if String.length manual_fp = 40 then
    let fp = User.find_raw_fp user manual_fp in
    let user = User.verify_fp user fp `Manual in
    (["verified " ^ manual_fp], Some user, None)
  else
    err "not a hex-encoded OTR fingerprint"

let handle_revoke user err fp =
  let manual_fp = string_normalize_fingerprint fp in
  if String.length manual_fp = 40 then
    let fp = User.find_raw_fp user manual_fp in
    let user = User.revoke_fp user fp in
    (["revoked " ^ manual_fp], Some user, None)
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
  let open Xmpp_callbacks.XMPPClient in
  match arg with
  | "allow"               -> Some (Subscribed, "is now allowed to receive your presence updates")
  | "cancel"              -> Some (Unsubscribed, "won't receive your presence updates anymore")
  | "request"             -> Some (Subscribe, "has been asked to sent presence updates to you")
  | "request_unsubscribe" -> Some (Unsubscribe, "has been asked to no longer sent presence updates to you")
  | _                     -> None

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
    let clos = fun s failure ->
      send s None (`Bare user.User.bare_jid) None out failure
    in
    ([ "starting OTR session" ], None, Some clos)
  | Some session when User.encrypted session.User.otr ->
    ([ "session is already encrypted, please finish first (/otr stop)!" ], None, None)
  | Some session ->
    let ctx, out = Otr.Engine.start_otr session.User.otr in
    let user = User.update_otr user session ctx in
    let clos = fun s failure ->
      send s (Some session) (`Full (user.User.bare_jid, session.User.resource)) None out failure
    in
    ([ "starting OTR session" ], Some user, Some clos)

let handle_otr_stop user session err =
  match session with
  | None -> err "no active session"
  | Some session ->
    let ctx, out = Otr.Engine.end_otr session.User.otr in
    let user = User.update_otr user session ctx in
    let datas, clos = match out with
      | None   -> ([], None)
      | Some body ->
         let clos = fun s failure ->
           let jid = `Full (user.User.bare_jid, session.User.resource) in
           send s (Some session) jid None body failure
         in
         ([ "finished OTR session" ], Some clos)
    in
    (datas, Some user, clos)

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
      Some (fun s failure ->
            let jid = `Full (user.User.bare_jid, session.User.resource) in
            send s (Some session) jid None out failure)
  in
  (datas, Some user, clos)

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
       Some (fun s failure ->
             let jid = `Full (user.User.bare_jid, session.User.resource) in
             send s (Some session) jid None body failure)
  in
  (datas @ ["initiated SMP"], Some user, clos)

let handle_smp_question term users user session question =
  let clos s failure =
    let jid = `Full (user.User.bare_jid, session.User.resource) in
    (* XXX   (new Cli_config.read_inputline ~term ~prompt:"shared secret: " ())#run >>= fun secret -> *)
    let _term = term in
    let secret = "blubb" in
    let sec = Astring.String.trim secret in
    let ctx, out, ret = Otr.Engine.start_smp session.User.otr ~question sec in
    let user = User.update_otr user session ctx in
    let add_msg u m = User.insert_message u (`Local (jid, "")) false false m in
    let user = if sec <> secret then
                 add_msg user "trimmed secret"
               else
                 user
    in
    let user = List.fold_left (fun c -> function
      | `Warning x -> add_msg c ("SMP question warning: " ^ x)
      | _ ->  c)
      user (List.rev ret)
    in
    Contact.replace_user users user ;
    match out with
    | None      -> Lwt.return_unit
    | Some body -> send s (Some session) jid None body failure
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
       Some (fun s failure ->
             let jid = `Full (user.User.bare_jid, session.User.resource) in
             send s (Some session) jid None body failure)
  in
  (datas, Some user, clos)

let handle_remove user =
  fun s failure ->
  (try_lwt
   let bare_jid = Xjid.bare_jid_to_string user.User.bare_jid in
   (* XXX: #116 - provide a error_callback: StanzaError.t -> ?? *)
     Xmpp_callbacks.(Roster.put ~remove:() s bare_jid
       (fun ?jid_from ?jid_to ?lang el ->
        ignore jid_to ; ignore lang ; ignore el ;
        match jid_from with
        | None -> fail XMPPClient.BadRequest
        | Some x -> match Xjid.string_to_jid x with
                    | None -> fail XMPPClient.BadRequest
                    | Some jid ->
                       s.XMPPClient.user_data.log (`From jid) ("Removal of " ^ bare_jid ^ " successful")))
   with e -> failure e)

let print_otr_policy cfg =
  let policies = String.concat ", "
      (List.map Otr.State.policy_to_string cfg.Otr.State.policies)
  and versions = String.concat ", "
      (List.map Otr.State.version_to_string cfg.Otr.State.versions)
  in
  ["OTR versions: " ^ versions ^ " policies: " ^ policies]

let adjust_otr_policy default_cfg cfg contact data =
  let try_decode str =
    Otr.State.string_to_policy str, Otr.State.string_to_version str
  in
  let rec parse_elements pols vers left =
    if String.length left > 0 then
      let arg, rest = split_ws left in
      let first, arg = String.get arg 0, String.sub arg 1 (pred (String.length arg)) in
      let pols, vers = match first, try_decode (String.uppercase arg) with
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
      (datas, Some user, None)
    else
      (["nothing changed"], None, None)
  with
    _ -> (["unable to parse argument"], None, None)

let tell_user (log:(User.direction * string) -> unit) jid ?(prefix:string option) (from:string) (msg:string) =
  let f = Utils.option from (fun x -> x ^ "; " ^ from) prefix in
  log (`Local (jid, f), msg)

let handle_join state s my_nick buddies room err =
  match Xjid.string_to_jid room with
  | Some (`Bare room_jid) ->
     let room = Muc.new_room ~jid:room_jid ~my_nick () in
     Contact.replace_room buddies room ;
     Xmpp_callbacks.Xep_muc.enter_room s (Xjid.jid_to_xmpp_jid (`Bare room_jid)) >>= fun () ->
     Lwt.return (`Ok state)
  | _ -> err "not a bare jid"

let handle_leave buddy reason =
  match buddy with
  | `Room r ->
     let nick = r.Muc.my_nick
     and jid = `Bare r.Muc.room_jid
     in
     let clos s _failure =
       Xmpp_callbacks.Xep_muc.leave_room s ?reason ~nick (Xjid.jid_to_xmpp_jid jid)
     in
     let r = `Room { r with Muc.last_status = false } in
     (["leaving room"], Some r, Some clos)
  | _ -> (["not a chatroom"], None, None)

let exec input state term contact isself failure p : ([ `Ok of Cli_state.state | `Quit of Cli_state.state ] Lwt.t) =
  let log (direction, msg) = add_status state direction msg in
  let msg = tell_user log state.active_contact in
  let err = msg "error" in
  let own_session = selfsession state in
  let real_user = match contact with
    | `User u -> Some u
    | _ -> None
  in

  let ok s = Lwt.return (`Ok s) in

  let global_things = ["add";"status";"priority";"join"] in
  match cmd_arg input with
  (* completely independent *)
  | ("help" , x) -> handle_help (msg ?prefix:None) x ; ok state

  | other ->
     let err msg = err msg ; ok state in
     match other, !xmpp_session with
     (* connect *)
     | ("connect", _), None   -> handle_connect p state.connect_mvar failure >>= fun () -> ok state
     | ("connect", _), Some _ -> err "already connected"

     (* disconnect *)
     | ("disconnect", _), Some _ -> handle_disconnect (msg ?prefix:None) >>= fun () -> ok state
     | ("disconnect", _), None   -> err "not connected"

     (* commands not using active_contact *)
     | (x, _), None when List.mem x global_things -> err "not connected"
     | (x, None), _ when List.mem x global_things ->
        handle_help (msg ~prefix:"argument required") (Some x) ; ok state

     (* add *)
     | ("add", Some a), Some s -> handle_add s a (msg ?prefix:None) failure >>= fun () -> ok state

     (* status *)
     | ("status", Some arg), Some s ->
        (match handle_status own_session arg with
         | None -> handle_help (msg ~prefix:"unknown argument") (Some "status") ; ok state
         | Some p ->
            Lwt_mvar.put state.connect_mvar (Presence p) >>= fun () ->
            send_status s p failure >>= fun () ->
            ok state)

     (* priority *)
     | ("priority", Some p), Some s ->
        (match handle_priority own_session p with
         | None   -> handle_help (msg ~prefix:"unknown argument") (Some "priority") ; ok state
         | Some p ->
            Lwt_mvar.put state.connect_mvar (Presence p) >>= fun () ->
            send_status s p failure >>= fun () ->
            ok state)

     (* multi user chat *)
     | ("join", Some a), Some s ->
        let my_nick = fst (fst state.config.Xconfig.jid) in
        handle_join state s my_nick state.contacts a err


     (* commands using active_contact as context *)
     | other, s ->
        let err str =
          msg "error" str ; ([], None, None)
        and handle_help msg arg =
          handle_help msg arg ; ([], None, None)
        and need_user f =
          Utils.option
            (["not applicable"], None, None)
            (fun x -> let (d, u, c) = f x in
                      (d, Utils.option None (fun u -> Some (`User u)) u, c))
            real_user
        in

        let datas, u, clos = match other, s with
          | ("clear", _), _ ->
             ([], Some (Contact.clear_messages contact), None)

          | ("log", None), _ -> handle_help (msg ~prefix:"argument required") (Some "log")
          | ("log", Some a), _ when a = "on"  -> handle_log contact true a state.config_directory
          | ("log", Some a), _ when a = "off" -> handle_log contact false a state.config_directory
          | ("log", Some _), _ -> handle_help (msg ~prefix:"unknown argument") (Some "log")

          | ("info", _), _ ->
             let datas =
               if isself then
                 handle_own_info (self state) own_session state.config_directory state.config.Xconfig.dsa
               else
                 handle_info contact (resource state) state.config_directory
             in
             (datas, None, None)

          | ("leave", reason), Some _ -> handle_leave contact reason

         | ("remove", _), None -> err "not connected"
         | ("remove", _), Some _ -> need_user (fun u -> ([], None, Some (handle_remove u)))

         | ("fingerprint", None), _ ->
            let datas =
              handle_own_otr_info state.config.Xconfig.dsa @
                current_otr_fp (session state)
            in
            (datas, None, None)
         | ("fingerprint", Some fp), _ ->
            if isself then
              err "won't talk to myself"
            else
              need_user (fun u -> handle_fingerprint u err fp)

         | ("revoke", None), _ -> handle_help (msg ~prefix:"argument required") (Some "revoke")
         | ("revoke", Some fp), _ -> need_user (fun u -> handle_revoke u err fp)

         | ("authorization", _), None -> err "not connected"
         | ("authorization", None), _ -> handle_help (msg ~prefix:"argument required") (Some "authorization")
         | ("authorization", Some a), Some _ ->
            if isself then
              err "won't authorize myself"
            else
              need_user
                (fun u ->
                 match handle_authorization a with
                 | None   -> handle_help (msg ~prefix:"unknown argument") (Some "authorization")
                 | Some (kind, m) ->
                    let clos = fun s failure ->
                      (try_lwt
                       let jid_to = Xjid.jid_to_xmpp_jid (`Bare u.User.bare_jid) in
                           Xmpp_callbacks.XMPPClient.send_presence s ~jid_to ~kind ()
                           with e -> failure e)
                    in
                    ([m], None, Some clos))

         | ("otrpolicy", None), _ ->
            need_user
              (fun u -> let cfg = otr_config u state in
                        (print_otr_policy cfg, None, None))

         | ("otrpolicy", Some _), _ when isself -> err "cannot adjust own otr policy"
         | ("otrpolicy", Some z), _ ->
            need_user
              (fun u -> let cfg = otr_config u state in
                        adjust_otr_policy state.config.Xconfig.otr_config cfg u z)


         | ("otr", None), _ ->
            handle_help (msg ~prefix:"argument required") (Some "otr")
         | ("otr", Some "info"), _  ->
            need_user
              (fun u -> if isself then
                          (handle_own_otr_info state.config.Xconfig.dsa, None, None)
                        else
                          (handle_otr_info u (session state), None, None))

         | ("otr", Some _), None  -> err "not connected"
         | ("otr", Some "start"), Some _ ->
            need_user
              (fun u ->
               if isself then
                 err "do not like to talk to myself"
               else
                 let cfg = otr_config u state in
                 handle_otr_start u (session state) cfg state.config.Xconfig.dsa)

         | ("otr", Some "stop"), Some _ ->
            need_user
              (fun u ->
               if isself then
                 err "do not like to talk to myself"
               else
                 handle_otr_stop u (session state) err)

         | ("otr", Some _), _ -> handle_help (msg ~prefix:"unknown argument") (Some "otr")

         | ("smp", _), _ when isself -> err "do not like to talk to myself"
         | ("smp", None), _ -> handle_help (msg ~prefix:"argument required") (Some "smp")
         | ("smp", _), None -> err "not connected"

         | ("smp", Some a), Some _ ->
            need_user
              (fun u ->
               match session state with
               | Some session when User.encrypted session.User.otr ->
                  (match split_ws a with
                   | "abort", _ -> handle_smp_abort u session
                   | "shared", Some arg -> handle_smp_shared u session arg
                   | "question", Some question -> handle_smp_question term state.contacts u session question
                   | "answer", Some arg -> handle_smp_answer u session arg
                   | _ -> handle_help (msg ~prefix:"argument required") (Some "smp"))
               | _ -> err "need a secure session, use /otr start first")
         | _ -> handle_help (msg ~prefix:"unknown command") None
        in

        let user old =
          let u = List.fold_left
                    (fun c d ->
                     let msg = User.message (`Local (state.active_contact, "")) false false d in
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
        | Some x, Some s -> x s failure >|= fun () -> `Ok state
        | Some _, None -> msg "error" "not connected" ; Lwt.return (`Ok state)
        | None, _ -> Lwt.return (`Ok state)
