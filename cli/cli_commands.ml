open Cli_state

let string_normalize_fingerprint fpstr =
  let fpstr = String.lowercase fpstr in
  let rec worker ~fpstr ~acclst = function
  | -1 -> String.concat "" acclst
  | i -> worker ~fpstr ~acclst:(
    (match (String.get fpstr i) with
     | ' '| ':' -> ""
     | c -> String.make 1 c
    )::acclst) (i-1)
  in
    worker ~fpstr ~acclst:[] ((String.length fpstr)-1)

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
    "log" "/log [on|off]" "enable or disable logging for current contact" [ "on" ; "off" ] ;
  new_command
    "clear" "/clear" "clears the active window chat backlog" [] ;
  new_command
    "authorization" "/authorization [argument]"
    "changes presence subscription of the current contact to argument -- one of 'allow', 'cancel', 'request', 'request_unsubscribe'"
    [ "allow" ; "cancel" ; "request" ; "request_unsubscribe" ] ;
  new_command
    "fingerprint" "/fingerprint [fp]"
    "verifies the current contact's OTR fingerprint (fp must match the one used in the currently established session)" [] ;
  new_command
    "info" "/info" "displays information about the current session" [] ;
  new_command
    "otr" "/otr [argument]" "manages OTR session by argument -- one of 'start' 'stop' or 'info'"
    [ "start" ; "stop" ; "info" ] ;

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
        Some str
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
  if String.(length input > 0 && get input 0 = '/') then
    match cmd_arg input with
    | (cmd, None) when Commands.mem commands cmd ->
      let command = Commands.find commands cmd in
      List.map (fun x -> ("/" ^ x, " ")) command.subcommands
    | (cmd, None) ->
      let cmds = keys () in
      List.map (fun x -> ("/" ^ x, " "))
        (List.filter (fun f -> might_match f cmd) cmds)
    | (cmd, Some arg) when Commands.mem commands cmd ->
      let command = Commands.find commands cmd in
      List.map (fun x -> "/" ^ cmd ^ " " ^ x, "")
        (List.filter (fun f -> might_match f arg) command.subcommands)
    | _ -> [(input, "")]
  else
    [(input, "")]

open Lwt


let handle_help msg = function
  | Some arg when Commands.mem commands arg ->
    let cmd = Commands.find commands arg in
    msg cmd.command_line cmd.documentation
  | _ ->
    let cmds = String.concat " " (keys ()) in
    msg "available commands (try [/help cmd])" cmds

let handle_connect ?out state config log redraw failure =
  let otr_config = config.Config.otr_config
  and notify u =
    if List.mem u state.notifications || state.active_contact = u then
      ()
    else
      state.notifications <- u :: state.notifications ;
    redraw ()
  and users = state.users
  in
  let (user_data : Xmpp_callbacks.user_data) = {
      Xmpp_callbacks.otr_config = otr_config ;
      Xmpp_callbacks.users      = users      ;
      Xmpp_callbacks.received   = log        ;
      Xmpp_callbacks.notify     = notify     ;
      Xmpp_callbacks.failure    = failure    ;
  } in
  Xmpp_callbacks.connect ?out config user_data () >|= (function
      | None   -> ()
      | Some s -> xmpp_session := Some s ;
                  Lwt.async (fun () -> Xmpp_callbacks.parse_loop s))



let send_status s presence status priority failure =
  let open Xmpp_callbacks.XMPPClient in
  let user_to_xmpp = function
    | `Offline      -> (Some Unavailable, None)
    | `Online       -> (None            , None)
    | `Free         -> (None            , Some ShowChat)
    | `Away         -> (None            , Some ShowAway)
    | `DoNotDisturb -> (None            , Some ShowDND)
    | `ExtendedAway -> (None            , Some ShowXA)
  in
  let kind, show = user_to_xmpp presence in
  let priority = match priority with 0 -> None | x -> Some x in
  (try_lwt send_presence s ?kind ?show ?status ?priority ()
   with e -> failure e) >|= fun () -> Some ()

let handle_status s failure session a =
  let p, status = split_ws a in
  match User.string_to_presence p with
  | None   -> return None
  | Some x -> let priority = session.User.priority in
              send_status s x status priority failure

let handle_priority s failure session p =
  try
    let prio = int_of_string p in
    assert (prio >= -128 && prio <= 127) ; (* RFC 6121 4.7.2.3 *)
    send_status s session.User.presence session.User.status prio failure
  with
    _ -> return None

let handle_add s failure msg a =
  try
    let jid_to = JID.of_string a in
    (try_lwt
       (Xmpp_callbacks.XMPPClient.(send_presence s ~jid_to ~kind:Subscribe ()) >>= fun () ->
        msg a "has been subscribed (approval pending)")
     with e -> failure e)
  with _ -> msg "error" "parsing of jid failed (user@node)"

let handle_fingerprint dump fp user =
  match User.active_session user with
  | Some session when User.encrypted session.User.otr ->
    let manual_fp = string_normalize_fingerprint fp in
    ( match session.User.otr.Otr.State.their_dsa with
      | Some key when User.fingerprint key = manual_fp ->
        let otr_fp = User.find_raw_fp user manual_fp in
        User.replace_fp user { otr_fp with User.verified = true } ;
        dump ("fingerprint " ^ fp ^ " is now marked verified") ;
        None
      | _ -> Some "provided fingerprint does not match the one of this active session" )
  | _ -> Some "no active OTR session"

let handle_log users dump user v a =
  if user.User.preserve_messages <> v then
    (dump ("logging turned " ^ a) ;
     let user = { user with User.preserve_messages = v } in
     User.Users.replace users user.User.jid user)

let handle_authorization s failure dump user arg =
  let open Xmpp_callbacks.XMPPClient in
  let jid = user.User.jid in
  let doit kind m =
    (try_lwt send_presence s ~jid_to:(JID.of_string jid) ~kind ()
     with e -> failure e) >|= fun () ->
     dump m ;
     Some ()
  in
  match arg with
  | "allow"               -> doit Subscribed "is allowed to receive your presence updates"
  | "cancel"              -> doit Unsubscribed "won't receive your presence updates"
  | "request"             -> doit Subscribe "has been asked to sent presence updates to you"
  | "request_unsubscribe" -> doit Unsubscribe "has been asked to no longer sent presence updates to you"
  | _                     -> return None

let dump_otr_fps fps =
  let marshal_otr fp =
    let ver = if fp.User.verified then "verified" else "unverified" in
    let used = string_of_int fp.User.session_count in
    let resources = String.concat ", " fp.User.resources in
    fp.User.data ^ " " ^ ver ^ " (used in " ^ used ^ " sessions, resources: " ^ resources ^ ")"
  in
  String.concat "\n" (List.map marshal_otr fps)

let handle_otr_info dump user =
  match User.active_session user with
  | Some session ->
    dump ("active otr session " ^ session.User.resource ^ ": " ^ Otr.State.session_to_string session.User.otr) ;
    dump ("otr fingerprints: " ^ (dump_otr_fps user.User.otr_fingerprints))
  | None ->
    dump ("(no active session) OTR fingerprints: " ^ (dump_otr_fps user.User.otr_fingerprints))

let handle_own_otr_info dump config =
  let otr_pub = Nocrypto.Dsa.pub_of_priv config.Config.otr_config.Otr.State.dsa in
  dump ("own otr fingerprint: " ^ (User.format_fp (User.fingerprint otr_pub)))

let common_info dump user cfgdir =
  dump "jid" user.User.jid ;
  ( match user.User.name with
    | None -> ()
    | Some x -> dump "name" x ) ;
  ( if user.User.preserve_messages then
      let histo =
        let dir = Persistency.message_history_dir cfgdir in
        Filename.concat dir user.User.jid
      in
      dump "persistent history in " histo )

let marshal_session s =
  let prio = string_of_int s.User.priority in
  let pres = User.presence_to_string s.User.presence in
  let status = match s.User.status with
    | None -> ""
    | Some x -> " - " ^ x
  in
  s.User.resource ^ " (" ^ prio ^ "): " ^ pres ^ status

let handle_info dump user cfgdir =
  let dump a b = dump (a ^ ": " ^ b) in
  common_info dump user cfgdir ;
  ( match user.User.groups with
    | [] -> ()
    | xs -> dump "groups" (String.concat ", " xs) ) ;
  let add =
    let ps x = List.mem x user.User.properties in
    ( if ps `Pending then "pending " else "" ) ^
    ( if ps `PreApproved then "preapproved" else "" )
  in
  let add = if String.length add > 0 then " (" ^ (String.trim add) ^ ")" else "" in
  dump "subscription" ((User.subscription_to_string user.User.subscription) ^ add) ;
  let active = User.active_session user in
  List.iteri (fun i s ->
      let act = match active with
        | Some x when x = s -> " (active)"
        | _ -> ""
      in
      dump ("session " ^ (string_of_int i) ^ act) (marshal_session s))
    user.User.active_sessions

let handle_own_info dump user own_session cfgdir config =
  let dump a b = dump (a ^ ": " ^ b) in
  common_info dump user cfgdir ;
  let otr_pub = Nocrypto.Dsa.pub_of_priv config.Config.otr_config.Otr.State.dsa in
  dump "own otr fingerprint" (User.format_fp (User.fingerprint otr_pub)) ;
  let active = User.active_session user in
  List.iteri (fun i s ->
      let act =
        let own = if own_session = s then
            " (own session)"
          else
            ""
        in
        match active with
        | Some x when x = s -> own ^ " (active)"
        | _ -> own
      in
      dump ("session " ^ (string_of_int i) ^ act) (marshal_session s))
    user.User.active_sessions

let handle_otr_start s dump failure otr_cfg user =
  let send_over resource body =
    let jid_to =
      let r = if resource = "" then "" else "/" ^ resource in
      JID.of_string (user.User.jid ^ r)
    in
    (try_lwt Xmpp_callbacks.XMPPClient.(send_message s ~kind:Chat ~jid_to ?body ())
     with e -> failure e)
  in
  match User.active_session user with
  | Some session when User.encrypted session.User.otr ->
    dump "session is already encrypted, please finish first (/otr stop)!" ; return_unit
  | Some session ->
    let ctx, out = Otr.Handshake.start_otr session.User.otr in
    session.User.otr <- ctx ;
    dump "starting OTR session" ;
    send_over session.User.resource (Some out)
  | None ->
    (* no OTR context, but we're sending only an OTR query anyways
       (and if we see a reply, we'll get some resource from the other side) *)
    let ctx = Otr.State.new_session otr_cfg () in
    let _, out = Otr.Handshake.start_otr ctx in
    dump "starting OTR session" ;
    send_over "" (Some out)

let handle_otr_stop s dump err failure user =
  match User.active_session user with
  | Some session ->
    ( match Otr.State.(session.User.otr.state.message_state) with
      | `MSGSTATE_ENCRYPTED _ | `MSGSTATE_FINISHED ->
        let ctx, out = Otr.Handshake.end_otr session.User.otr in
        session.User.otr <- ctx ;
        dump "finished OTR session" ;
        ( match out with
          | None   -> return_unit
          | Some body ->
            let jid_to = JID.of_string (User.userid user session) in
            (try_lwt Xmpp_callbacks.XMPPClient.(send_message s ~kind:Chat ~jid_to ~body ())
             with e -> failure e) )
      | _ -> err "no OTR session" )
  | None -> err "no active session"

let tell_user (log:(User.direction * string) -> unit) ?(prefix:string option) (from:string) (msg:string) =
  let f = match prefix with
    | None -> from
    | Some x -> x ^ "; " ^ from
  in
  log (`Local f, msg) ;
  return_unit

let exec ?out input state config log redraw =
  let msg = tell_user log in
  let err = msg "error" in
  let failure reason =
    xmpp_session := None ;
    msg "session error" (Printexc.to_string reason)
  in
  let contact = User.Users.find state.users state.active_contact in
  let dump data = User.new_message contact (`Local "") false false data in
  let self = state.user = contact in

  match cmd_arg input with
  (* completely independent *)
  | ("help" , x) -> handle_help (msg ?prefix:None) x
  | ("clear", _) -> contact.User.message_history <- [] ; return_unit

  (* connect *)
  | ("connect", _) ->
    ( match !xmpp_session with
      | None   -> handle_connect ?out state config log redraw failure
      | Some _ -> err "already connected" )

  (* own things *)
  | ("status", x) ->
    ( match !xmpp_session, x with
      | None  , _      -> err "not connected"
      | Some _, None   -> handle_help (msg ~prefix:"argument required") (Some "status")
      | Some s, Some a -> handle_status s failure state.session a >>= (function
          | None   -> handle_help (msg ~prefix:"unknown argument") (Some "status")
          | Some _ -> return_unit ) )
  | ("priority", x) ->
    ( match !xmpp_session, x with
      | None  , _      -> err "not connected"
      | Some _, None   -> handle_help (msg ~prefix:"argument required") (Some "priority")
      | Some s, Some p -> handle_priority s failure state.session p >>= (function
          | None   -> handle_help (msg ~prefix:"unknown argument") (Some "priority")
          | Some _ -> return_unit ) )

  (* do not use active_chat *)
  | ("add", x) ->
    ( match !xmpp_session, x with
      | None  , _      -> err "not connected"
      | Some _, None   -> handle_help (msg ~prefix:"argument required") (Some "add")
      | Some s, Some a -> handle_add s failure (msg ?prefix:None) a )

  (* commands using active_chat as context *)
  | ("log", x) ->
    ( match x with
      | None   -> handle_help (msg ~prefix:"argument required") (Some "log")
      | Some a when a = "on"  -> handle_log state.users dump contact true a ; return_unit
      | Some a when a = "off" -> handle_log state.users dump contact false a ; return_unit
      | Some _ -> handle_help (msg ~prefix:"unknown argument") (Some "log") )

  | ("info", _) ->
    ( if self then
        handle_own_info dump contact state.session state.config_directory config
      else
        handle_info dump contact state.config_directory );
    return_unit
  | ("otr", x) ->
    ( match x with
      | None -> handle_help (msg ~prefix:"argument required") (Some "otr")
      | Some x when x = "info" ->
        ( if self then
            handle_own_otr_info dump config
          else
            handle_otr_info dump contact ) ;
        return_unit
      | Some x -> (match !xmpp_session with
          | None -> err "not connected"
          | Some s when x = "start" ->
            if self then err "do not like to talk to myself" else
              handle_otr_start s dump failure config.Config.otr_config contact
          | Some s when x = "stop" ->
            if self then err "do not like to talk to myself" else
              handle_otr_stop s dump err failure contact
          | Some _ -> handle_help (msg ~prefix:"unknown argument") (Some "otr") ) )

  | ("fingerprint", x) ->
    ( match x with
      | None   -> handle_help (msg ~prefix:"argument required") (Some "fingerprint")
      | Some fp ->
        if self then err "won't talk to myself" else
          match handle_fingerprint dump fp contact with
          | None -> return_unit
          | Some x -> err x )
  | ("authorization", x) ->
    ( match !xmpp_session, x with
      | None  , _      -> err "not connected"
      | Some _, None   -> handle_help (msg ~prefix:"argument required") (Some "authorization")
      | Some s, Some a ->
        if self then err "won't authorize myself" else
          handle_authorization s failure dump contact a >>= (function
              | None   -> handle_help (msg ~prefix:"unknown argument") (Some "authorization")
              | Some _ -> return_unit ) )

  | _ -> handle_help (msg ~prefix:"unknown command") None
