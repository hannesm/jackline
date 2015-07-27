open Cli_state

let string_normalize_fingerprint fpstr =
  let fpstr = String.lowercase fpstr in
  let chars = Stringext.to_list fpstr in
  let hexchars = List.filter (function '0'..'9' | 'a'..'f' -> true | _ -> false) chars in
  Stringext.of_list hexchars

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
    "verifies the current contact's OTR fingerprint (fp must match the one used in the currently established session)" [] ;
  new_command
    "info" "/info" "displays information about the current session" [] ;
  new_command
    "otr" "/otr [argument]" "manages OTR session by argument -- one of 'start' 'stop' or 'info'"
    [ "start" ; "stop" ; "info" ] ;
  new_command
    "smp" "/smp [argument]" "manages SMP session by argument -- one of 'start [?question] [secret]' 'answer' or 'abort' - question is optional and may _NOT_ include a whitespace!"
    [ "start" ; "answer" ; "abort" ] ;
  new_command
    "remove" "/remove" "remove current user from roster" [] ;

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

let resolve config log =
  let domain = JID.to_idn (Jid.jid_to_xmpp_jid (`Full config.Config.jid))
  and hostname = config.Config.hostname
  and port = config.Config.port
  in
  let report sockaddr =
    let addr = match sockaddr with
      | Unix.ADDR_INET (inet_addr, port) ->
        Unix.string_of_inet_addr inet_addr ^ " on port " ^ string_of_int port
      | Unix.ADDR_UNIX str -> str
    in
    log (`Local "connecting", "to " ^ domain ^ " (" ^ addr ^ ")") ;
  in
  Xmpp_callbacks.resolve hostname port domain >|= fun sa ->
  report sa ;
  sa

let handle_connect ?out state config log redraw failure =
  let doit user_data () =
    match config.Config.password with
    | None -> failure (Invalid_argument "no password provided, please restart") >|= fun () -> None
    | Some password ->
      try_lwt
        (resolve config log >>= fun sockaddr ->
         let certname = match config.Config.certificate_hostname with
           | None -> JID.to_idn (Jid.jid_to_xmpp_jid (`Full config.Config.jid))
           | Some x -> x
         in
         (X509_lwt.authenticator (match config.Config.authenticator with
              | `Trust_anchor x -> `Ca_file x
              | `Fingerprint fp -> `Hex_fingerprints (`SHA256, [(certname, fp)])))
         >>= fun authenticator ->
         Xmpp_callbacks.connect ?out sockaddr
           config.Config.jid certname password
           config.Config.priority authenticator user_data)
      with exn -> failure exn >|= fun () -> None
  in

  let remove jid =
    let bare = Jid.t_to_bare jid in
    User.Users.remove state.users bare ;
    if state.active_contact = jid then
      state.active_contact <- state.myjid ;
    if state.last_active_contact = jid then
      state.last_active_contact <- state.myjid ;
    redraw ()
  and log dir txt = log (dir, txt)
  and message jid dir enc txt =
    User.add_message state.users jid dir enc true txt ;
    notify state jid ;
    redraw ()
  and receipt jid id =
    let bare = Jid.t_to_bare jid in
    let user = User.find_or_create state.users bare in
    let user = User.received_message user id in
    User.Users.replace state.users bare user ;
    redraw ()
  and user jid =
    let jid = Jid.t_to_bare jid in
    User.find_or_create state.users jid
  and session jid =
    let bare = Jid.t_to_bare jid in
    let user = User.find_or_create state.users bare in
    let otr_config = match user.User.otr_custom_config with
      | None -> config.Config.otr_config
      | Some x -> x
    in
    let resource = match Jid.resource jid with
      | Some x -> x
      | None -> "NONE"
    in
    let user, session = User.find_or_create_session user resource otr_config config.Config.dsa in
    User.Users.replace state.users bare user ;
    session
  and update_session jid session =
    let bare = Jid.t_to_bare jid in
    let user = User.find_or_create state.users bare in
    User.replace_session state.users user session
  and update_user user alert =
    let jid = user.User.bare_jid in
    User.Users.replace state.users jid user ;
    if alert then notify state (`Bare jid) ;
    redraw ()
  and inc_fp jid raw_fp =
    let bare = Jid.t_to_bare jid in
    let resource = match Jid.resource jid with Some x -> x | None -> "NONE" in
    let user = User.find_or_create state.users bare in
    let fp = User.find_raw_fp user raw_fp in
    let resources =
      if List.mem resource fp.User.resources then
        fp.User.resources
      else
        resource :: fp.User.resources
    in
    let fp = { fp with User.session_count = succ fp.User.session_count ; User.resources = resources } in
    let u = User.replace_fp user fp in
    User.Users.replace state.users bare u ;
    Lwt.async (fun () -> Lwt_mvar.put state.user_mvar u) ;
    (fp.User.verified, pred fp.User.session_count, List.exists (fun x -> x.User.verified) user.User.otr_fingerprints)
  in
  let (user_data : Xmpp_callbacks.user_data) = {
      Xmpp_callbacks.log ;
      remove ;
      message ;
      user ;
      session ;
      update_session ;
      update_user ;
      receipt ;
      inc_fp ;
      failure ;
  }
  in
  doit user_data () >>= function
  | None   -> return_unit
  | Some s ->
    xmpp_session := Some s ;
    Lwt.async (fun () -> Xmpp_callbacks.parse_loop s) ;
    Lwt_mvar.put state.state_mvar Connected >|= fun () ->
    Xmpp_callbacks.restart_keepalive s

let handle_disconnect s users msg =
  Xmpp_callbacks.close s >>= fun () ->
  msg "session error" "disconnected" ;
  cleanups users ;
  return_unit

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

let handle_status s session failure a =
  let p, status = split_ws a in
  match User.string_to_presence p with
  | None   -> return None
  | Some x -> let priority = session.User.priority in
              send_status s x status priority failure

let handle_priority s session failure p =
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

let handle_fingerprint mvar users dump err fp user =
  match User.active_session user with
  | Some session when User.encrypted session.User.otr ->
    let manual_fp = string_normalize_fingerprint fp in
    ( match Otr.Utils.their_fingerprint session.User.otr with
      | Some key when User.hex_fingerprint key = manual_fp ->
        let otr_fp = User.find_raw_fp user manual_fp in
        let user = User.replace_fp user { otr_fp with User.verified = true } in
        User.Users.replace users user.User.bare_jid user ;
        dump ("fingerprint " ^ fp ^ " is now marked verified") ;
        Lwt_mvar.put mvar user
      | _ -> err "provided fingerprint does not match the one of this active session" )
  | _ -> err "no active OTR session"

let handle_log mvar users dump user v a =
  if user.User.preserve_messages <> v then
    (let user = { user with User.preserve_messages = v } in
     User.Users.replace users user.User.bare_jid user ;
     dump ("logging turned " ^ a) ;
     Lwt_mvar.put mvar user)
  else
    Lwt.return_unit

let handle_authorization s failure dump user arg =
  let open Xmpp_callbacks.XMPPClient in
  let doit kind m =
    (try_lwt send_presence s ~jid_to:(Jid.jid_to_xmpp_jid (`Bare user.User.bare_jid)) ~kind ()
     with e -> failure e) >|= fun () ->
     dump m ;
     Some ()
  in
  match arg with
  | "allow"               -> doit Subscribed "is now allowed to receive your presence updates"
  | "cancel"              -> doit Unsubscribed "won't receive your presence updates anymore"
  | "request"             -> doit Subscribe "has been asked to sent presence updates to you"
  | "request_unsubscribe" -> doit Unsubscribe "has been asked to no longer sent presence updates to you"
  | _                     -> return None

let dump_otr_fps fps =
  let marshal_otr fp =
    let ver = if fp.User.verified then "verified" else "unverified" in
    let used = string_of_int fp.User.session_count in
    let resources = String.concat ", " fp.User.resources in
    (User.format_fp fp.User.data) ^ " " ^ ver ^ " (used in " ^ used ^ " sessions, resources: " ^ resources ^ ")"
  in
  String.concat "\n" (List.map marshal_otr fps)

let current_otr_fp dump user =
  match User.active_session user with
  | None -> dump "no active session"
  | Some session ->
    match User.otr_fingerprint session.User.otr with
    | Some fp -> dump ("their otr fingerprint: " ^ (User.format_fp fp))
    | None -> dump "no active OTR session"

let handle_otr_info dump user =
  match User.active_session user with
  | Some session ->
    dump ("active otr session " ^ session.User.resource ^ ": " ^ Otr.State.session_to_string session.User.otr) ;
    dump ("otr fingerprints: " ^ (dump_otr_fps user.User.otr_fingerprints))
  | None ->
    dump ("(no active session) OTR fingerprints: " ^ (dump_otr_fps user.User.otr_fingerprints))

let handle_own_otr_info dump config =
  let otr_fp = Otr.Utils.own_fingerprint config.Config.dsa in
  dump ("your otr fingerprint:  " ^ (User.format_fp (User.hex_fingerprint otr_fp)))

let common_info dump user cfgdir =
  dump "jid" (User.jid user) ;
  ( match user.User.name with
    | None -> ()
    | Some x -> dump "name" x ) ;
  ( if user.User.preserve_messages then
      let histo =
        let dir = Persistency.history in
        Filename.(concat (concat cfgdir dir) (User.jid user))
      in
      dump "persistent history in " histo )

let marshal_session s =
  let prio = string_of_int s.User.priority in
  let pres = User.presence_to_string s.User.presence in
  let status = match s.User.status with
    | None -> ""
    | Some x -> " - " ^ x
  in
  let receipts = User.receipt_state_to_string s.User.receipt in
  s.User.resource ^ " (" ^ prio ^ ") (receipts " ^ receipts ^ "): " ^ pres ^ status

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
  List.iter (fun s ->
      let act = match active with
        | Some x when x = s -> "active"
        | _ -> "other"
      in
      dump act (marshal_session s))
      (List.sort User.compare_session user.User.active_sessions)

let handle_own_info dump user cfgdir config res =
  let dump a b = dump (a ^ ": " ^ b) in
  common_info dump user cfgdir ;
  let otr_fp = Otr.Utils.own_fingerprint config.Config.dsa in
  dump "own otr fingerprint" (User.format_fp (User.hex_fingerprint otr_fp)) ;
  let active = User.active_session user in
  List.iteri (fun i s ->
      let own = if s.User.resource = res then " (own)" else "" in
      let act =
        match active with
        | Some x when x = s -> own ^ " (active)"
        | _ -> own
      in
      dump ("session " ^ (string_of_int i) ^ act) (marshal_session s))
    user.User.active_sessions

let handle_otr_start s users dump failure otr_cfg dsa user =
  let send_over session body =
    let jid = `Full (user.User.bare_jid, session.User.resource) in
    send s jid None body failure
  in
  match User.active_session user with
  | Some session when User.encrypted session.User.otr ->
    dump "session is already encrypted, please finish first (/otr stop)!" ; return_unit
  | Some session ->
    let ctx, out = Otr.Engine.start_otr session.User.otr in
    User.replace_session users user { session with User.otr = ctx } ;
    dump "starting OTR session" ;
    send_over session out
  | None ->
    (* no OTR context, but we're sending only an OTR query anyways
       (and if we see a reply, we'll get some resource from the other side) *)
    let _, session = User.find_or_create_session user "" otr_cfg dsa in
    let _, out = Otr.Engine.start_otr session.User.otr in
    dump "starting OTR session" ;
    send_over session out

let handle_otr_stop s users dump err failure user =
  match User.active_session user with
  | Some session ->
    let ctx, out = Otr.Engine.end_otr session.User.otr in
    User.replace_session users user { session with User.otr = ctx } ;
    ( match out with
      | None   -> return_unit
      | Some body ->
        dump "finished OTR session" ;
        let jid = `Full (user.User.bare_jid, session.User.resource) in
        send s jid None body failure )
  | None -> err "no active session"

let handle_smp_abort users s session user dump failure =
  let ctx, out, ret = Otr.Engine.abort_smp session.User.otr in
  User.replace_session users user { session with User.otr = ctx } ;
  List.iter (function
      | `Warning x -> dump ("SMP abort warning: " ^ x)
      | _ -> () )
    ret ;
  match out with
  | None -> return_unit
  | Some out ->
     let jid = `Full (user.User.bare_jid, session.User.resource) in
     send s jid None out failure

let handle_smp_start users s session user dump failure args =
  let secret, question = match split_ws args with
  | question, Some secret -> (secret, Some question)
  | secret, None -> (secret, None)
  in
  let ctx, out, ret = Otr.Engine.start_smp session.User.otr ?question secret in
  User.replace_session users user { session with User.otr = ctx } ;
  List.iter (function
      | `Warning x -> dump ("SMP start warning: " ^ x)
      | _ -> () )
    ret ;
  dump "initiated SMP" ;
  match out with
  | None   -> return_unit
  | Some body ->
     let jid = `Full (user.User.bare_jid, session.User.resource) in
     send s jid None body failure

let handle_smp_answer users s session user dump failure secret =
  let ctx, out, ret = Otr.Engine.answer_smp session.User.otr secret in
  User.replace_session users user { session with User.otr = ctx } ;
  List.iter (function
      | `Warning x -> dump ("SMP answer warning: " ^ x)
      | _ -> () )
    ret ;
  match out with
  | None   -> return_unit
  | Some body ->
     let jid = `Full (user.User.bare_jid, session.User.resource) in
     send s jid None body failure

let handle_remove s dump user failure =
  (try_lwt
     Xmpp_callbacks.Roster.put ~remove:() s (User.jid user)
       (fun ?jid_from ?jid_to ?lang el ->
         ignore jid_from ; ignore jid_to ; ignore lang ; ignore el ;
         dump ("Removal of " ^ User.jid user ^ " successful") ;
         return_unit)
   with e -> failure e)

let print_otr_policy dump pref cfg =
  let policies = String.concat ", "
      (List.map Otr.State.policy_to_string cfg.Otr.State.policies)
  and versions = String.concat ", "
      (List.map Otr.State.version_to_string cfg.Otr.State.versions)
  in
  dump ("OTR " ^ pref ^ "versions: " ^ versions ^ " policies: " ^ policies)

let adjust_otr_policy mvar dump users default_cfg cfg contact data =
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
      let user = { contact with User.otr_custom_config = cfg ; active_sessions } in
      User.Users.replace users contact.User.bare_jid user ;
      (match cfg with
       | None -> dump "reverted to default otr policy"
       | Some x -> print_otr_policy dump "" x ) ;
      Lwt_mvar.put mvar user
    else
      (dump "nothing changed" ;
       Lwt.return_unit)
  with
    _ -> dump "unable to parse argument" ; return_unit

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
    Lwt_mvar.put state.state_mvar Disconnected >>= fun () ->
    msg "session error" (Printexc.to_string reason)
  in
  let contact = User.Users.find state.users (Jid.t_to_bare state.active_contact) in
  let dump data =
    let contact = User.Users.find state.users (Jid.t_to_bare state.active_contact) in
    let user = User.insert_message contact (`Local "") false false data in
    User.Users.replace state.users user.User.bare_jid user
  in
  let self = Jid.jid_matches (`Bare contact.User.bare_jid) state.myjid in
  let own_session () =
    let user = User.Users.find state.users (Jid.t_to_bare state.myjid) in
    let resource = match Jid.resource state.myjid with Some x -> x | None -> "NONE" in
    List.find (fun s -> s.User.resource = resource) user.User.active_sessions
  in

  match cmd_arg input with
  (* completely independent *)
  | ("help" , x) -> handle_help (msg ?prefix:None) x
  | ("clear", _) ->
    let user = { contact with User.message_history = [] } in
    User.Users.replace state.users user.User.bare_jid user ;
    return_unit

  (* connect *)
  | ("connect", _) ->
    ( match !xmpp_session with
      | None   -> handle_connect ?out state config log redraw failure
      | Some _ -> err "already connected" )

  (* disconnect *)
  | ("disconnect", _) ->
    ( match !xmpp_session with
      | Some x ->
        handle_disconnect x state.users (msg ?prefix:None) >>= fun () ->
        Lwt_mvar.put state.state_mvar Disconnected
      | None   -> err "not connected" )

  (* own things *)
  | ("status", x) ->
    ( match !xmpp_session, x with
      | None  , _      -> err "not connected"
      | Some _, None   -> handle_help (msg ~prefix:"argument required") (Some "status")
      | Some s, Some a ->
        handle_status s (own_session ()) failure a >>= (function
          | None   -> handle_help (msg ~prefix:"unknown argument") (Some "status")
          | Some _ -> return_unit ) )
  | ("priority", x) ->
    ( match !xmpp_session, x with
      | None  , _      -> err "not connected"
      | Some _, None   -> handle_help (msg ~prefix:"argument required") (Some "priority")
      | Some s, Some p ->
        handle_priority s (own_session ()) failure p >>= (function
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
      | Some a when a = "on"  -> handle_log state.user_mvar state.users dump contact true a
      | Some a when a = "off" -> handle_log state.user_mvar state.users dump contact false a
      | Some _ -> handle_help (msg ~prefix:"unknown argument") (Some "log") )

  | ("info", _) ->
    ( if self then
        handle_own_info dump contact state.config_directory config (own_session ()).User.resource
      else
        handle_info dump contact state.config_directory );
    return_unit

  | ("otrpolicy", x) ->
    let cfg, pref = match contact.User.otr_custom_config with
      | None -> (config.Config.otr_config, "default ")
      | Some x -> (x, "")
    in
    (match x with
     | None -> print_otr_policy dump pref cfg ; return_unit
     | Some _ when self -> err "cannot adjust own otr policy"
     | Some z -> adjust_otr_policy state.user_mvar dump state.users config.Config.otr_config cfg contact z
    )

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
              let cfg = match contact.User.otr_custom_config with
                | None -> config.Config.otr_config
                | Some x -> x
              in
              handle_otr_start s state.users dump failure cfg config.Config.dsa contact
          | Some s when x = "stop" ->
            if self then err "do not like to talk to myself" else
              handle_otr_stop s state.users dump err failure contact
          | Some _ -> handle_help (msg ~prefix:"unknown argument") (Some "otr") ) )
  | ("smp", x) ->
    ( if self then
        err "do not like to talk to myself"
      else
        match !xmpp_session with
        | Some s -> ( match User.active_session contact with
            | Some session when User.encrypted session.User.otr -> ( match x with
                | None -> handle_help (msg ~prefix:"argument required") (Some "smp")
                | Some x when x = "abort" -> handle_smp_abort state.users s session contact dump failure
                | Some x ->
                  match split_ws x with
                  | x, Some arg when x = "start" -> handle_smp_start state.users s session contact dump failure arg
                  | x, Some arg when x = "answer" -> handle_smp_answer state.users s session contact dump failure arg
                  | _ -> handle_help (msg ~prefix:"argument required") (Some "smp") )
            | _ -> err "need a secure session, use /otr start first" )
        | None -> err "not connected" )
  | ("remove", _) ->
    (match !xmpp_session with
     | Some s -> handle_remove s dump contact failure
     | None   -> err "not connected")

  | ("fingerprint", x) ->
    ( match x with
      | None    ->
        ( if self then
            handle_own_otr_info dump config
          else
            (handle_own_otr_info dump config ;
             current_otr_fp dump contact ;
             dump "/fingerprint: argument expected") ) ;
        return_unit
      | Some fp ->
        if self then err "won't talk to myself" else
          handle_fingerprint state.user_mvar state.users dump err fp contact )
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
