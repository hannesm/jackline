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
  new_command
    "add" "/add [jid]"
    "adds jid to your contact list, and sends a subscription request" [] ;
  new_command
    "authorization" "/authorization [argument]"
    "changes presence subscription of the current contact to argument -- one of 'allow', 'cancel', 'request', 'request_unsubscribe'"
    [ "allow" ; "cancel" ; "request" ; "request_unsubscribe" ] ;
  new_command
    "connect" "/connect" "connects to the server" [] ;
  new_command
    "fingerprint" "/fingerprint [fp]"
    "verifies the current contact's OTR fingerprint (fp must match the one used in the currently established session)" [] ;
  new_command
    "status" "/status [presence] [message]"
    "sets your presence -- one of 'free' 'away' 'dnd' 'xa' 'offline' or 'online' and status message"
    [ "free" ; "away" ; "dnd" ; "xa" ; "offline" ; "online" ] ;
  new_command
    "quit" "/quit" "exits this client" [] ;
  new_command
    "info" "/info" "displays information about the current session" [] ;
  new_command
    "otr" "/otr [argument]" "manages OTR session by argument -- one of 'start' 'stop' or 'info'"
    [ "start" ; "stop" ; "info" ] ;
  new_command
    "log" "/log [on|off]" "enable or disable logging for current contact" [ "on" ; "off" ] ;
  new_command
    "priority" "/priority [number]" "set your presence priority to number" [ ] ;
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
  and received jid msg =
    let now = Unix.localtime (Unix.time ()) in
    log (now, jid, msg)
  and notify u =
    (if (List.mem u state.notifications) || (fst state.active_chat = u) then
       ()
     else
       state.notifications <- u :: state.notifications) ;
    redraw ()
  and users = state.users
  in
  let (user_data : Xmpp_callbacks.user_data) = {
      Xmpp_callbacks.otr_config = otr_config ;
      Xmpp_callbacks.users      = users      ;
      Xmpp_callbacks.received   = received   ;
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

let handle_fingerprint dump err a = function
  | user, Some session when User.encrypted session.User.otr ->
    let manual_fp = string_normalize_fingerprint a in
    ( match User.fingerprint session.User.otr with
      | _, Some raw_cur_fp when raw_cur_fp = manual_fp ->
        let otr_fp = User.find_raw_fp user raw_cur_fp in
        User.replace user { otr_fp with User.verified = true } ;
        dump ("fingerprint " ^ a ^ " is now marked verified") ;
        return_unit
      | _ -> err "provided fingerprint does not match the one of this active session" )
  | _ -> err "no active OTR session"

let handle_log dump (user, _) v a =
  dump ("logging turned " ^ a) ;
  user.User.preserve_history <- v ;
  return_unit

let handle_authorization s failure dump (user, _) arg =
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
    fp.User.data ^ " " ^ ver ^ " (used in " ^ used ^ " sessions)"
  in
  String.concat ", " (List.map marshal_otr fps)

let handle_otr_info dump (user, active_session) =
  match active_session with
  | Some session ->
    dump ("otr session " ^ session.User.resource ^ ": " ^ Otr.State.session_to_string session.User.otr) ;
    dump ("otr fingerprints: " ^ (dump_otr_fps user.User.otr_fingerprints))
  | None ->
    dump ("(no active session) OTR fingerprints: " ^ (dump_otr_fps user.User.otr_fingerprints))

let handle_info dump cfgdir (user, active_session) =
  let dump a b = dump (a ^ ": " ^ b) in
  dump "jid" user.User.jid ;
  ( match user.User.name with
    | None -> ()
    | Some x -> dump "name" x ) ;
  ( if user.User.preserve_history then
      let histo =
        let dir = Persistency.history_dir cfgdir in
        Filename.concat dir user.User.jid
      in
      dump "persistent history in " histo ) ;
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
  dump "otr fingerprints" (dump_otr_fps user.User.otr_fingerprints) ;
  let marshal_session s =
    let prio = string_of_int s.User.priority in
    let pres = User.presence_to_string s.User.presence in
    let status = match s.User.status with
      | None -> ""
      | Some x -> " - " ^ x
    in
    s.User.resource ^ " (" ^ prio ^ "): " ^ pres ^ status
  in
  List.iteri (fun i s ->
      let act = match active_session with
        | Some x when x = s -> " (active)"
        | _ -> ""
      in
      dump ("session " ^ (string_of_int i) ^ act) (marshal_session s) ;
      dump "otr" (Otr.State.session_to_string s.User.otr))
    user.User.active_sessions

let handle_otr_start s dump failure otr_cfg (user, active_session) =
  let send_over resource body =
    let jid_to =
      let r = if resource = "" then "" else "/" ^ resource in
      JID.of_string (user.User.jid ^ r)
    in
    (try_lwt Xmpp_callbacks.XMPPClient.(send_message s ~kind:Chat ~jid_to ?body ())
     with e -> failure e)
  in
  match active_session with
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

let handle_otr_stop s dump err failure (user, active_session) =
  let send_over resource body =
    let jid_to =
      let r = if resource = "" then "" else "/" ^ resource in
      JID.of_string (user.User.jid ^ r)
    in
    (try_lwt Xmpp_callbacks.XMPPClient.(send_message s ~kind:Chat ~jid_to ?body ())
     with e -> failure e)
  in
  match active_session with
  | Some session ->
    let ctx, out = Otr.Handshake.end_otr session.User.otr in
    session.User.otr <- ctx ;
    dump "finished OTR session" ;
    send_over session.User.resource out
  | None -> err "no active session"

let tell_user (log:(Unix.tm * string * string) -> unit) ?(prefix:string option) (from:string) (msg:string) =
  let now = Unix.localtime (Unix.time ()) in
  let f = match prefix with
    | None -> from
    | Some x -> x ^ "; " ^ from
  in
  log (now, f, msg) ;
  return_unit

let exec ?out input state config log redraw =
  let msg = tell_user log in
  let err = msg "error" in
  let failure reason =
    xmpp_session := None ;
    msg "session error" (Printexc.to_string reason)
  in
  let dump data = User.new_message (fst state.active_chat) `Local false false data in

  match cmd_arg input with
  | ("help", x) -> handle_help (msg ?prefix:None) x
  | ("connect", _) ->
    ( match !xmpp_session with
      | None   -> handle_connect ?out state config log redraw failure
      | Some _ -> err "already connected" )
  | ("status", x) ->
    ( match !xmpp_session, x with
      | None  , _      -> err "not connected"
      | Some _, None   -> handle_help (msg ~prefix:"argument required") (Some "status")
      | Some s, Some a -> handle_status s failure state.session a >>= (function
          | None   -> handle_help (msg ~prefix:"unknown argument") (Some "status")
          | Some _ -> return_unit ) )
  | ("add", x) ->
    ( match !xmpp_session, x with
      | None  , _      -> err "not connected"
      | Some _, None   -> handle_help (msg ~prefix:"argument required") (Some "add")
      | Some s, Some a -> handle_add s failure (msg ?prefix:None) a )
  | ("fingerprint", x) ->
    ( match x with
      | None   -> handle_help (msg ~prefix:"argument required") (Some "fingerprint")
      | Some a -> handle_fingerprint dump err a state.active_chat )
  | ("log", x) ->
    ( match x with
      | None   -> handle_help (msg ~prefix:"argument required") (Some "log")
      | Some a when a = "on"  -> handle_log dump state.active_chat true a
      | Some a when a = "off" -> handle_log dump state.active_chat false a
      | Some _ -> handle_help (msg ~prefix:"unknown argument") (Some "log") )
  | ("authorization", x) ->
    ( match !xmpp_session, x with
      | None  , _      -> err "not connected"
      | Some _, None   -> handle_help (msg ~prefix:"argument required") (Some "authorization")
      | Some s, Some a -> handle_authorization s failure dump state.active_chat a >>= (function
        | None   -> handle_help (msg ~prefix:"unknown argument") (Some "authorization")
        | Some _ -> return_unit ) )
  | ("info", _) -> handle_info dump state.config_directory state.active_chat ; return_unit
  | ("otr", x) ->
    ( match x with
      | None -> handle_help (msg ~prefix:"arguent required") (Some "otr")
      | Some x when x = "info"  -> handle_otr_info dump state.active_chat ; return_unit
      | Some x -> (match !xmpp_session with
          | None -> err "not connected"
          | Some s when x = "start" -> handle_otr_start s dump failure config.Config.otr_config state.active_chat
          | Some s when x = "stop" -> handle_otr_stop s dump err failure state.active_chat
          | Some _ -> handle_help (msg ~prefix:"unknown argument") (Some "otr") ) )
  | ("priority", x) ->
    ( match !xmpp_session, x with
      | None  , _      -> err "not connected"
      | Some _, None   -> handle_help (msg ~prefix:"argument required") (Some "priority")
      | Some s, Some p -> handle_priority s failure state.session p >>= (function
          | None   -> handle_help (msg ~prefix:"unknown argument") (Some "priority")
          | Some _ -> return_unit ) )
  | _ -> handle_help (msg ~prefix:"unknown command") None
