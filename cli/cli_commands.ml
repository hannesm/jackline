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
    "add" "/add jid"
    "adds jid to your contact list, and sends a subscription request" [] ;
  new_command
    "authorization" "/authorization sub"
    "changes presence subscription of the current contact to sub -- one of 'allow', 'cancel', 'request', 'request_unsubscribe'"
    [ "allow" ; "cancel" ; "request" ; "request_unsubscribe" ] ;
  new_command
    "connect" "/connect" "connects to the server" [] ;
  new_command
    "fingerprint" "/fingerprint fp"
    "verifies the current contact's OTR fingerprint (fp must match the one used in the currently established session)" [] ;
  new_command
    "status" "/status presence message"
    "sets your presence [one of 'free' 'away' 'dnd' 'xa' 'offline' or 'online'] and status message"
    [ "free" ; "away" ; "dnd" ; "xa" ; "offline" ; "online" ] ;
  new_command
    "quit" "/quit" "exits this client" [] ;
  new_command
    "info" "/info" "displays information about the current session" [] ;
  new_command
    "otr" "/otr sub" "manages OTR session [sub is one of 'start' 'stop' or 'info']"
    [ "start" ; "stop" ; "info" ] ;
  new_command
    "log" "/log on|off" "enable or disable logging for current contact" [ "on" ; "off" ] ;
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
      Some (String.trim str)
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

let exec ?out input state config log redraw =
  let now = Unix.localtime (Unix.time ()) in
  let msg, err =
    let msg from m = log (now, from, m) ; return_unit in
    let err m = msg "error" m in
    (msg, err)
  in
  let failure reason =
    xmpp_session := None ;
    msg "session error" (Printexc.to_string reason)
  in
  let dump data = User.new_message (fst state.active_chat) `Local false false data in

  match cmd_arg input with

  | ("help", Some arg) when Commands.mem commands arg ->
    let cmd = Commands.find commands arg in
    msg cmd.command_line cmd.documentation >|= fun () ->
    true

  | ("help", _) ->
    let cmds = String.concat " " (keys ()) in
    msg "available commands (try [/help cmd])" cmds >|= fun () ->
    true

  | ("quit", _) ->
    msg "self-destruction mechanism initiated" "have a nice day" >|= fun () ->
    false

  | x ->
    (match !xmpp_session, x with
     | None, ("connect", _) ->
       let otr_config = config.Config.otr_config in
       let received jid msg =
         let now = Unix.localtime (Unix.time ()) in
         log (now, jid, msg)
       and notify u =
         (if (List.mem u state.notifications) || (fst state.active_chat = u) then
            ()
          else
            state.notifications <- u :: state.notifications) ;
         redraw ()
       in
       let (user_data : Xmpp_callbacks.user_data) = Xmpp_callbacks.({
           otr_config ;
           users = state.users ;
           received ;
           notify ;
           failure ;
         }) in
       Xmpp_callbacks.connect ?out config user_data () >|= (function
           | None -> ()
           | Some s ->
             xmpp_session := Some s ;
             Lwt.async (fun () -> Xmpp_callbacks.parse_loop s))

     | Some s, ("status", Some arg) ->
       let open Xmpp_callbacks.XMPPClient in
       let kindshow p = match User.string_to_presence p with
         | `Offline -> (Some Unavailable, None)
         | `Online -> (None, None)
         | `Free -> (None, Some ShowChat)
         | `Away -> (None, Some ShowAway)
         | `DoNotDisturb -> (None, Some ShowDND)
         | `ExtendedAway -> (None, Some ShowXA)
       in
       let p, status = split_ws arg in
       let kind, show = kindshow p in
       (try_lwt send_presence s ?kind ?show ?status ()
        with e -> failure e)

     | Some s, ("add", Some arg) ->
       (try
          let jid_to = JID.of_string arg in
          (try_lwt
             (Xmpp_callbacks.XMPPClient.(send_presence s ~jid_to ~kind:Subscribe ()) >>= fun () ->
              msg arg "has been asked to sent presence updates to you")
           with e -> failure e)
        with _ -> err "parse of jid failed")

     | Some s, ("fingerprint", Some arg) ->
       begin
         match state.active_chat with
         | user, Some session when User.encrypted session.User.otr ->
           let manual_fp = string_normalize_fingerprint arg in
           ( match User.fingerprint session.User.otr with
             | cur_fp, Some raw_cur_fp when raw_cur_fp = manual_fp ->
               let fp = try
                   Some (List.find
                           (fun x -> x.User.data = manual_fp)
                           user.User.otr_fingerprints)
                 with Not_found -> None
               in
               ( match fp with
                 | None -> err "fingerprint didn't match"
                 | Some x ->
                   User.replace user { x with User.verified = true } ;
                   dump "fingerprint is now verified" ;
                   return_unit)
             | _ -> err "provided fingerprint does not match the one of this active session" )
         | _ -> err "no active OTR session"
       end

     | Some s, ("log", Some arg) ->
       let doit n =
         dump ("logging turned " ^ arg) ;
         (fst state.active_chat).User.preserve_history <- n ;
         return_unit
       in
       ( match arg with
         | "on"  -> doit true
         | "off" -> doit false
         | _ -> err "don't know what you want" )

     | Some s, ("authorization", Some arg) ->
       let open Xmpp_callbacks.XMPPClient in
       let user = fst state.active_chat in
       let jid = user.User.jid in
       let doit kind m =
         (try_lwt send_presence s ~jid_to:(JID.of_string jid) ~kind ()
          with e -> failure e) >|= fun () ->
         dump m
       in
       ( match arg with
         | "allow" -> doit Subscribed "is allowed to receive your presence updates"
         | "cancel" -> doit Unsubscribed "won't receive your presence updates"
         | "request" -> doit Subscribe "has been asked to sent presence updates to you"
         | "request_unsubscribe" -> doit Unsubscribe "has been asked to no longer sent presence updates to you"
         | _ -> err "don't know what you want" )

     | Some s, ("info", _) ->
       let user = fst state.active_chat in
       let dump a b = dump (a ^ ": " ^ b) in
       dump "jid" user.User.jid ;
       ( match user.User.name with
         | None -> ()
         | Some x -> dump "name" x ) ;
       ( if user.User.preserve_history then
           let histo =
             let dir = Persistency.history_dir state.config_directory in
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
       let marshal_otr fp =
         let ver = if fp.User.verified then "verified" else "unverified" in
         let used = string_of_int fp.User.session_count in
         fp.User.data ^ " " ^ ver ^ " (used in " ^ used ^ " sessions)"
       in
       dump "otr fingerprints" (String.concat ", " (List.map marshal_otr user.User.otr_fingerprints)) ;
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
           let act = match snd state.active_chat with
             | Some x when x = s -> " (active)"
             | _ -> ""
           in
           dump ("session " ^ (string_of_int i) ^ act) (marshal_session s) ;
           dump "otr" (Otr.State.session_to_string s.User.otr))
         user.User.active_sessions ;
       return_unit

     | Some s, ("otr", arg) ->
       let user = fst state.active_chat in
       let send_over resource body =
         let jid_to =
           let r = if resource = "" then "" else "/" ^ resource in
           JID.of_string (user.User.jid ^ r)
         in
         (try_lwt Xmpp_callbacks.XMPPClient.(send_message s ~kind:Chat ~jid_to ?body ())
          with e -> failure e)
       in
       let marshal_otr fp =
         let ver = if fp.User.verified then "verified" else "unverified" in
         let used = string_of_int fp.User.session_count in
         fp.User.data ^ " " ^ ver ^ " (used in " ^ used ^ " sessions)"
       in
       ( match state.active_chat, arg with
         | (user, Some session), Some "start" ->
           let ctx, out = Otr.Handshake.start_otr session.User.otr in
           session.User.otr <- ctx ;
           dump "starting OTR session" ;
           send_over session.User.resource (Some out)
         | (user, Some session), Some "stop" ->
           let ctx, out = Otr.Handshake.end_otr session.User.otr in
           session.User.otr <- ctx ;
           dump "finished OTR session" ;
           send_over session.User.resource out
         | (user, Some session), Some "info" ->
           dump ("otr session " ^ session.User.resource ^ ": " ^ Otr.State.session_to_string session.User.otr) ;
           dump ("otr fingerprints: " ^ String.concat ", " (List.map marshal_otr user.User.otr_fingerprints)) ;
           return_unit
         | (user, None), Some "info" ->
           dump ("(no active session) OTR fingerprints: " ^ String.concat ", " (List.map marshal_otr user.User.otr_fingerprints)) ;
           return_unit
         | (user, None), Some "start" ->
           (* no OTR context, but we're sending only an OTR query anyways
              (and if we see a reply, we'll get some resource from the other side) *)
           let ctx = Otr.State.new_session config.Config.otr_config () in
           let _, out = Otr.Handshake.start_otr ctx in
           dump "starting OTR session" ;
           send_over "" (Some out)
         | (user, None), Some "stop" -> err "no active session"
         |  _ -> err "unknown argument (/otr [start|stop|info])"
       )

     | Some _, ("connect", _) -> err "already connected"

     | _ -> err "unknown command or not connected, try /help" ) >|= fun () ->
    true
