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

let exec ?out input state config session_data log redraw =
  let now = Unix.localtime (Unix.time ()) in
  let msg, err =
    let msg from m = log (now, from, m) ; return_unit in
    let err m = msg "error" m >|= fun () -> session_data in
    (msg, err)
  in
  match cmd_arg input with

  | ("help", Some arg) when Commands.mem commands arg ->
    let cmd = Commands.find commands arg in
    msg cmd.command_line cmd.documentation >|= fun () ->
    (true, session_data)

  | ("help", _) ->
    let cmds = String.concat " " (keys ()) in
    msg "available commands (try [/help cmd])" cmds >|= fun () ->
    (true, session_data)

  | ("quit", _) ->
    msg "self-destruction mechanism initiated" "have a nice day" >|= fun () ->
    (false, session_data)

  | x ->
    (match session_data, x with
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
         }) in
       (* TODO: I'd like to catch tls and auth failures here, but neither try_lwt nor Lwt.catch seem to do that *)
       (Xmpp_callbacks.connect ?out config user_data () >|= fun s -> Some s) >>= fun session_data ->
       (match session_data with
        | None -> return None
        | Some s ->
          Lwt.async (fun () -> Xmpp_callbacks.parse_loop s) ;
          return (Some s))

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
       send_presence s ?kind ?show ?status () >|= fun () ->
       session_data

     | Some s, ("add", Some arg) ->
       (try
          let jid_to = JID.of_string arg in
          Xmpp_callbacks.XMPPClient.(send_presence s ~jid_to ~kind:Subscribe ()) >>= fun () ->
          msg arg "has been asked to sent presence updates to you" >|= fun () ->
          session_data
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
                   msg user.User.jid "fingerprint is now verified" >|= fun () ->
                   session_data)
             | _ -> err "provided fingerprint does not match the one of this active session" )
         | _ -> err "no active OTR session"
       end

     | Some s, ("authorization", Some arg) ->
       let open Xmpp_callbacks.XMPPClient in
       let user = fst state.active_chat in
       let jid = user.User.jid in
       let doit kind m =
         send_presence s ~jid_to:(JID.of_string jid) ~kind () >>= fun () ->
         msg jid m >|= fun () -> session_data
       in
       ( match arg with
         | "allow" -> doit Subscribed "is allowed to receive your presence updates"
         | "cancel" -> doit Unsubscribed "won't receive your presence updates"
         | "request" -> doit Subscribe "has been asked to sent presence updates to you"
         | "request_unsubscribe" -> doit Unsubscribe "has been asked to no longer sent presence updates to you"
         | _ -> err "don't know what you want" )

     | Some s, ("info", _) ->
       let user = fst state.active_chat in
       msg "jid" user.User.jid >>= fun () ->
       ( match user.User.name with
         | None -> return_unit
         | Some x -> msg "name" x ) >>= fun () ->
       ( match user.User.groups with
         | [] -> return_unit
         | xs -> msg "groups" (String.concat ", " xs) ) >>= fun () ->
       let add =
         ( if List.mem `Pending user.User.props then
             "pending "
           else
             "" ) ^
         ( if List.mem `PreApproved user.User.props then
             "preapproved"
           else
             "" )
       in
       let add = if String.length add > 0 then " (" ^ (String.trim add) ^ ")" else "" in
       msg "subscription" ((User.subscription_to_string user.User.subscription) ^ add) >>= fun () ->
       let marshal_otr fp =
         let ver = if fp.User.verified then "verified" else "unverified" in
         let used = string_of_int fp.User.session_count in
         fp.User.data ^ " " ^ ver ^ " (used in " ^ used ^ " sessions)"
       in
       msg "otr fingerprints" (String.concat ", " (List.map marshal_otr user.User.otr_fingerprints)) >>= fun () ->
       let marshal_session s =
         let prio = string_of_int s.User.priority in
         let pres = User.presence_to_string s.User.presence in
         let status = match s.User.status with
           | None -> ""
           | Some x -> " - " ^ x
         in
         s.User.resource ^ " (" ^ prio ^ "): " ^ pres ^ status
       in
       Lwt_list.iteri_s (fun i s ->
           let act = match snd state.active_chat with
             | Some x when x = s -> " (active)"
             | _ -> ""
           in
           msg ("session " ^ (string_of_int i) ^ act) (marshal_session s) >>= fun () ->
           msg "otr" (Otr.State.session_to_string s.User.otr))
         user.User.active_sessions >|= fun () ->
       session_data

     | Some s, ("otr", arg) ->
       let user = fst state.active_chat in
       let send_over resource body =
         let jid_to =
           let r = if resource = "" then "" else "/" ^ resource in
           JID.of_string (user.User.jid ^ r)
         in
         Xmpp_callbacks.XMPPClient.(send_message s ~kind:Chat ~jid_to ?body ()) >|= fun () ->
         session_data
       in
       let add_msg session data =
         let msg =
           let now = Unix.localtime (Unix.time ()) in
           (`Local, false, false, now, data)
         in
         session.User.messages <- msg :: session.User.messages
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
           send_over session.User.resource (Some out)
         | (user, Some session), Some "stop" ->
           let ctx, out = Otr.Handshake.end_otr session.User.otr in
           session.User.otr <- ctx ;
           add_msg session "finished OTR session" ;
           send_over session.User.resource out
         | (user, Some session), Some "info" ->
           msg ("otr session " ^ session.User.resource) (Otr.State.session_to_string session.User.otr) >>= fun () ->
           msg "otr fingerprints" (String.concat ", " (List.map marshal_otr user.User.otr_fingerprints)) >|= fun () ->
           session_data
         | (user, None), Some "info" ->
           msg "(no active session) OTR fingerprints" (String.concat ", " (List.map marshal_otr user.User.otr_fingerprints)) >|= fun () ->
           session_data
         | (user, None), Some "start" ->
           (* no OTR context, but we're sending only an OTR query anyways
              (and if we see a reply, we'll get some resource from the other side) *)
           let ctx = Otr.State.new_session config.Config.otr_config () in
           let _, out = Otr.Handshake.start_otr ctx in
           send_over "" (Some out)
         | (user, None), Some "stop" -> err "no active session"
         |  _ -> err "unknown argument (/otr [start|stop|info])"
       )

     | Some _, ("connect", _) -> err "already connected"

     | _ -> err "unknown command or not connected, try /help" ) >|= fun s -> (true, s)
