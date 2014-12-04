open Cli_state

type command = {
  name : string ;
  documentation : string ;
  completion : string -> string list ;
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

let new_command name documentation completion =
  Commands.add commands name { name ; documentation ; completion }

let _ =
  new_command
    "add" "[/add jid] adds jid to your buddy list, and sends a subscription request"
    (fun _ -> []) ;
  new_command
    "authorization" "[/authorization new] changes presence subscription of the current user to new"
    (fun arg ->
       let subcommands = [ "allow" ; "cancel" ; "request" ; "request_unsubscribe" ] in
       List.filter (fun f -> Zed_utf8.starts_with f arg) subcommands) ;
  new_command
    "connect" "[/connect] connects to the server"
    (fun _ -> []) ;
  new_command
    "status" "[/status presence message] sets your presence [one of 'free' 'away' 'dnd' 'xa' 'offline' or 'online'] and status message"
    (fun arg ->
       let subcmds = [ "free" ; "away" ; "dnd" ; "xa" ; "offline" ; "online" ] in
       List.filter (fun f -> Zed_utf8.starts_with f arg) subcmds) ;
  new_command
    "quit" "[/quit] exits this client"
    (fun _ -> []) ;
  new_command
    "help" "[/help [cmd]] shows available commands or detailed help for cmd"
    (fun arg ->
       let cmds = keys () in
       List.filter (fun f -> Zed_utf8.starts_with f arg) cmds)

let split_ws s =
  let l = String.length s in
  let ws = try String.index s ' ' with Not_found -> l in
  let arg = if ws = l then None
    else
      let ws' = succ ws in
      let str = String.sub s ws' (l - ws') in
      Some str
  in
  (String.sub s 0 ws, arg)

let cmd_arg input =
  let open String in
  let l = length input in
  assert (l > 0) ;
  assert (get input 0 = '/') ;
  split_ws (sub input 1 (pred l))

let topcompletion input =
  if String.(length input > 0 && get input 0 = '/') then
    match cmd_arg input with
    | (cmd, None) when Commands.mem commands cmd ->
      let command = Commands.find commands cmd in
      List.map (fun f -> cmd ^ " " ^ f) (command.completion "")
    | (cmd, None) ->
      let cmds = keys () in
      List.filter (fun f -> Zed_utf8.starts_with f cmd) cmds
    | (cmd, Some arg) when Commands.mem commands cmd ->
      let command = Commands.find commands cmd in
      List.map (fun f -> cmd ^ " " ^ f) (command.completion arg)
    | (cmd, Some arg) -> [cmd ^ " " ^ arg]
  else
    [input]

open Lwt

let exec input state config session_data log redraw =
  let now = Unix.localtime (Unix.time ()) in
  let msg, err =
    let msg from m = log (now, from, m) ; return_unit in
    let err m = msg "error" m >|= fun () -> session_data in
    (msg, err)
  in
  match cmd_arg input with
  | ("help", Some arg) when Commands.mem commands (String.trim arg) ->
    let a = String.trim arg in
    let cmd = Commands.find commands a in
    msg ("help for " ^ a) cmd.documentation >|= fun () ->
    (true, session_data)
  | ("help", _) ->
    let cmds = String.concat " " (keys ()) in
    msg "available commands" cmds >|= fun () ->
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
       (Xmpp_callbacks.connect config user_data () >|= fun s -> Some s) >>= fun session_data ->
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
     | Some _, ("connect", _) -> err "already connected"
     | _ -> err "unknown command or not connected" ) >|= fun s -> (true, s)
