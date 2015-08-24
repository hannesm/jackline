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

let handle_connect state log redraw failure =
  let remove jid =
    let bare = User.Jid.t_to_bare jid in
    User.Users.remove state.users bare ;
    if User.Jid.jid_matches (`Bare bare) state.active_contact then
      state.active_contact <- `Full state.config.Config.jid ;
    if User.Jid.jid_matches (`Bare bare) state.last_active_contact then
      state.last_active_contact <- `Full state.config.Config.jid ;
    redraw ()
  and log dir txt =
    log (dir, txt)
  and locallog str txt =
    let d = `Local (state.active_contact, str) in
    log (d, txt)
  and message jid dir enc txt =
    User.add_message state.users jid dir enc true txt ;
    notify state jid ;
    redraw ()
  and receipt jid id =
    let user = User.find_or_create state.users jid in
    let user = User.received_message user id in
    User.replace_user state.users user ;
    redraw ()
  and user jid =
    User.find_or_create state.users jid
  and session jid =
    let otr_config = otr_config (User.find_or_create state.users jid) state in
    User.session state.users jid otr_config state.config.Config.dsa
  and update_session jid session =
    let user = User.find_or_create state.users jid in
    User.replace_session state.users user session
  and update_user user alert =
    User.replace_user state.users user ;
    if alert then notify state (`Bare user.User.bare_jid) ;
    redraw ()
  and inc_fp jid raw_fp =
    match User.Jid.resource jid with
    | None -> assert false
    | Some resource ->
       let user = User.find_or_create state.users jid in
       let fp = User.find_raw_fp user raw_fp in
       let resources =
         if List.mem resource fp.User.resources then
           fp.User.resources
         else
           resource :: fp.User.resources
       in
       let fp = { fp with User.session_count = succ fp.User.session_count ; User.resources = resources } in
       let u = User.replace_fp user fp in
       User.replace_user state.users u ;
       Lwt.async (fun () -> Lwt_mvar.put state.user_mvar u) ;
       (fp.User.verified, pred fp.User.session_count, List.exists (fun x -> x.User.verified) user.User.otr_fingerprints)
  in
  let (user_data : Xmpp_callbacks.user_data) = {
      Xmpp_callbacks.log ;
      locallog ;
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
  Lwt_mvar.put state.Cli_state.connect_mvar (Connect user_data)

let handle_disconnect msg =
  Connect.disconnect () >|= fun () ->
  msg "session" "disconnected"

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
   with e -> failure e)

let handle_status session arg =
  let p, status = split_ws arg in
  match User.string_to_presence p with
  | None   -> None
  | Some x -> Some (x, status, session.User.priority)

let handle_priority session p =
  try
    let prio = int_of_string p in
    assert (prio >= -128 && prio <= 127) ; (* RFC 6121 4.7.2.3 *)
    Some (session.User.presence, session.User.status, prio)
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

let handle_fingerprint user session err fp =
  Utils.option
    (err "no active OTR session")
    (fun s ->
     if User.encrypted s.User.otr then
       let manual_fp = string_normalize_fingerprint fp in
       (match Otr.Utils.their_fingerprint s.User.otr with
        | Some key when User.hex_fingerprint key = manual_fp ->
           let otr_fp = User.find_raw_fp user manual_fp in
           let user = User.replace_fp user { otr_fp with User.verified = true } in
           (["fingerprint " ^ fp ^ " is now marked verified"], Some user, None)
        | _ -> err "provided fingerprint does not match the one of this active session")
     else
       err "no active OTR session")
    session

let handle_log user v a =
  if user.User.preserve_messages <> v then
    let user = { user with User.preserve_messages = v } in
    (["logging turned " ^ a], Some user, None)
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
  let marshal_otr fp =
    let ver = if fp.User.verified then "verified" else "unverified" in
    let used = string_of_int fp.User.session_count in
    let resources = String.concat ", " fp.User.resources in
    "  " ^ ver ^ " " ^ User.format_fp fp.User.data ^ " (used in " ^ used ^ " sessions, resources: " ^ resources ^ ")"
  in
  "otr fingerprints:" :: List.map marshal_otr fps

let current_otr_fp session =
  Utils.option
    []
    (fun s -> Utils.option
                ["no active OTR session"]
                (fun fp -> ["their otr fingerprint: " ^ (User.format_fp fp)])
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
  ["your otr fingerprint:  " ^ (User.format_fp (User.hex_fingerprint otr_fp))]

let common_info user cfgdir =
  let name = match user.User.name with
    | None -> []
    | Some x -> ["name: " ^ x]
  and pres =
    match user.User.preserve_messages with
    | true ->
       let histo =
         let dir = Persistency.history in
         Filename.(concat (concat cfgdir dir) (User.jid user))
       in
       ["persistent history in: " ^ histo]
    | false -> []
  in
  [ "jid: " ^ (User.jid user) ] @ name @ pres

let marshal_session s =
  let prio = string_of_int s.User.priority in
  let pres = User.presence_to_string s.User.presence in
  let status = match s.User.status with
    | None -> ""
    | Some x -> " - " ^ x
  in
  let receipts = User.receipt_state_to_string s.User.receipt in
  s.User.resource ^ " (" ^ prio ^ ") (receipts " ^ receipts ^ "): " ^ pres ^ status

let handle_info user session cfgdir =
  let ci = common_info user cfgdir
  and groups =
    match user.User.groups with
    | [] -> []
    | xs -> ["groups: " ^ (String.concat ", " xs)]
  and add =
    let add =
      let ps x = List.mem x user.User.properties in
      ( if ps `Pending then "pending " else "" ) ^
        ( if ps `PreApproved then "preapproved" else "" )
    in
    let add = if String.length add > 0 then " (" ^ (String.trim add) ^ ")" else "" in
    ["subscription: " ^ ((User.subscription_to_string user.User.subscription) ^ add)]
  and sessions =
    List.map (fun s ->
      let act = match session with
        | Some x when x = s -> "(active) "
        | _ -> ""
      in
      act ^ (marshal_session s))
      (User.sorted_sessions user)
  in
  ci @ groups @ add @ sessions

let handle_own_info user session cfgdir dsa =
  let ci = common_info user cfgdir
  and otr_fp =
    let fp = Otr.Utils.own_fingerprint dsa in
    let formatted = User.format_fp (User.hex_fingerprint fp) in
    [ "own otr fingerprint: " ^ formatted ]
  and sessions =
    let active = User.active_session user in
    List.map (fun s ->
      let own = if s = session then " (own) " else " " in
      let act =
        match active with
        | Some x when x = s -> own ^ "(active) "
        | _ -> own
      in
      act ^ marshal_session s)
      user.User.active_sessions
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
      send s (`Bare user.User.bare_jid) None out failure
    in
    ([ "starting OTR session" ], None, Some clos)
  | Some session when User.encrypted session.User.otr ->
    ([ "session is already encrypted, please finish first (/otr stop)!" ], None, None)
  | Some session ->
    let ctx, out = Otr.Engine.start_otr session.User.otr in
    let user = User.replace_session_1 user { session with User.otr = ctx } in
    let clos = fun s failure ->
      send s (`Full (user.User.bare_jid, session.User.resource)) None out failure
    in
    ([ "starting OTR session" ], Some user, Some clos)

let handle_otr_stop user session err =
  match session with
  | None -> err "no active session"
  | Some session when User.encrypted session.User.otr ->
    let ctx, out = Otr.Engine.end_otr session.User.otr in
    let user = User.replace_session_1 user { session with User.otr = ctx } in
    let datas, clos = match out with
      | None   -> ([], None)
      | Some body ->
         let clos = fun s failure ->
           let jid = `Full (user.User.bare_jid, session.User.resource) in
           send s jid None body failure
         in
         ([ "finished OTR session" ], Some clos)
    in
    (datas, Some user, clos)
  | Some _ -> err "no active encrypted session"

let handle_smp_abort user session =
  let ctx, out, ret = Otr.Engine.abort_smp session.User.otr in
  let user = User.replace_session_1 user { session with User.otr = ctx } in
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
            send s jid None out failure)
  in
  (datas, Some user, clos)

let handle_smp_start user session args =
  let secret, question = match split_ws args with
    | question, Some secret -> (secret, Some question)
    | secret, None -> (secret, None)
  in
  let ctx, out, ret = Otr.Engine.start_smp session.User.otr ?question secret in
  let user = User.replace_session_1 user { session with User.otr = ctx } in
  let datas = List.fold_left (fun ds -> function
      | `Warning x -> ("SMP start warning: " ^ x) :: ds
      | _ -> ds )
    []
    (List.rev ret)
  in
  let clos =
    match out with
    | None   -> None
    | Some body ->
       Some (fun s failure ->
             let jid = `Full (user.User.bare_jid, session.User.resource) in
             send s jid None body failure)
  in
  (datas @ ["initiated SMP"], Some user, clos)

let handle_smp_answer user session secret =
  let ctx, out, ret = Otr.Engine.answer_smp session.User.otr secret in
  let user = User.replace_session_1 user { session with User.otr = ctx } in
  let datas = List.fold_left (fun ds -> function
      | `Warning x -> ("SMP answer warning: " ^ x) :: ds
      | _ -> ds)
    []
    (List.rev ret)
  in
  let clos =
    match out with
    | None   -> None
    | Some body ->
       Some (fun s failure ->
             let jid = `Full (user.User.bare_jid, session.User.resource) in
             send s jid None body failure)
  in
  (datas, Some user, clos)

let handle_remove user =
  fun s failure ->
  (try_lwt
     Xmpp_callbacks.(Roster.put ~remove:() s (User.jid user)
       (fun ?jid_from ?jid_to ?lang el ->
        ignore jid_to ; ignore lang ; ignore el ;
        match jid_from with
        | None -> fail XMPPClient.BadRequest
        | Some x -> match User.Jid.string_to_jid x with
                    | None -> fail XMPPClient.BadRequest
                    | Some jid ->
                       s.XMPPClient.user_data.log (`From jid) ("Removal of " ^ User.Jid.jid_to_string jid ^ " successful") ;
                       return_unit))
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
  let f = match prefix with
    | None -> from
    | Some x -> x ^ "; " ^ from
  in
  log (`Local (jid, f), msg)

let exec input state contact session self failure log redraw =
  let msg = tell_user log state.active_contact in
  let err = msg "error" in
  let own_session =
    let id, resource = state.config.Config.jid in
    match User.find_session (User.find_or_create state.users (`Bare id)) resource with
    | None -> assert false
    | Some x -> x
  in

  let global_things = ["add";"status";"priority"] in
  match cmd_arg input with
  (* completely independent *)
  | ("help" , x) -> handle_help (msg ?prefix:None) x ; Lwt.return_unit

  | other ->
     let err msg = err msg ; Lwt.return_unit in
     match other, !xmpp_session with
     (* connect *)
     | ("connect", _), None   -> handle_connect state log redraw failure
     | ("connect", _), Some _ -> err "already connected"

     (* disconnect *)
     | ("disconnect", _), Some _ -> handle_disconnect (msg ?prefix:None)
     | ("disconnect", _), None   -> err "not connected"

     (* commands not using active_contact *)
     | (x, _), None when List.mem x global_things -> err "not connected"
     | (x, None), _ when List.mem x global_things ->
        handle_help (msg ~prefix:"argument required") (Some x) ; Lwt.return_unit

     (* add *)
     | ("add", Some a), Some s -> handle_add s a (msg ?prefix:None) failure

     (* status *)
     | ("status", Some arg), Some s ->
        (match handle_status own_session arg with
         | None -> handle_help (msg ~prefix:"unknown argument") (Some "status") ; Lwt.return_unit
         | Some (pres, stat, prio) -> send_status s pres stat prio failure)

     (* priority *)
     | ("priority", Some p), Some s ->
        (match handle_priority own_session p with
         | None   -> handle_help (msg ~prefix:"unknown argument") (Some "priority") ; Lwt.return_unit
         | Some (pres, stat, prio) -> send_status s pres stat prio failure)

     (* commands using active_contact as context *)
     | other, s ->
        let err str =
          msg "error" str ; ([], None, None)
        in
        let handle_help msg arg =
          handle_help msg arg ; ([], None, None)
        in

        let datas, u, clos = match other, s with
          | ("clear", _), _ ->
             ([], Some { contact with User.message_history = [] }, None)

          | ("log", None), _ -> handle_help (msg ~prefix:"argument required") (Some "log")
          | ("log", Some a), _ when a = "on"  -> handle_log contact true a
          | ("log", Some a), _ when a = "off" -> handle_log contact false a
          | ("log", Some _), _ -> handle_help (msg ~prefix:"unknown argument") (Some "log")

          | ("info", _), _ ->
             let datas =
               if self then
                 handle_own_info contact own_session state.config_directory state.config.Config.dsa
               else
                 handle_info contact (session state) state.config_directory
             in
             (datas, None, None)

         | ("remove", _), None -> err "not connected"
         | ("remove", _), Some _ -> let clos = handle_remove contact in
                                    ([], None, Some clos)

         | ("fingerprint", None), _ ->
            let datas =
              handle_own_otr_info state.config.Config.dsa @
                current_otr_fp (session state)
            in
            (datas, None, None)
         | ("fingerprint", Some fp), _ ->
            if self then
              err "won't talk to myself"
            else
              handle_fingerprint contact (session state) err fp

         | ("authorization", _), None -> err "not connected"
         | ("authorization", None), _ -> handle_help (msg ~prefix:"argument required") (Some "authorization")
         | ("authorization", Some a), Some _ ->
            if self then
              err "won't authorize myself"
            else
              (match handle_authorization a with
               | None   -> handle_help (msg ~prefix:"unknown argument") (Some "authorization")
               | Some (kind, m) ->
                  let clos = fun s failure ->
                    (try_lwt
                       let jid_to = User.Jid.jid_to_xmpp_jid (`Bare contact.User.bare_jid) in
                       Xmpp_callbacks.XMPPClient.send_presence s ~jid_to ~kind ()
                     with e -> failure e)
                  in
                  ([m], None, Some clos))

         | ("otrpolicy", None), _ ->
            let cfg = otr_config contact state in
            (print_otr_policy cfg, None, None)
         | ("otrpolicy", Some _), _ when self -> err "cannot adjust own otr policy"
         | ("otrpolicy", Some z), _ ->
            let cfg = otr_config contact state in
            adjust_otr_policy state.config.Config.otr_config cfg contact z

         | ("otr", None), _ ->
            handle_help (msg ~prefix:"argument required") (Some "otr")
         | ("otr", Some "info"), _  ->
            if self then
              (handle_own_otr_info state.config.Config.dsa, None, None)
            else
              (handle_otr_info contact (session state), None, None)

         | ("otr", Some _), None  -> err "not connected"
         | ("otr", Some "start"), Some _ ->
            if self then
              err "do not like to talk to myself"
            else
              let cfg = otr_config contact state in
              handle_otr_start contact (session state) cfg state.config.Config.dsa

         | ("otr", Some "stop"), Some _ ->
            if self then
              err "do not like to talk to myself"
            else
              handle_otr_stop contact (session state) err

         | ("otr", Some _), _ -> handle_help (msg ~prefix:"unknown argument") (Some "otr")

         | ("smp", _), _ when self -> err "do not like to talk to myself"
         | ("smp", None), _ -> handle_help (msg ~prefix:"argument required") (Some "smp")
         | ("smp", _), None -> err "not connected"

         | ("smp", Some a), Some _ ->
            (match session state with
             | Some session when User.encrypted session.User.otr ->
                (match split_ws a with
                 | "abort", _ -> handle_smp_abort contact session
                 | "start", Some arg -> handle_smp_start contact session arg
                 | "answer", Some arg -> handle_smp_answer contact session arg
                 | _ -> handle_help (msg ~prefix:"argument required") (Some "smp"))
             | _ -> err "need a secure session, use /otr start first")
         | _ -> handle_help (msg ~prefix:"unknown command") None
        in

        let user old =
          let u = List.fold_left
                    (fun c d -> User.insert_message c (`Local (state.active_contact, "")) false false d)
                    old datas
          in
          User.replace_user state.users u ;
          u
        in
        (match u with
         | None   ->
            ignore (user contact) ;
            Lwt.return_unit
         | Some x ->
            let u = user x in
            Lwt_mvar.put state.user_mvar u) >>= fun () ->
        match clos, !xmpp_session with
        | Some x, Some s -> x s failure
        | Some _, None -> msg "error" "not connected" ; Lwt.return_unit
        | None, _ -> Lwt.return_unit
