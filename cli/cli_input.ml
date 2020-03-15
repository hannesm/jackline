open Lwt.Infix

open Cli_state
open Cli_support

type direction = Up | Down

let navigate_message_buffer state direction =
  match direction, state.scrollback with
  | Down, 0 -> state
  | Down, n ->
    let s = { state with scrollback = pred n } in
    if s.scrollback = 0 then maybe_clear s else s
  | Up, n -> { state with scrollback = succ n }

let history state dir =
  let active = active state in
  let cursor = Contact.history_position active in
  let active =
    let input =
      let pre, post = state.input in
      char_list_to_str (pre@post)
    in
    if List.mem input (Contact.readline_history active) then
      active
    else
      Contact.add_readline_history active input
  in
  let hitems = Contact.readline_history active in
  let l = List.length hitems in
  let activate p =
    let c = Contact.set_history_position active p in
    Contact.replace_contact state.contacts c ;
    let data = List.nth hitems p in
    { state with input = (str_to_char_list data, []) }
  in
  match dir with
  | Up -> activate (succ cursor mod l)
  | Down -> activate ((l + pred cursor) mod l)

let navigate_buddy_list state direction =
  let userlist = all_jids state in
  let set_active idx =
    let user = List.nth userlist idx in
    activate_contact state user
  and active_idx = Utils.find_index Xjid.jid_matches state.active_contact 0 userlist
  in
  let l = List.length userlist in
  match direction with
  | Down -> set_active (succ active_idx mod l)
  | Up -> set_active ((l + pred active_idx) mod l)

let next_crypto_buddy state =
  let enc_contacts =
    List.fold_left (fun acc c ->
        match c with
        | `Room _ -> acc
        | `User u ->
          if Contact.expanded c then begin
            List.fold_left
              (fun acc -> function
                 | `Session s when User.encrypted s.User.otr ->
                   let jid = `Full (u.User.bare_jid, s.User.resource) in
                   jid :: acc
                 | _ -> acc)
              acc (active_resources state c)
          end else begin
            match User.active_session u with
              | Some s when User.encrypted s.User.otr ->
                `Bare u.User.bare_jid :: acc
              | _ -> acc
          end)
      [] (active_contacts state)
  in
  match List.rev enc_contacts with
  | [] -> state
  | [ x ] -> activate_contact state x
  | act ->
    let us = state.active_contact in
    let _, next =
      List.fold_left (fun (i, r) jid ->
          match r with
          | Some x -> 0, Some x
          | None ->
            let r =
              match jid, us with
              | `Bare bare, `Bare bare' -> Xjid.compare_bare_jid bare bare'
              | `Full (bare, _), `Bare bare' -> Xjid.compare_bare_jid bare bare'
              | `Bare bare, `Full (bare', _) -> Xjid.compare_bare_jid bare bare'
              | `Full (bare, res), `Full (bare', res') ->
                match Xjid.compare_bare_jid bare' bare with
                | 0 -> String.compare res' res
                | x -> x
            in
            match r with
            | 0 -> 0, Some (succ i)
            | 1 -> 0, Some i
            | _ -> succ i, None)
        (0, None) act
    in
    let idx = match next with
      | None -> 0
      | Some start -> start
    in
    let num = List.length act in
    let next = List.nth act (idx mod num) in
    activate_contact state next


let warn jid user add_msg =
  let last_msg =
    try Some (List.find
                (fun m -> match m.User.direction with
                          | `From (`Full _) -> true
                          | `Local ((`Bare _), s) when s = "resource warning" -> true
                          | _ -> false)
                user.User.message_history)
    with Not_found -> None
  in
  match last_msg, jid with
  | Some m, `Full (_, r) ->
     (match m.User.direction with
      | `From (`Full (_, r'))  when not (Xjid.resource_similar r r') ->
         let msg =
           "message sent to the active resource, " ^ r ^ ", while the last \
            message was received from " ^ r' ^ "."
         in
         add_msg (`Local (`Bare (Xjid.t_to_bare jid), "resource warning")) false msg
      | _ -> ())
  | _ -> ()

let send_msg t state active_user message =
  let handle_otr_out jid user_out =
    let add_msg ?kind direction encrypted data =
      let msg = User.message ?kind direction encrypted false data in
      let u = active state in
      let u = Contact.new_message u msg in
      Contact.replace_contact state.contacts u
    in
    (match active state with
     | `User u -> warn jid u add_msg
     | `Room _ -> ()) ;
    match user_out with
    | `Warning msg      ->
       add_msg ~kind:`Warning (`Local (jid, "OTR Warning")) false msg ;
       ""
    | `Sent m           ->
       let id = random_string () in
       add_msg (`To (jid, id)) false m ;
       id
    | `Sent_encrypted m ->
       let id = random_string () in
       add_msg (`To (jid, id)) true (Escape.unescape m) ;
       id
  in
  let maybe_send ?kind jid session out user_out =
    let id = handle_otr_out jid user_out in
    Utils.option
      Lwt.return_unit
      (fun body -> send t session ?kind jid (Some id) body)
      out
  in
  let jid, session, out, user_out, kind =
    match active_user with
    | `Room _ -> (* XXX MUC should also be more careful, privmsg.. *)
       let jid = `Bare (Xjid.t_to_bare state.active_contact) in
       (jid, None, Some message, `Sent message, Some Xmpp_callbacks.XMPPClient.Groupchat)
    | `User u ->
       let bare = u.User.bare_jid in
       match session state with
       | None ->
          let ctx = Otr.State.new_session (otr_config u state) state.config.Xconfig.dsa () in
          let _, out, user_out = Otr.Engine.send_otr ctx message in
          (`Bare bare, None, out, user_out, None)
       | Some session ->
          let ctx = session.User.otr in
          let msg =
            if Otr.State.is_encrypted ctx then
              Escape.escape message
            else
              message
          in
          let ctx, out, user_out = Otr.Engine.send_otr ctx msg in
          let user = User.update_otr u session ctx in
          Contact.replace_user state.contacts user ;
          let jid, session =
            if session.User.resource = "" then
              (`Bare bare, None)
            else
              (`Full (bare, session.User.resource), Some session)
          in
          (jid, session, out, user_out, None)
  in
  maybe_send ?kind jid session out user_out

(*
let m_to_s ms =
  let rec to_s acc = function
    | [] -> String.concat " " acc
    | `Shift::xs -> to_s ("shift"::acc) xs
    | `Meta::xs -> to_s ("meta"::acc) xs
    | `Ctrl::xs -> to_s ("ctrl"::acc) xs
  in
  to_s [] ms

let k_to_s = function
  | `Key (`Uchar chr, ms) -> Printf.sprintf "uchar 0x%X:%s" (Uchar.to_int chr) (m_to_s ms)
  | `Key (`Escape, ms) -> "escape:" ^ m_to_s ms
  | `Key (`Enter, ms) -> "enter:" ^ m_to_s ms
  | `Key (`Tab, ms) -> "tab:" ^ m_to_s ms
  | `Key (`Backspace, ms) -> "backspace:" ^ m_to_s ms
  | `Key (`Insert, ms) -> "insert:" ^ m_to_s ms
  | `Key (`Delete, ms) -> "delete:" ^ m_to_s ms
  | `Key (`Home, ms) -> "home:" ^ m_to_s ms
  | `Key (`End, ms) -> "end:" ^ m_to_s ms
  | `Key (`Arrow `Up, ms) -> "arrow up:" ^ m_to_s ms
  | `Key (`Arrow `Down, ms) -> "arrow down:" ^ m_to_s ms
  | `Key (`Arrow `Left, ms) -> "arrow left:" ^ m_to_s ms
  | `Key (`Arrow `Right, ms) -> "arrow right:" ^ m_to_s ms
  | `Key (`Page `Up, ms) -> "page up:" ^ m_to_s ms
  | `Key (`Page `Down, ms) -> "page down:" ^ m_to_s ms
  | `Key (`Function x, ms) -> "function " ^ string_of_int x ^ ":" ^ m_to_s ms
  | _ -> "unknown"
*)

let read_terminal term mvar input_mvar () =
  let p = Lwt_mvar.put mvar
  and ok s = Lwt.return (`Ok s)
  in
  let rec loop () =
    Lwt_stream.next (Notty_lwt.Term.events term) >>= fun e ->
    match readline_input e with
    | `Ok f -> p (fun s -> ok (let input = f s.input in { s with input })) >>= fun () -> loop ()
    | `Unhandled k -> match emacs_bindings k with
      | `Ok f -> p (fun s ->
          let input, kill = f s.input s.kill in
          ok ({ s with input ; kill })) >>= fun () -> loop ()
      | `Unhandled k ->
        if not !reading then
          (* command and message processing *)
          match k with
          | `Key (`Enter, []) ->
            let handler s =
              let pre, post = s.input in
              let input = char_list_to_str (pre @ post) in
              let b = Contact.set_input_buffer (active s) ([], []) in
              Contact.replace_contact s.contacts b ;
              let s = { s with input = ([], []) } in
              Lwt_mvar.put input_mvar input >>= fun () ->
              ok s
            in
            p handler >>= fun () -> loop ()
          | _ -> loop () (* don't do this navigation when reading a password! *)

        else
          match k with
          | `Key (`Enter, []) ->
            let handler s =
              let pre, post = s.input in
              let input = char_list_to_str (pre @ post) in
              let clear_input b message =
                let b = Contact.add_readline_history b message in
                let b = Contact.set_input_buffer b ([], []) in
                let b = Contact.set_history_position b 0 in
                Contact.replace_contact s.contacts b ;
                b
              and self = Xjid.jid_matches (`Bare (fst s.config.Xconfig.jid)) s.active_contact
              and s = { s with input = ([], []) }
              in
              let err msg = add_status ~kind:`Error s (`Local ((`Full s.config.Xconfig.jid), "error")) msg in
              if String.length input = 0 then
                let active = active s in
                let exp = Contact.expanded active in
                ok (if exp || potentially_visible_resource s active then
                      (Contact.replace_contact s.contacts (Contact.expand active) ;
                       if exp then
                         { s with active_contact = `Bare (Contact.bare active) }
                       else
                         s)
                    else
                      s)
              else if
                String.get input 0 = '/'
                (* copy-paste: treat as text if line starts with "/*" or "// " *)
                && not String.(length input >= 2 && get input 1 = '*')
                && not String.(length input >= 3 && sub input 1 2 = "/ ")
                && not String.(length input >= 3 && sub input 1 2 = "me")
              then
                match String.trim input with
                | "/quit" -> Lwt.return (`Quit s)
                | cmd when String.length cmd > 4 && String.sub cmd 0 4 = "/raw" ->
                  (match !xmpp_session with
                   | None -> err "no active session, try to connect first" ; Lwt.return_unit
                   | Some t ->
                     let data, otr =
                       match Cli_commands.split_ws cmd with
                       | ("/rawenc", Some data) -> data, true
                       | ("/raw", Some data) -> data, false
                       | _ -> "", false
                     in
                     let user = clear_input (active s) input in
                     let msg =
                       try Some (Cstruct.to_string (Cstruct.of_hex data)) with _ -> None
                     in
                     let m, jid, kind = match user with
                       | `Room r -> msg, `Bare r.Muc.room_jid, Some Xmpp_callbacks.XMPPClient.Groupchat
                       | `User u ->
                         let bare = u.User.bare_jid in
                         match session s, msg with
                         | _, None -> None, `Bare bare, None
                         | None, _ -> (if otr then None else msg), `Bare bare, None
                         | Some se, Some txt ->
                           let jid = `Full (bare, se.User.resource) in
                           if otr && Otr.State.is_encrypted se.User.otr then
                             let ctx, out, _ = Otr.Engine.send_otr se.User.otr txt in
                             Contact.replace_user s.contacts (User.update_otr u se ctx) ;
                             (out, jid, None)
                           else
                             (Some txt, jid, None)
                     in
                     let log kind dir msg =
                       let u = active s in
                       let msg = User.message ~kind dir otr false msg in
                       let u = Contact.new_message u msg in
                       Contact.replace_contact s.contacts u
                     in
                     match m with
                     | None -> log `Warning (`Local (jid, "warning")) "didn't send anything" ; Lwt.return_unit
                     | Some m ->
                       log (match kind with None -> `Chat | _ -> `GroupChat) (`To (jid, "")) ("hex encoded: " ^ data) ;
                       send t None ?kind jid None m) >>= fun () ->
                  ok s
                | cmd ->
                  let realcmd =
                    match Cli_commands.completion s cmd with
                    | [x] -> cmd ^ x
                    | _ -> cmd
                  in
                  let active = clear_input (active s) cmd in
                  Cli_commands.exec realcmd s active self p
              else if self then
                (err "try `M-x doctor` in emacs instead" ;
                 ok s)
              else
                let active = clear_input (active s) input in
                (match !xmpp_session with
                 | None -> err "no active session, try to connect first" ; Lwt.return_unit
                 | Some t -> send_msg t s active input) >>= fun () ->
                ok s
            in
            p handler >>= fun () ->
            loop ()

          | `Key (`Tab, []) ->
            let handle s =
              let pre, post = s.input in
              let input = char_list_to_str pre in
              let pre =
                match Cli_commands.completion s input with
                | [] -> pre
                | [x] ->
                  if String.length input > 0 && String.get input 0 = '/' then
                    pre @ str_to_char_list (x ^ " ")
                  else
                    pre @ str_to_char_list (x ^ ": ")
                | x::xs ->
                  let shortest = List.fold_left (fun l x -> min l (String.length x)) (String.length x) xs in
                  let rec prefix idx =
                    if idx >= shortest then
                      idx
                    else
                      let c = String.get x idx in
                      if List.for_all (fun s -> String.get s idx = c) xs then
                        prefix (succ idx)
                      else
                        idx
                  in
                  pre @ str_to_char_list (String.sub x 0 (prefix 0))
              in
              ok ({ s with input = (pre, post) })
            in
            p handle >>= fun () -> loop ()

          | `Key (`Arrow `Up, []) -> p (fun s -> ok (history s Up)) >>= fun () -> loop ()
          | `Key (`Arrow `Down, []) -> p (fun s -> ok (history s Down)) >>= fun () -> loop ()

          | `Key (`ASCII 'D', [`Ctrl]) -> p (fun s -> Lwt.return (`Quit s))

          (* UI navigation and toggles *)
          | `Key (`Page `Up, []) -> p (fun s -> ok (navigate_buddy_list s Up)) >>= fun () -> loop ()
          | `Key (`Page `Down, []) -> p (fun s -> ok (navigate_buddy_list s Down)) >>= fun () -> loop ()

          | `Key (`ASCII 'C', [`Ctrl]) -> p (fun s -> ok (next_crypto_buddy s)) >>= fun () -> loop ()

          | `Key (`Page `Up, [`Ctrl]) -> p (fun s -> ok (navigate_message_buffer s Up)) >>= fun () -> loop ()
          | `Key (`ASCII 'P', [`Ctrl]) -> p (fun s -> ok (navigate_message_buffer s Up)) >>= fun () -> loop ()
          | `Key (`Page `Down, [`Ctrl]) -> p (fun s -> ok (navigate_message_buffer s Down)) >>= fun () -> loop ()
          | `Key (`ASCII 'N', [`Ctrl]) -> p (fun s -> ok (navigate_message_buffer s Down)) >>= fun () -> loop ()

          | `Key (`ASCII 'X', [`Ctrl]) -> p (fun s -> ok (activate_contact s s.last_active_contact)) >>= fun () -> loop ()
          | `Key (`ASCII 'Q', [`Ctrl]) ->
            let handle s =
              let s = match List.rev s.notifications with
                | x::_ -> activate_contact s x
                | _ -> s
              in
              ok s
            in
            p handle >>= fun () -> loop ()

          | `Key (`Function 5, []) -> p (fun s -> ok { s with show_offline = not s.show_offline }) >>= fun () -> loop ()
          | `Key (`Function 10, []) -> p (fun s -> ok { s with log_height = succ s.log_height }) >>= fun () -> loop ()
          | `Key (`Function 10, [`Shift]) -> p (fun s -> ok { s with log_height = max 0 (pred s.log_height) }) >>= fun () -> loop ()
          | `Key (`Function 10, [`Ctrl]) -> p (fun s -> ok { s with log_height = max 0 (pred s.log_height) }) >>= fun () -> loop ()
          | `Key (`Function 11, []) -> p (fun s -> ok { s with buddy_width = succ s.buddy_width }) >>= fun () -> loop ()
          | `Key (`Function 11, [`Shift]) -> p (fun s -> ok { s with buddy_width = max 0 (pred s.buddy_width) }) >>= fun () -> loop ()
          | `Key (`Function 11, [`Ctrl]) -> p (fun s -> ok { s with buddy_width = max 0 (pred s.buddy_width) }) >>= fun () -> loop ()
          | `Key (`Function 12, []) -> p (fun s -> ok { s with window_mode = next_display_mode s.window_mode }) >>= fun () -> loop ()

          | `Resize size -> p (fun _ -> Lwt.return (`Resize size)) >>= fun () -> loop ()

          | _ -> p ok >>= fun () -> loop ()

(*
          | k ->
            let k = k_to_s k in
            p (fun s -> add_status s (`Local (`Full s.config.Xconfig.jid, "key")) k ; ok s) >>= fun () ->
            loop () *)
  in
  loop ()
