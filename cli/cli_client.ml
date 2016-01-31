open Lwt.Infix

open Notty

open Cli_state
open Cli_support

let print_time ~now ~tz_offset_s timestamp =
  let daydiff, _ = Ptime.Span.to_d_ps (Ptime.diff now timestamp) in
  let (_, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ~tz_offset_s timestamp in
  if daydiff = 0 then (* less than a day ago *)
    Printf.sprintf "%02d:%02d:%02d " hh mm ss
  else
    Printf.sprintf "%02d-%02d %02d:%02d " m d hh mm

let format_log tz_offset_s now log =
  let { User.direction ; timestamp ; message ; _ } = log in
  let time = print_time ~now ~tz_offset_s timestamp in
  let from = match direction with
    | `From jid -> Xjid.jid_to_string jid ^ ":"
    | `Local (_, x) when x = "" -> "***"
    | `Local (_, x) -> "*** " ^ x ^ " ***"
    | `To _ -> ">>>"
  in
  I.string A.empty (time ^ from ^ " " ^ message)

let render_wrapped_list width fmt entries =
  let formatted = List.map fmt entries in
  I.vcat (List.map (wrap ~width) formatted)

let format_message tz_offset_s now buddy resource { User.direction ; encrypted ; received ; timestamp ; message ; _ } =
  let time = print_time ~now ~tz_offset_s timestamp
  and style, pre =
    match buddy with
    | `Room _ ->
      ( match direction with
        | `From (`Full (_, nick)) -> (`Highlight, nick ^ ": ")
        | `From (`Bare _) -> (`Highlight, " ")
        | `Local (_, x) -> (`Default, "***" ^ x ^ " ")
        | `To _ -> (`Default, if received then "-> " else "?> ") )
    | `User _ ->
      let en = if encrypted then "O" else "-" in
      let style, pre = match direction with
        | `From _ -> (`Highlight, "<" ^ en ^ "- ")
        | `To _   ->
          let f = if received then "-" else "?" in
          (`Default, f ^ en ^ "> ")
        | `Local (_, x) when x = "" -> (`Default, "*** ")
        | `Local (_, x) -> (`Default, "***" ^ x ^ "*** ")
      in
      let r =
        let show_res =
          let other = User.jid_of_direction direction in
          let other_resource s = match Xjid.resource other with
            | None -> None
            | Some x when x = s.User.resource -> None
            | Some x -> Some x
          in
          match resource with
          | Some (`Session s) -> other_resource s
          | _ -> Xjid.resource other
        in
        Utils.option "" (fun x -> "(" ^ x ^ ") ") show_res
      in
      (style, r ^ pre)
  and to_style = function
    | `Default -> A.empty
    | `Highlight -> A.(st bold)
  in
  I.string (to_style style) (time ^ pre ^ message)

let buddy_to_color = function
  | `Default -> A.empty
  | `Good -> A.(fg green)
  | `Bad -> A.(fg red)

let format_buddy state width contact =
  (* XXX: pass resource *)
  let jid = Contact.jid contact None in
  let a =
    if isactive state jid then
      A.(st reverse)
    else if isnotified state jid then
      A.(st blink)
    else
      A.empty
  in
  let a = A.(buddy_to_color (Contact.color contact None) & a) in
  let first_char = if isnotified state jid then "*" else " " in
  let buddy = I.string a (first_char ^ Contact.oneline contact None) in
  let len = width - I.width buddy in
  if len > 0 then
    I.(buddy <|> char a ' ' len 1)
  else
    I.hpad 0 len buddy

let render_buddy_list (w, h) state =
  (* XXX: actually a treeview, resources and whether to expand contact / potential children *)
  let buddies = active_contacts state in
  let start =
    let focus =
      let jids = List.map (fun c -> Contact.jid c None) buddies in
      Utils.find_index state.active_contact 0 jids
    in
    let l = List.length buddies in
    assert (focus >= 0 && focus < l) ;
    let up, down = (h / 2, (h + 1) / 2) in
    match focus - up >= 0, focus + down > l with
    | true, true -> l - h
    | true, false -> focus - up
    | false, _ -> 0
  in
  let to_draw = Utils.drop start buddies in
  let formatted = I.vcat (List.map (format_buddy state w) to_draw) in
  I.vframe ~align:`Top h formatted

let horizontal_line buddy resource a scrollback width =
  let pre = I.string a "── "
  and scroll = if scrollback = 0 then I.empty else I.string a ("*scrolling " ^ string_of_int scrollback ^ "* ")
  and jid =
    let p = match buddy with
      | `User _ -> "buddy: "
      | `Room _ -> "room: "
    in
    let id = Contact.jid buddy resource in
    I.string a (p ^ Xjid.jid_to_string id ^ " ")
  and otr =
    match buddy, resource with
    | `User user, Some (`Session s) ->
      let col, data =
        Utils.option
          (`Bad, "no OTR")
          (fun fp ->
           let vs = User.verified_fp user fp in
           (User.verification_status_to_color vs, User.verification_status_to_string vs))
          (User.otr_fingerprint s.User.otr)
      in
      I.(string a " " <|> string A.(a & buddy_to_color col) data <|> string a " ─")
    | _ -> I.empty
  and presence_status =
    let tr p s =
      let status =
        Utils.option
          ""
          (fun x -> Utils.option
              (x ^ " ")
              (fun (a, _) -> a ^ " ")
              (Astring.String.cut ~sep:"\n" x))
          s
      in
      I.string a (" " ^ User.presence_to_string p ^ " " ^ status ^ "─")
    in
    Utils.option
      I.empty
      (function
        | `Session s -> tr s.User.presence s.User.status
        | `Member m -> tr m.Muc.presence m.Muc.status)
      resource
  in
  let fill =
    let len = width - I.(width pre + width scroll + width jid + width otr + width presence_status) in
    if len <= 0 then I.empty else I.uchar a (`Uchar 0x2015) len 1
  in
  I.hcat [ pre ; scroll ; jid ; fill ; otr ; presence_status ]

let status_line self mysession notify log a width =
  let a = A.(a & st bold) in
  let notify = if notify then I.string A.(a & st blink) "##" else I.string a "──"
  and jid =
    let data = User.userid self mysession
    and a' = if log then A.(st reverse) else a
    in
    I.(string a "< " <|> string a' data <|> string a " >")
  and status =
    let data = User.presence_to_string mysession.User.presence
    and color = if mysession.User.presence = `Offline then `Bad else `Good
    in
    I.(string a "[ " <|> string A.(a & buddy_to_color color) data <|> string a " ]─")
  in
  let fill =
    let len = width - I.(width jid + width status + width notify) in
    if len <= 0 then I.empty else I.uchar a (`Uchar 0x2015) len 1
  in
  I.(notify <|> jid <|> fill <|> status)

let cut_scroll scrollback height image =
  let bottom = scrollback * height in
  I.vframe ~align:`Bottom height (I.vcrop 0 bottom image)

let render_messages width p msgfmt data =
  let data = List.filter p data in
  render_wrapped_list width msgfmt data

let msgfilter active jid m =
  let o = User.jid_of_direction m.User.direction in
  if Contact.expanded active then
    match active, jid with
    | `Room _, _ -> true
    | `User _, `Bare _ -> true
    | `User _, `Full _ -> Xjid.jid_matches o jid
  else
    true

let tz_offset_s () =
  match Ptime_clock.current_tz_offset_s () with
  | None -> 0 (* XXX: report error *)
  | Some x -> x

let render_state (width, height) input state =
  let log_height, main_height =
    let s = state.log_height in
    if s + 10 > height then
      (0, height - 2)
    else
      (s, height - s - 3)
  and buddy_width, chat_width =
    let b = state.buddy_width in
    match state.window_mode with
    | BuddyList -> (b, width - b - 1)
    | FullScreen | Raw -> (0, width)
  in

  if main_height <= 4 || chat_width <= 20 then
    I.string A.empty "need more space"
  else
    let active = active state
    and resource = resource state
    in

    let now = Ptime_clock.now ()
    and tz_offset_s = tz_offset_s ()
    in

    let logfmt = format_log tz_offset_s now
    and a = buddy_to_color (Contact.color active resource)
    in

    let main =
      let msgfmt = format_message tz_offset_s now active resource
      and msgfilter = msgfilter active state.active_contact
      and msgs msgfilter msgfmt =
        let r = match active with
          | `User x when x.User.self -> (fun x -> render_wrapped_list x logfmt)
          | _ -> (fun x -> render_messages x msgfilter msgfmt)
        in
        let image = r chat_width (List.rev (Contact.messages active)) in
        cut_scroll state.scrollback main_height image
      in
      match state.window_mode with
      | BuddyList ->
        let buddies = render_buddy_list (buddy_width, main_height) state
        and vline = I.uchar a (`Uchar 0x2502) 1 main_height
        in
        I.(buddies <|> vline <|> msgs msgfilter msgfmt)
      | FullScreen -> msgs msgfilter msgfmt
      | Raw ->
        let p m = match m.User.direction with `From _ -> true | _ -> false
        and msgfmt x = I.string A.empty x.User.message
        in
        msgs p msgfmt
    and bottom =
      let self = self state in
      let hline_log =
        if log_height = 0 then
          I.empty
        else
          let logs =
            let l = render_wrapped_list width logfmt (List.rev self.User.message_history) in
            I.vframe ~align:`Bottom log_height l
          and hline = horizontal_line active resource a state.scrollback width
          in
          I.(hline <-> logs)
      and status =
        let notify = List.length state.notifications > 0
        and log = Contact.preserve_messages active
        and mysession = selfsession state
        in
        status_line self mysession notify log a width
      in
      I.vcat [ hline_log ; status ; input ]
    in
    I.(main <-> bottom)

let quit state =
  match !xmpp_session with
  | None -> Lwt.return_unit
  | Some x ->
     let otr_sessions =
       Contact.fold
         (fun _ u acc ->
          match u with
          | `Room _ -> acc
          | `User u ->
             List.fold_left
               (fun acc s ->
                if User.(encrypted s.otr) then
                  (u, s) :: acc
                else acc)
               acc
               u.User.active_sessions)
         state.contacts []
     in
     let send_out (user, session) =
       match Otr.Engine.end_otr session.User.otr with
       | _, Some body ->
          let jid = `Full (user.User.bare_jid, session.User.resource) in
          send x jid None body (fun _ -> Lwt.return_unit)
       | _ -> Lwt.return_unit
     in
     Lwt_list.iter_s send_out otr_sessions

(*
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

let send_msg t state active_user failure message =
  let handle_otr_out jid user_out =
    let add_msg direction encrypted data =
      let msg = User.message direction encrypted false data in
      let u = active state in
      let u = Contact.new_message u msg in
      Contact.replace_contact state.contacts u
    in
    (match active state with
     | `User u -> warn jid u add_msg
     | `Room _ -> ()) ;
    match user_out with
    | `Warning msg      ->
       add_msg (`Local (jid, "OTR Warning")) false msg ;
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
  let maybe_send ?kind jid out user_out =
    Utils.option
      Lwt.return_unit
      (fun body -> send t ?kind jid (Some (handle_otr_out jid user_out)) body failure)
      out
  in
  let jid, out, user_out, kind =
    match active_user with
    | `Room _ -> (* XXX MUC should also be more careful, privmsg.. *)
       let jid = `Bare (Xjid.t_to_bare state.active_contact) in
       (jid, Some message, `Sent message, Some Xmpp_callbacks.XMPPClient.Groupchat)
    | `User u ->
       let bare = u.User.bare_jid in
       match session state with
       | None ->
          let ctx = Otr.State.new_session (otr_config u state) state.config.Xconfig.dsa () in
          let _, out, user_out = Otr.Engine.send_otr ctx message in
          (`Bare bare, out, user_out, None)
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
          (`Full (bare, session.User.resource), out, user_out, None)
  in
  maybe_send ?kind jid out user_out
*)

(* main thingy: *)
(* draw ; read mvar ; process : might do network output, modify user hash table (state -> state lwt.t) ; goto 10 *)

(* terminal reader *)
(*  waits for terminal input, processes it [commands, special keys, messages], puts result into mvar *)

(* sigwinch -- rerender *)

(* stream reader *)
(*  processes xml stream fragments, puts resulting action [message received, buddy list updates] into mvar *)

(* disconnect and quit -- exceptions!? --> for now deliver to mvar as well *)

(* XXX: attention: state updates for disconnect have been removed for now (were in make_prompt) *)

(* XXX: remove mutable (and hashtbl) from cli_state, rewrite code to conform *)

module T = Notty_lwt.Terminal

(* this is rendering and drawing stuff, waiting for change on mvar... *)
let rec loop term mvar state =
  (*  let history = Contact.readline_history (active state) in (* for keyup.down *) *)
  let input, cursorc =
    (* XXX: needs a proper renderer with sideways scrolling, should maybe move into render_state!? *)
    let pre, post = state.input in
    let inp = Array.of_list pre
    and inp2 = Array.of_list post
    in

    let iinp = I.uchars A.empty inp
    and iinp2 = I.uchars A.empty inp2
    in
    (I.(iinp <|> iinp2), succ (I.width iinp))
  in
  (* render things *)
  let size = T.size term in
  let image = render_state size input state in
  T.image term image >>= fun () ->
  T.cursor term (Some (cursorc, snd size)) >>= fun () ->
  Lwt_mvar.take mvar >>= fun action ->
  action state >>= function
    (* XXX: should we treat disconnect here as well? someone needs to! (so far, make_prompt did this... could also do the reconnect dance.. and adjust xmpp_session) *)
  | `Ok state -> loop term mvar state
  | `Quit state -> Lwt.return state

let init_system ui_mvar connect_mvar =
  let err r m =
    Lwt.async (fun () ->
        Connect.disconnect () >>= fun () ->
        selflog ui_mvar "async error" m >|= fun () ->
        if r then
          ignore (Lwt_engine.on_timer 10. false (fun _ ->
              Lwt.async (fun () -> Lwt_mvar.put connect_mvar Reconnect))))
  in
  Lwt.async_exception_hook := (function
      | Tls_lwt.Tls_failure `Error (`AuthenticationFailure _) as exn ->
         err false (Printexc.to_string exn)
      | Unix.Unix_error (Unix.EBADF, _, _ ) as exn ->
         xmpp_session := None ; err true (Printexc.to_string exn)
      | Unix.Unix_error (Unix.EINVAL, "select", _ ) as exn ->
         xmpp_session := None ; err true (Printexc.to_string exn)
      | exn -> err true (Printexc.to_string exn)
  )


let rec winch term mvar () =
  T.next_resize term >>= fun _ ->
  Lwt_mvar.put mvar (fun s -> Lwt.return (`Ok s)) >>= fun () ->
  winch term mvar ()

type direction = Up | Down

let navigate_message_buffer state direction =
  match direction, state.scrollback with
  | Down, 0 -> state
  | Down, n ->
    let s = { state with scrollback = pred n } in
    if s.scrollback = 0 then notified s else s
  | Up, n -> { state with scrollback = succ n }

let navigate_buddy_list state direction =
  let userlist = all_jids state in
  let set_active idx =
    let user = List.nth userlist idx in
    activate_contact state user
  and active_idx = Utils.find_index state.active_contact 0 userlist
  in
  let l = List.length userlist in
  match direction with
  | Down -> set_active (succ active_idx mod l)
  | Up -> set_active ((l + pred active_idx) mod l)

let read_terminal term mvar () =
  (* XXX: notifications C-q / C-x *)
  (* XXX: emacs key bindings: C-ae C-ku C-fb C-left/right [word forward/backward] C- wy[mark, kill, yank] C-_-[undo/redo] *)
  (* XXX: handle history (up/down) *)
  (* XXX: handle tab completion -- maybe suggestions in grey as well *)
  let p = Lwt_mvar.put mvar
  and input s f =
    let pre, post = s.input in
    let input = f pre post in
    { s with input }
  and ok s = Lwt.return (`Ok s)
  in
  let rec loop () =
    Lwt_stream.next (T.input term) >>= function
    | `Key (`Uchar chr, []) ->
      p (fun s -> ok (input s (fun pre post -> pre @ [chr], post))) >>= fun () ->
      loop ()

    (* navigation / readline stuff *)
    | `Key (`Backspace, []) ->
      let f pre post = match List.rev pre with
        | [] -> (pre, post)
        | _::tl -> (List.rev tl, post)
      in
      p (fun s -> ok (input s f)) >>= fun () ->
      loop ()
    | `Key (`Delete, []) ->
      let f pre post = match post with
        | [] -> (pre, post)
        | _::tl -> (pre, tl)
      in
      p (fun s -> ok (input s f)) >>= fun () ->
      loop ()
    | `Key (`Home, []) ->
      p (fun s -> ok (input s (fun pre post -> [], pre @ post))) >>= fun () ->
      loop ()
    | `Key (`End, []) ->
      p (fun s -> ok (input s (fun pre post -> pre @ post, []))) >>= fun () ->
      loop ()
    | `Key (`Right, []) ->
      let f pre post = match post with
        | [] -> (pre, post)
        | hd::tl -> (pre @ [hd], tl)
      in
      p (fun s -> ok (input s f)) >>= fun () ->
      loop ()
    | `Key (`Left, []) ->
      let f pre post = match List.rev pre with
        | [] -> ([], post)
        | hd::tl -> (List.rev tl, hd :: post)
      in
      p (fun s -> ok (input s f)) >>= fun () ->
      loop ()

    | `Key (`Enter, []) ->
      let handler s =
        let pre, post = s.input in
        let inp = Array.of_list pre
        and inp2 = Array.of_list post
        in
        let buf = Buffer.create (Array.length inp + Array.length inp2) in
        Array.iter (Uutf.Buffer.add_utf_8 buf) inp ;
        Array.iter (Uutf.Buffer.add_utf_8 buf) inp2 ;
        match Buffer.contents buf with
        | "/quit" -> quit s >|= fun () -> `Quit s
        | x -> add_status s (`Local (`Full s.config.Xconfig.jid, "XXX")) x ; ok s
      in
      p handler >>= fun () ->
      loop ()
(*
XXX: elsewhere: if List.length state.notifications = 0 then Lwt.async (fun () -> Lwt_mvar.put state.state_mvar Clear)

    | Some message ->
       let active =
         let b = active state in
         let b = Contact.add_readline_history b message in
         let b = Contact.set_saved_input_buffer b "" in
         Contact.replace_contact state.contacts b ;
         b
       in
       let failure reason =
         Connect.disconnect () >>= fun () ->
         log (`Local (state.active_contact, "session error"), Printexc.to_string reason) ;
         ignore (Lwt_engine.on_timer 10. false (fun _ -> Lwt.async (fun () ->
                   Lwt_mvar.put state.connect_mvar Reconnect))) ;
         Lwt.return_unit
       and self = Xjid.jid_matches (`Bare (fst state.config.Xconfig.jid)) state.active_contact
       and err data = log (`Local (state.active_contact, "error"), data) ; return_unit
       in
       let fst =
         if String.length message = 0 then
           None
         else
           Some (String.get message 0)
       in
       match String.length message, fst with
       | 0, _ ->
          if Contact.expanded active || potentially_visible_resource state active then
            (Contact.replace_contact state.contacts (Contact.expand active) ;
             if Contact.expanded active then
               (state.active_contact <- `Bare (Contact.bare active))) ;
       | _, Some '/' ->
          if String.trim message = "/quit" then
            quit state >|= fun () -> state
          else
            Cli_commands.exec message state term active session self failure log force_redraw >>= fun () ->
       | _, _ when self ->
          err "try `M-x doctor` in emacs instead" >>= fun () ->
       | _, _ ->
          (match !xmpp_session with
           | None -> err "no active session, try to connect first"
           | Some t -> send_msg t state active failure message) >>= fun () ->
*)

    (* UI navigation and toggles *)
    | `Key (`Pg_up, []) ->
      (* XXX: preserve input buffer for current user *)
      p (fun s -> ok (navigate_buddy_list s Up)) >>= fun () ->
      loop ()
    | `Key (`Pg_dn, []) ->
      (* XXX: preserve input buffer for current user *)
      p (fun s -> ok (navigate_buddy_list s Down)) >>= fun () ->
      loop ()

    | `Key (`Pg_up, [`Ctrl]) ->
      p (fun s -> ok (navigate_message_buffer s Up)) >>= fun () ->
      loop ()
    | `Key (`Pg_dn, [`Ctrl]) ->
      p (fun s -> ok (navigate_message_buffer s Down)) >>= fun () ->
      loop ()

    | `Key ((`Fn 5), []) ->
      p (fun s -> ok { s with show_offline = not s.show_offline }) >>= fun () ->
      loop ()
    | `Key ((`Fn 10), []) ->
      p (fun s -> ok { s with log_height = succ s.log_height }) >>= fun () ->
      loop ()
(*    | `Key `Shift (`Fn 10) ->
      p (fun s -> ok { s with log_height = max 0 (pred s.log_height) }) >>= fun () ->
      loop () *)
    | `Key ((`Fn 11), []) ->
      p (fun s -> ok { s with buddy_width = succ s.buddy_width }) >>= fun () ->
      loop ()
(*    | `Key `Shift (`Fn 11) ->
      p (fun s -> ok { s with buddy_width = max 0 (pred s.buddy_width) }) >>= fun () ->
      loop () *)
    | `Key ((`Fn 12), []) ->
      p (fun s -> ok { s with window_mode = next_display_mode s.window_mode }) >>= fun () ->
      loop ()

    | _ -> loop ()
  in
  loop ()
