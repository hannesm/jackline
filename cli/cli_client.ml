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
  let a = A.(a ++ buddy_to_color (Contact.color contact None)) in
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
    let up, down = (h / 2, (h + 1) / 2) in
    match focus - up >= 0, focus + down > l with
    | true, true -> l - h
    | true, false -> focus - up
    | false, _ -> 0
  in
  let to_draw = Utils.drop start buddies in
  let formatted = I.vcat (List.map (format_buddy state w) to_draw) in
  I.vlimit ~align:`Top h formatted

let spacer a uchar width left right =
  let fill =
    let len = width - I.(width left + width right) in
    if len <= 0 then I.empty else I.uchar a uchar len 1
  in
  I.hcat [ left ; fill ; right ]

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
      I.(string a " " <|> string A.(a ++ buddy_to_color col) data <|> string a " ─")
    | _ -> I.empty
  and presence_status =
    let tr p s =
      let status =
        Utils.option "" (fun x -> Utils.option x fst (Astring.String.cut ~sep:"\n" x) ^ " ") s
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
  spacer a 0x2015 width (I.hcat [ pre ; scroll ; jid ]) I.(otr <|> presence_status)

let status_line self mysession notify log a width =
  let a = A.(a ++ st bold) in
  let notify = if notify then I.string A.(a ++ st blink) "##" else I.string a "──"
  and jid =
    let data = User.userid self mysession
    and a' = if log then A.(st reverse) else a
    in
    I.(string a "< " <|> string a' data <|> string a " >")
  and status =
    let data = User.presence_to_string mysession.User.presence
    and color = if mysession.User.presence = `Offline then `Bad else `Good
    in
    I.(string a "[ " <|> string A.(buddy_to_color color ++ a) data <|> string a " ]─")
  in
  spacer a 0x2015 width I.(notify <|> jid) status

let cut_scroll scrollback height image =
  let bottom = scrollback * height in
  I.vlimit ~align:`Bottom height (I.vcrop 0 bottom image)

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

let render_state (width, height) state =
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
    (I.string A.empty "need more space", 1)
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

    let input, cursorc =
      (* XXX: needs a proper renderer with sideways scrolling *)
      let pre, post = state.input in
      let inp = Array.of_list pre
      and inp2 = Array.of_list post
      in

      let iinp = I.uchars A.empty inp
      and iinp2 = I.uchars A.empty inp2
      in
      let completion =
        match post with
        | [] ->
          let buf = Buffer.create (Array.length inp) in
          Array.iter (Uutf.Buffer.add_utf_8 buf) inp ;
          let input = Buffer.contents buf in
          ( match Cli_commands.completion input with
            | [] -> I.empty
            | [x] -> I.string A.(fg (gray 20)) x
            | xs -> I.string A.(fg (gray 20)) (String.concat "|" xs) )
        | _ -> I.empty
      in
      (I.(iinp <|> completion <|> iinp2), succ (I.width iinp))
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
        and vline = I.uchar a 0x2502 1 main_height
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
            I.vlimit ~align:`Bottom log_height l
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
    (I.(main <-> bottom), cursorc)

module T = Notty_lwt.Term

let quit state =
  Utils.option
    Lwt.return_unit
    (fun x ->
       let otr_sessions =
         Contact.fold
           (fun _ u acc ->
              match u with
              | `Room _ -> acc
              | `User u ->
                List.fold_left
                  (fun acc s ->
                     if User.(encrypted s.otr) then (u, s) :: acc
                     else acc)
                  acc
                  u.User.active_sessions)
           state.contacts []
       in
       let send_out (user, session) =
         match Otr.Engine.end_otr session.User.otr with
         | _, Some body ->
           let jid = `Full (user.User.bare_jid, session.User.resource) in
           send x (Some session) jid None body (fun _ -> Lwt.return_unit)
         | _ -> Lwt.return_unit
       in
       Lwt_list.iter_s send_out otr_sessions)
    !xmpp_session

(* this is rendering and drawing stuff to terminal, waiting for updates of the ui_mvar... *)
let rec loop term mvar state =
  let size = T.size term in
  let image, cursorc = render_state size state in
  T.image term image >>= fun () ->
  T.cursor term (Some (cursorc, snd size)) >>= fun () ->
  Lwt_mvar.take mvar >>= fun action ->
  action state >>= function
    (* XXX: should we treat disconnect here as well? someone needs to!
        --> distinguish `Failure (disconnected, should reconnect) and `Disconnect (/disconnect)
       (so far, make_prompt did this...
       could also do the reconnect dance (currently in several handlers (connect), and init_system)..
       and adjust xmpp_session) *)
  | `Ok state -> loop term mvar state
  | `Quit state ->
    quit state >>= fun () ->
    Lwt.return state

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
        xmpp_session := None ; err false (Printexc.to_string exn) (* happens on /disconnect *)
      | Unix.Unix_error (Unix.EINVAL, "select", _ ) as exn ->
        xmpp_session := None ; err true (Printexc.to_string exn) (* not sure whether true should be false *)
      | exn -> err true (Printexc.to_string exn)
  )


type direction = Up | Down

let navigate_message_buffer state direction =
  match direction, state.scrollback with
  | Down, 0 -> state
  | Down, n ->
    let s = { state with scrollback = pred n } in
    if s.scrollback = 0 then notified s else s
  | Up, n -> { state with scrollback = succ n }

let history state dir =
  let active = active state in
  let hitems = "" :: Contact.readline_history active
  and cursor = Contact.history_position active
  in
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
  and active_idx = Utils.find_index state.active_contact 0 userlist
  in
  let l = List.length userlist in
  match direction with
  | Down -> set_active (succ active_idx mod l)
  | Up -> set_active ((l + pred active_idx) mod l)

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
  let maybe_send ?kind jid session out user_out =
    let id = handle_otr_out jid user_out in
    Utils.option
      Lwt.return_unit
      (fun body -> send t session ?kind jid (Some id) body failure)
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
          (`Full (bare, session.User.resource), Some session, out, user_out, None)
  in
  maybe_send ?kind jid session out user_out

let m_to_s ms =
  let rec to_s acc = function
    | [] -> String.concat " " acc
    | `Shift::xs -> to_s ("shift"::acc) xs
    | `Meta::xs -> to_s ("meta"::acc) xs
    | `Ctrl::xs -> to_s ("ctrl"::acc) xs
  in
  to_s [] ms

let k_to_s = function
  | `Key (`Uchar chr, ms) -> Printf.sprintf "uchar 0x%X:%s" chr (m_to_s ms)
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

let read_terminal term mvar () =
  (* XXX: emacs key bindings: C-left/right [word forward/backward] C- wy[mark, kill, yank] C-_-[undo/redo] *)
  let p = Lwt_mvar.put mvar
  and ok s = Lwt.return (`Ok s)
  in
  let rec loop () =
    Lwt_stream.next (T.events term) >>= fun e ->
    match readline_input e with
    | `Ok f -> p (fun s -> ok (let input = f s.input in { s with input })) >>= fun () -> loop ()
    | `Unhandled k -> match emacs_bindings k with
      | `Ok f -> p (fun s -> ok (let input = f s.input in { s with input })) >>= fun () -> loop ()
      | `Unhandled k -> match k with
        (* command and message processing *)
        | `Key (`Enter, []) ->
          let handler s =
            let pre, post = s.input in
            let inp = Array.of_list pre
            and inp2 = Array.of_list post
            in
            let buf = Buffer.create (Array.length inp + Array.length inp2) in
            Array.iter (Uutf.Buffer.add_utf_8 buf) inp ;
            Array.iter (Uutf.Buffer.add_utf_8 buf) inp2 ;
            let clear_input b message =
              let b = Contact.add_readline_history b message in
              let b = Contact.set_input_buffer b ([], []) in
              Contact.replace_contact s.contacts b ;
              b
            and self = Xjid.jid_matches (`Bare (fst s.config.Xconfig.jid)) s.active_contact
            and s = { s with input = ([], []) }
            in
            let err msg = add_status s (`Local ((`Full s.config.Xconfig.jid), "error")) msg
            and failure reason =
              Connect.disconnect () >>= fun () ->
              add_status s (`Local (s.active_contact, "session error")) (Printexc.to_string reason) ;
              ignore (Lwt_engine.on_timer 10. false (fun _ -> Lwt.async (fun () ->
                  Lwt_mvar.put s.connect_mvar Reconnect))) ;
              Lwt.return_unit (* XXX: disconnected? *)
            in
            match Buffer.contents buf with
            | "/quit" -> Lwt.return (`Quit s)
            | "" -> (* XXX: expand/unexpand:
                       if Contact.expanded active || potentially_visible_resource state active then
                       (Contact.replace_contact state.contacts (Contact.expand active) ;
                       if Contact.expanded active then
                       (state.active_contact <- `Bare (Contact.bare active))) ;
                    *)
              ok s
            | cmd when String.get cmd 0 = '/' ->
              let active = clear_input (active s) cmd in
              Cli_commands.exec cmd s term active self failure p
            | _ when self ->
              err "try `M-x doctor` in emacs instead" ;
              ok s
            | message ->
              let active = clear_input (active s) message in
              (match !xmpp_session with
               | None -> err "no active session, try to connect first" ; Lwt.return_unit
               | Some t -> send_msg t s active failure message) >>= fun () ->
              ok s
          in
          p handler >>= fun () ->
          loop ()
        (* XXX: elsewhere: if List.length state.notifications = 0 then Lwt.async (fun () -> Lwt_mvar.put state.state_mvar Clear) *)

        | `Key (`Tab, []) ->
          let handle s =
            let pre, post = s.input in
            let input =
              let inp = Array.of_list pre in
              let buf = Buffer.create (Array.length inp) in
              Array.iter (Uutf.Buffer.add_utf_8 buf) inp ;
              Buffer.contents buf
            in
            let pre =
              match Cli_commands.completion input with
              | [] -> pre
              | [x] -> pre @ str_to_char_list x
              | _ -> pre
            in
            ok ({ s with input = (pre, post) })
          in
          p handle >>= fun () -> loop ()

        | `Key (`Arrow `Up, []) -> p (fun s -> ok (history s Up)) >>= fun () -> loop ()
        | `Key (`Arrow `Down, []) -> p (fun s -> ok (history s Down)) >>= fun () -> loop ()

        | `Key (`Uchar 0x44, [`Ctrl]) (* C-d *) -> p (fun s -> Lwt.return (`Quit s))

        (* UI navigation and toggles *)
        | `Key (`Page `Up, []) -> p (fun s -> ok (navigate_buddy_list s Up)) >>= fun () -> loop ()
        | `Key (`Page `Down, []) -> p (fun s -> ok (navigate_buddy_list s Down)) >>= fun () -> loop ()

        | `Key (`Page `Up, [`Ctrl]) -> p (fun s -> ok (navigate_message_buffer s Up)) >>= fun () -> loop ()
        | `Key (`Page `Down, [`Ctrl]) -> p (fun s -> ok (navigate_message_buffer s Down)) >>= fun () -> loop ()

        | `Key (`Uchar 0x58, [`Ctrl]) (* C-x *) -> p (fun s -> ok (activate_contact s s.last_active_contact)) >>= fun () -> loop ()
        | `Key (`Uchar 0x71, [`Ctrl]) (* C-q *) ->
          p (fun s -> ok (match List.rev s.notifications with
              | x::_ -> activate_contact s x
              | _ -> s)) >>= fun () ->
          loop ()

        | `Key (`Function 5, []) -> p (fun s -> ok { s with show_offline = not s.show_offline }) >>= fun () -> loop ()
        | `Key (`Function 10, []) -> p (fun s -> ok { s with log_height = succ s.log_height }) >>= fun () -> loop ()
        | `Key (`Function 10, [`Shift]) -> p (fun s -> ok { s with log_height = max 0 (pred s.log_height) }) >>= fun () -> loop ()
        | `Key (`Function 11, []) -> p (fun s -> ok { s with buddy_width = succ s.buddy_width }) >>= fun () -> loop ()
        | `Key (`Function 11, [`Shift]) -> p (fun s -> ok { s with buddy_width = max 0 (pred s.buddy_width) }) >>= fun () -> loop ()
        | `Key (`Function 12, []) -> p (fun s -> ok { s with window_mode = next_display_mode s.window_mode }) >>= fun () -> loop ()

        | `Resize _ -> p (fun s -> ok s) >>= fun () -> loop ()

        | k ->
          let k = k_to_s k in
          p (fun s -> add_status s (`Local (`Full s.config.Xconfig.jid, "key")) k ; ok s) >>= fun () ->
          loop ()
  in
  loop ()
