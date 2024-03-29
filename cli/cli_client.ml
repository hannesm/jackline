open Lwt.Infix

open Notty

open Cli_state
open Cli_support

let print_time ~now ~tz_offset_s timestamp =
  let day_of_time t = match fst (Ptime.to_date_time ~tz_offset_s t) with
    | (_, _, d) -> d
  in
  let daydiff = day_of_time now - day_of_time timestamp in
  let (_, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ~tz_offset_s timestamp in
  if daydiff = 0 then (* same calendar day as now *)
    Printf.sprintf "%02d:%02d:%02d " hh mm ss
  else
    Printf.sprintf "%02d-%02d %02d:%02d " m d hh mm

let format_log tz_offset_s now log =
  let { User.direction ; timestamp ; message ; kind ; _ } = log in
  let time = print_time ~now ~tz_offset_s timestamp in
  let from = match direction with
    | `From jid -> Xjid.jid_to_string jid ^ ":"
    | `Local (_, x) when x = "" -> "*"
    | `Local (_, x) -> "* " ^ x ^ " *"
    | `To _ -> ">>>"
  in
  (Cli_colour.kind kind, time ^ from ^ " " ^ message)

let format_message tz_offset_s now self buddy resource { User.direction ; encrypted ; received ; timestamp ; message ; kind ; _ } =
  let time = print_time ~now ~tz_offset_s timestamp
  and style, pre =
    match buddy with
    | `Room r ->
      ( match direction with
        | `From (`Full (_, nick)) ->
          let tag =
            if Astring.String.is_infix ~affix:r.Muc.my_nick message then
              `Underline
            else
              `Highlight
          in
          (tag, nick ^ ": ")
        | `From (`Bare _) -> (`Highlight, " ")
        | `Local (_, x) -> (`Default, "*" ^ x ^ " ")
        | `To _ when received -> (`Default, "-> ")
        | `To _ -> (`Transit, "?> "))
    | `User _ ->
      let en = if encrypted then "O" else "-" in
      let style, pre = match direction with
        | `From _ -> (`Highlight, "<" ^ en ^ "- ")
        | `To _ when received -> (`Default, "-" ^ en ^ "> ")
        | `To _ -> (`Transit, "?" ^ en ^ "> ")
        | `Local (_, x) when x = "" -> (`Default, "* ")
        | `Local (_, x) -> (`Default, "*" ^ x ^ "* ")
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
  and to_style st =
    match st, kind with
    | `Transit, `Chat | `Transit, `GroupChat -> Cli_colour.kind `Transit
    | `Transit, x | `Default, x -> Cli_colour.kind x
    | `Highlight, `Chat | `Highlight, `GroupChat -> A.(st bold)
    | `Highlight, x -> Cli_colour.kind x
    | `Underline, `Chat | `Underline, `GroupChat -> A.(st underline)
    | `Underline, x -> A.(st underline ++ Cli_colour.kind x)
  in
  let p, msg =
    if String.length message >= 3 && String.sub message 0 3 = "/me" then
      let n = match buddy with
        | `User _ -> fst (match direction with
            | `From jid -> Xjid.t_to_bare jid
            | `To _ -> self.User.bare_jid
            | `Local (jid, _) -> Xjid.t_to_bare jid)
        | `Room r -> (match direction with
            | `From (`Full (_, r)) -> r
            | `From (`Bare (u, _)) -> u
            | `To _ -> r.Muc.my_nick
            | _ -> "local")
      in
      ("*" ^ n ^ "*", String.sub message 3 (String.length message - 3))
    else
      ("", message)
  in
  let a = to_style style in
  (a, time ^ pre ^ p ^ msg)

let buddy_to_color = function
  | `Default -> A.empty
  | `Good -> Cli_colour.kind `Success
  | `Bad -> Cli_colour.kind `Error

let format_buddy state width s contact resource =
  let jid = Contact.jid contact resource in
  let a =
    if isactive state jid then
      A.(st reverse)
    else if has_notifications state jid then
      A.(st blink)
    else
      A.empty
  in
  let a = A.(a ++ buddy_to_color (Contact.color contact resource)) in
  let first =
    match has_notifications state jid, Contact.expanded contact with
    | true, true -> I.char a '*' 1 1
    | false, false -> I.char a (if potentially_visible_resource state contact then '+' else ' ') 1 1
    | true, false -> Chars.star a 1
    | false, true -> I.char a ' ' 1 1
  and data = if s then Contact.oneline contact None else Contact.oneline contact resource
  in
  let buddy = I.(first <|> string a data) in
  v_space (I.char a ' ' 1 1) width buddy I.empty

let format_buddies state w buddies =
  (* where buddies is (contact * resource list) list *)
  List.fold_right
    (fun (c, res) acc ->
       let r = if Contact.expanded c then None else Contact.active c
       and res = List.map (fun x -> Some x) res
       in
       format_buddy state w true c r :: List.map (format_buddy state w false c) res @ acc)
    buddies []

let render_buddy_list (w, h) state =
  let buddies = active_contacts_resources state in
  let flattened = show_resources buddies in
  let start =
    let l = List.length flattened in
    if h >= l then
      0
    else
      let focus = Utils.find_index Xjid.jid_matches state.active_contact 0 flattened in
      let up, down = (h / 2, (h + 1) / 2) in
      match focus - up >= 0, focus + down > l with
      | true, true -> l - h
      | true, false -> focus - up
      | false, _ -> 0
  in

  (* XXX: could be smarter and not format all the buddies, but only those in view *)
  let formatted_buddies = format_buddies state w buddies in

  let to_render =
    let fst = Utils.drop start formatted_buddies in
    Utils.take h fst
  in
  let formatted = I.vcat to_render in
  I.vsnap ~align:`Top h formatted

let horizontal_line buddy resource a scrollback filter width =
  let pre = I.(Chars.hdash a 2 <|> I.char a ' ' 1 1)
  and scroll = if scrollback = 0 then I.empty else I.string a ("*scrolling " ^ string_of_int scrollback ^ "* ")
  and filter = match filter with None -> I.empty | Some x -> I.string a ("*filter: " ^ x ^ "* ")
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
      I.(string a " " <|> string A.(a ++ buddy_to_color col) (data ^ " ") <|> Chars.hdash a 1)
    | _ -> I.empty
  and presence_status =
    let tr p s =
      let status =
        Utils.option
          I.empty
          (fun x ->
             match split_on_nl a x with
             | [] -> I.empty
             | x::_ -> I.(x <|> string a " "))
          s
      in
      I.(string a (" " ^ User.presence_to_string p ^ " ") <|> status <|> Chars.hdash a 1)
    in
    Utils.option
      I.empty
      (function
        | `Session s -> tr s.User.presence s.User.status
        | `Member m -> tr m.Muc.presence m.Muc.status)
      resource
  in
  v_space (Chars.hdash a 1) width (I.hcat [ pre ; scroll ; filter ; jid ]) I.(otr <|> presence_status)

let status_line self mysession notify log a width =
  let a = A.(a ++ st bold) in
  let notify = if notify then I.string A.(a ++ st blink ++ Cli_colour.kind `Warning) "##" else Chars.hdash a 2
  and jid =
    let data = User.userid self mysession
    and a' = if log then A.(st reverse) else a
    in
    I.(string a "< " <|> string a' data <|> string a " >")
  and status =
    let data = User.presence_to_string mysession.User.presence
    and color = if mysession.User.presence = `Offline then `Bad else `Good
    in
    I.(string a "[ " <|> string A.(buddy_to_color color ++ a) data <|> string a " ]" <|> Chars.hdash a 1)
  in
  v_space (Chars.hdash a 1) width I.(notify <|> jid) status

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
    let lh =
      let s = state.log_height in
      if s + 10 > height then 0 else s
    in
    if lh = 0 then
      (0, height - 3)
    else
      (lh, height - lh - 3)
  and buddy_width, chat_width =
    let b = state.buddy_width in
    match state.window_mode with
    | BuddyList -> if b + 20 > width then (0, width) else (b, width - b - 1)
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
      let pre, post = state.input in

      let split_lines lines =
        let line_of_list lst img =
          Notty.Infix.(I.uchars A.empty (Array.of_list lst) <-> img)
        in
        match
          List.fold_right
            (fun ch (line_acc, acc) ->
               if Uchar.(equal (of_char '\n')) ch
               then [], line_of_list line_acc acc
               else ch::line_acc, acc)
            lines ([], I.empty)
        with
        | [], acc -> acc
        | last, img -> line_of_list last img
      in
      let iinp = split_lines pre
      and iinp2 = split_lines post
      in
      let r = match post with
        | [] ->
          let input = char_list_to_str pre in
          ( match Cli_commands.completion state input with
            | [] -> I.empty
            | [x] -> I.string (Cli_colour.kind `Info) x
            | xs -> I.string (Cli_colour.kind `Info) (String.concat "|" xs) )
        | _ -> iinp2
      in
      v_center iinp r width
    and main =
      let msgfmt = format_message tz_offset_s now (self state) active resource
      and msgfilter = msgfilter active state.active_contact
      and msgs strip msgfilter msgfmt =
        let mfilter, fmt = match active with
          | `User x when x.User.self -> ((fun _ -> true), logfmt)
          | _ -> (msgfilter, msgfmt)
        in
        let max =
          (* this is an upper limit *)
          (succ state.scrollback) * main_height
        in
        let filter = match state.filter with
          | None -> (fun id -> id)
          | Some x -> List.filter (fun { User.message ; _ } -> Astring.String.is_infix ~affix:x message)
        in
        let presence_filter =
          if state.ignore_presence
          then List.filter (function { User.kind = `Presence ; _ } -> false
                                   | _ -> true)
          else (fun id -> id)
        in
        let data = Utils.take_rev max
            (List.filter mfilter (presence_filter (filter (Contact.messages active))))
        in
        let image = render_wrapped_list strip chat_width (List.map fmt data) in
        let bottom = state.scrollback * main_height in
        I.vsnap ~align:`Bottom main_height (I.vcrop 0 bottom image)
      in
      match state.window_mode with
      | BuddyList ->
        let buddies = render_buddy_list (buddy_width, main_height) state
        and vline = Chars.vdash a main_height
        in
        I.(buddies <|> vline <|> msgs true msgfilter msgfmt)
      | FullScreen -> msgs true msgfilter msgfmt
      | Raw ->
        let p m = match m.User.direction with `From _ -> true | _ -> false
        and msgfmt x = A.empty, x.User.message
        in
        msgs false p msgfmt
    and hline = horizontal_line active resource a state.scrollback state.filter width
    and bottom =
      let self = self state in
      let status =
        let notify = has_any_notifications state
        and log = Contact.preserve_messages active
        and mysession = selfsession state
        in
        status_line self mysession notify log a width
      and logs =
        if log_height = 0 then
          I.empty
        else
          let msgs = Utils.take_rev log_height self.User.message_history in
          let l = render_wrapped_list true width (List.map logfmt msgs) in
          I.vsnap ~align:`Bottom log_height l
      in
      I.(logs <-> status)
    in
    let disp =
      if state.config.Xconfig.log_top then
        I.(bottom <-> main <-> hline <-> input)
      else
        I.(main <-> hline <-> bottom <-> input)
    in
    (disp, cursorc)

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
           send x (Some session) jid None body
         | _ -> Lwt.return_unit
       in
       Lwt_list.iter_s send_out otr_sessions)
    !xmpp_session

(* how should I know what is smooth? *)
let redraw_interval = 0.04

(* this is rendering and drawing stuff to terminal, waiting for updates of the ui_mvar... *)
let rec loop term size redrawer mvar input_mvar state =
  let reset state =
    let buddies = Contact.fold (fun _ b acc -> Contact.reset b :: acc) state.contacts [] in
    List.iter (Contact.replace_contact state.contacts) buddies
  in
  Lwt_engine.stop_event redrawer ;
  let redraw = Lwt_engine.on_timer redraw_interval false (fun _ ->
      let image, cursorc =
        try
          render_state size state
        with e ->
          let e = Cli_colour.kind `Error, (Printexc.to_string e)
          and note =
            "While trying to render the UI.  Try to scroll to another buddy \
             (Page Up/Down), switch rendering of buddy list (F12), or clear \
             this buddies messages (/clear<ret>); please report this bug \
             (including the offending characters and the error message)\n"
          in
          let w = fst size in
          (render_wrapped_list true w [e ; A.empty, note], 1)
      in
      Lwt.async (fun () ->
          Notty_lwt.Term.image term image >>= fun () ->
          Notty_lwt.Term.cursor term (Some (cursorc, snd size))))
  in
  Lwt_mvar.take mvar >>= fun action ->
  Lwt.catch (fun () -> action state)
    (fun exn ->
       add_status ~kind:`Error state (`Local ((`Full state.config.Xconfig.jid), "error")) (Printexc.to_string exn) ;
       Lwt.return (`Failure state)) >>= function
  | `Ok state -> loop term size redraw mvar input_mvar state
  | `Resize size -> loop term size redraw mvar input_mvar state
  | `Disconnect state -> reset state ; loop term size redraw mvar input_mvar state
  | `Failure state ->
    reset state ;
    ignore (Lwt_engine.on_timer 10. false
              (fun _ -> Lwt.async (fun () -> Lwt_mvar.put state.connect_mvar Reconnect))) ;
    loop term size redraw mvar input_mvar state
  | `Ask c ->
    c state input_mvar mvar >>= fun s ->
    loop term size redraw mvar input_mvar s
  | `Quit state ->
    quit state >>= fun () ->
    Lwt.return state
