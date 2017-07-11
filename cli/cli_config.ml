open Cli_support
open Lwt.Infix
open Notty
open Astring

let safe_int_of_string x =
  try int_of_string x with Failure _ -> -1

let string_of_int_option default = function
  | None -> default
  | Some x -> string_of_int x

let rewrap term above below (prefix, inp, inp2) (width, _) =
  let wrap = render_wrapped_list true width in
  let content = wrap [A.empty, prefix] in
  let input = I.(inp <|> inp2) in
  let above = wrap above in
  let below = wrap below in
  let input = if I.width input = 0 then I.string A.empty " " else input in
  let image = I.(above <-> content <-> input <-> below) in
  Notty_lwt.Term.image term image >>= fun () ->
  let col, row =
    let col = I.width inp in
    let h = I.(height (above <-> content <-> inp)) in
    let height = if col = width then h else pred h in
    (col mod width, height)
  in
  Notty_lwt.Term.cursor term (Some (col, row))

let read_line ?(above = []) ?(prefix = "") ?default ?(below = []) term =
  let rec go (pre, post) =
    let iinp =
      let inp = Array.of_list pre in
      I.uchars A.(st reverse) inp
    and iinp2 =
      let inp2 = Array.of_list post in
      I.uchars A.(st reverse) inp2
    in
    rewrap term above below (prefix, iinp, iinp2) (Notty_lwt.Term.size term) >>= fun () ->
    Lwt_stream.next (Notty_lwt.Term.events term) >>= fun e ->
    match readline_input e with
    | `Ok f -> go (f (pre, post))
    | `Unhandled k ->
      match emacs_bindings k with
      | `Ok f -> go (f (pre, post))
      | `Unhandled k ->
        match k with
        | `Key (`Enter, []) -> Lwt.return (char_list_to_str (pre @ post))
        | `Key (`Uchar x, [`Ctrl]) when Uchar.to_int x = 0x43 -> Lwt.fail (Invalid_argument "Ctrl-c")
        | `Key (`Uchar x, [`Ctrl]) when Uchar.to_int x = 0x44 -> Lwt.fail (Invalid_argument "Ctrl-d")
        | _ -> go (pre, post)
  in
  let pre = Utils.option [] str_to_char_list default in
  go (pre, [])

let read_password ?(above = []) ?(prefix = "") ?(below = []) term =
  let rec go pre =
    let w = I.(width (uchars A.empty (Array.of_list pre))) in
    let input = Chars.star A.(st reverse) w in
    rewrap term above below (prefix, input, I.empty) (Notty_lwt.Term.size term) >>= fun () ->
    Lwt_stream.next (Notty_lwt.Term.events term) >>= function
      | `Key (`Enter, []) -> Lwt.return (char_list_to_str pre)
      | `Key (`Backspace, []) ->
         (match List.rev pre with
          | [] -> go pre
          | _::tl -> go (List.rev tl))
      | `Key (`Uchar chr, []) -> go (pre @ [chr])
      | _ -> go pre
  in
  go []

let rec read_yes_no ?above ?prefix ?below def term =
  let default = if def then "yes" else "no" in
  read_line ?above ?below ?prefix ~default term >>= function
    | "" -> Lwt.return def
    | "y" | "Y" | "yes" | "Yes" | "YES" -> Lwt.return true
    | "n" | "N" | "no" | "No" | "NO" -> Lwt.return false
    | _ -> read_yes_no ?above ?prefix ?below def term

let ask above ?(below = []) prefix ?default transform valid term =
  let rec doit diderror above below prefix ?default transform valid term =
    read_line ~above ~below ~prefix ?default term >>= fun data ->
    match valid (transform data) with
    | `Ok f -> Lwt.return f
    | `Invalid ->
      let below =
        if diderror then
          below
        else
          let im = (A.(fg red), "invalid data, try again") in
          im :: below
      in
      doit true above below prefix ?default transform valid term
  in
  doit false above below prefix ?default transform valid term

let configure term () =
  let above =
    let greet =
      "Welcome to Jackline configuration. You will be guided through the setup."
    in
    [A.empty, greet]
  in

  (* JID *)
  let prefix = "Jabber ID: " in
  let below = [A.empty, "format must be 'user@server/resource'"] in
  ask above ~below prefix
    Xjid.string_to_jid
    (function Some (`Full f) -> `Ok f | _ -> `Invalid)
    term >>= fun jid ->

  let ((_, dom), _) = jid in
  let above = above @ [A.empty, "Jabber ID: " ^ Xjid.full_jid_to_string jid] in

  (* Priority *)
  let below = [A.empty, "between 0 and 128"] in
  ask above ~below "Priority: " ~default:"0"
    safe_int_of_string
    (function
      | p when p = 0 -> `Ok None
      | p when p >= 0 && p < 128 -> `Ok (Some p)
      | _ -> `Invalid)
    term >>= fun priority ->

  let above =
    let txt = "Priority: " ^ string_of_int_option "0 (default)" priority in
    above @ [A.empty, txt]
  in

  (* Server name *)
  ask above "Servername: " ~default:dom
    (fun x -> x)
    (function
      | "" -> `Invalid
      | x when x = dom -> `Ok None
      | x -> `Ok (Some x))
    term >>= fun hostname ->

  let above =
    let servername = "Servername: " ^ Utils.option dom (fun x -> x) hostname in
    above @ [A.empty, servername]
  in

  (* Port *)
  ask above "TCP Port: " ~default:"5222"
    safe_int_of_string
    (function
      | x when x = 5222 -> `Ok None
      | x when x > 0 && x < 65536 -> `Ok (Some x)
      | _ -> `Invalid)
    term >>= fun port ->

  let above =
    above @ [A.empty, "Port: " ^ string_of_int_option "5222 (default)" port]
  in

  (* Password *)
  (let prefix = "Password (if empty, will be asked at every startup): " in
   read_password ~above ~prefix term >|= function
   | "" -> None
   | x -> Some x) >>= fun password ->

  let above =
    let pw = "Password: " in
    let chars = match password with
      | None -> "will be asked at startup"
      | Some _ -> "*****"
    in
    above @ [A.empty, pw ; A.empty, chars]
  in

  (* Fingerprint authenticator *)
  let prefix =
    "Certificate fingerprint (leave empty to instead specify CA file): "
  and below =
    let str1, str2 =
      let host =
        Utils.option dom (fun x -> x) hostname
      in
      let hostport =
        host ^ ":" ^ string_of_int (Utils.option 5222 (fun x -> x) port)
      in
      "If you have `tlsclient` installed, run: "
      ^ "`tlsclient -z --starttls=xmpp " ^ hostport ^ "`",
      "Alternatively: `openssl s_client -connect " ^ hostport
      ^ " -starttls xmpp | openssl x509 -sha256 -fingerprint -noout` (add -xmpphost " ^ host ^ " if available)"
    in
    [A.empty, str1; A.empty, str2]
  and transform fp =
    let dotted_hex_to_cs hex =
      try
        Nocrypto.Uncommon.Cs.of_hex
          (String.map (function ':' -> ' ' | x -> x) hex)
      with _ -> Cstruct.create 0
    in
    (fp, dotted_hex_to_cs fp)
  and valid = function
    | ("", _) -> `Ok None
    | (fp, x) when Cstruct.len x = 32 -> `Ok (Some fp)
    | _ -> `Invalid
  in
  ask above ~below prefix transform valid term >>= (function
  | None ->
    (* do sth smart here... ask doesn't allow valid to have lwt.t.. -- maybe it should!? *)
    ask above "CA file: " (fun x -> x) (fun x -> if Sys.file_exists x then `Ok x else `Invalid) term >>= fun trust_anchor ->
    Lwt_unix.access trust_anchor [ Unix.F_OK ; Unix.R_OK ] >>= fun () ->
    X509_lwt.certs_of_pem trust_anchor >>= fun tas ->
    (match X509.Validation.valid_cas ~time:(Unix.time ()) tas with
     | [] -> Lwt.fail (Invalid_argument "trust anchor file is empty!")
     | _ -> Lwt.return (`Trust_anchor trust_anchor))
  | Some fp -> Lwt.return (`Fingerprint fp) ) >>= fun authenticator ->

  let above =
    let txt =
      match authenticator with
      | `Fingerprint fp -> "Certificate fingerprint: " ^ fp
      | `Trust_anchor file -> "Trust anchor: " ^ file
    in
    above @ [A.empty, txt]
  in

  (* SubjectAlternativeName *)
  let prefix = "Server name in certificate: " in
  read_line ~above ~prefix ~default:dom term >|= (function
    | x when x = dom -> None
    | x -> Some x) >>= fun certificate_hostname ->

  let above =
    let name = Utils.option (dom ^ " (default)") (fun x -> x) certificate_hostname in
    let txt = "Certificate server name: " ^ name in
    above @ [A.empty, txt]
  in

  (* otr config *)
  let prefix = "OTR protocol version 3 support? " in
  read_yes_no ~above ~prefix true term >>= fun v3 ->
  let above' versions =
    let text = "OTR versions: " in
    let v = String.concat ~sep:", " (List.map Otr.State.version_to_string versions) in
    above @ [A.empty, text ^ v]
  in
  let versions = if v3 then [ `V3 ] else [] in
  let prefix = "OTR protocol version 2 support? " in
  read_yes_no ~above:(above' versions) ~prefix true term >>= fun v2 ->

  let versions = versions @ if v2 then [ `V2 ] else [] in
  (match versions with
   | [] -> Lwt.fail (Invalid_argument "no OTR version selected")
   | _ -> Lwt.return_unit) >>= fun () ->

  let above = above' versions in

  let not_slack = not (Astring.String.is_infix ~affix:"slack.com" dom) in

  let above' pols =
    let txt = "OTR policies: " in
    let p = String.concat ~sep:", " (List.map Otr.State.policy_to_string pols) in
    above @ [A.empty, txt ^ p]
  in

  let prefix = "Require encryption? " in
  read_yes_no ~above ~prefix not_slack term >>= fun require ->

  let pols = if require then [ `REQUIRE_ENCRYPTION ] else [] in
  let prefix = "Send whitespace tag? " in
  read_yes_no ~above:(above' pols) ~prefix not_slack term >>= fun ws_tag ->

  let pols =
    pols @ if ws_tag then [ `SEND_WHITESPACE_TAG ] else []
  in
  let prefix = "Whitespace tag starts key exchange? " in
  read_yes_no ~above:(above' pols) ~prefix true term >>= fun ws_start ->

  let pols =
    pols @ if ws_start then [ `WHITESPACE_START_AKE ] else []
  in
  let prefix = "Error starts key exchange? " in
  read_yes_no ~above:(above' pols) ~prefix not_slack term >>= fun err_start ->

  let pols =
    pols @ if err_start then [ `ERROR_START_AKE ] else []
  in
  let prefix = "Reveal MAC after usage? " in
  read_yes_no ~above:(above' pols) ~prefix false term >>= fun reveal ->

  let policies =
    pols @ if reveal then [ `REVEAL_MACS ] else []
  in

  let dsa = Nocrypto.Dsa.generate `Fips1024 in
  let otr_config = Otr.State.config versions policies in

  let above =
    let fp = User.pp_binary_fingerprint (Otr.Utils.own_fingerprint dsa) in
    above' pols @ [A.empty, "Your OTR fingerprint is " ^ fp]
  in

  (read_line ~above ~prefix:"Path to notification callback: " term >|= function
   | "" -> None
   | x -> Some x) >>= fun notification_callback ->

  let above =
    let x = match notification_callback with None -> "none" | Some x -> x in
    above @ [A.empty, "Notification callback " ^ x]
  in

  read_yes_no ~above ~prefix:"event log at top (default is bottom)?" false term >|= fun log_top ->

  Xconfig.({
      version = current_version ;
      jid ;
      priority ;
      hostname ;
      port ;
      password ;
      authenticator ;
      otr_config ;
      dsa ;
      certificate_hostname ;
      notification_callback ;
      log_top ;
    })
