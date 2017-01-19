open Cli_support
open Lwt.Infix
open Notty

let safe_int_of_string x =
  try int_of_string x with Failure _ -> -1

let string_of_int_option default = function
  | None -> default
  | Some x -> string_of_int x

let wrap w image =
  let w1 = I.width image in
  let rec go i =
    if (w1 - i) <= w then
      [ I.hcrop i 0 image ]
    else
      I.hcrop i (w1 - i - w) image :: go (i + w)
  in
  let vs = go 0 in
  I.vcat vs

let rewrap term above below (prefix, inp, inp2) (width, _) =
  let content = wrap width I.(prefix <|> inp <|> inp2) in
  let above = I.vcat (List.map (wrap width) above) in
  let below = I.vcat (List.map (wrap width) below) in
  let image = I.(above <-> content <-> below) in
  Notty_lwt.Term.image term image >>= fun () ->
  let col, row =
    let col = I.width prefix + I.width inp in
    let h =
      let content = wrap width I.(prefix <|> inp) in
      I.(height (above <-> content)) in
    let height = if col mod width = 0 then succ h else h in
    (succ (col mod width), height)
  in
  Notty_lwt.Term.cursor term (Some (col, row))

let read_line ?(above = []) ?(prefix = "") ?default ?(below = []) term =
  let rec go (pre, post) =
    let iprefix = I.string A.empty prefix
    and iinp =
      let inp = Array.of_list pre in
      I.uchars A.(st reverse) inp
    and iinp2 =
      let inp2 = Array.of_list post in
      I.uchars A.(st reverse) inp2
    in
    rewrap term above below (iprefix, iinp, iinp2) (Notty_lwt.Term.size term) >>= fun () ->
    Lwt_stream.next (Notty_lwt.Term.events term) >>= fun e ->
    match readline_input e with
    | `Ok f -> go (f (pre, post))
    | `Unhandled k ->
      match emacs_bindings k with
      | `Ok f -> go (f (pre, post))
      | `Unhandled k ->
        match k with
        | `Key (`Enter, []) -> Lwt.return (char_list_to_str (pre @ post))
        | `Key (`Uchar 0x43, [`Ctrl]) -> Lwt.fail (Invalid_argument "Ctrl-c")
        | `Key (`Uchar 0x44, [`Ctrl]) -> Lwt.fail (Invalid_argument "Ctrl-d")
        | _ -> go (pre, post)
  in
  let pre = Utils.option [] str_to_char_list default in
  go (pre, [])

let read_password ?(above = []) ?(prefix = "") ?(below = []) term =
  let rec go pre =
    let w = I.(width (uchars A.empty (Array.of_list pre))) in
    let input = Char.star A.(st reverse) w in
    let prefix = I.string A.empty prefix in
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
          let im = I.string A.(fg red) "invalid data, try again" in
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
    [I.string A.empty greet]
  in

  (* JID *)
  let prefix = "Jabber ID: " in
  let below = [I.string A.empty "format must be 'user@server/resource'"] in
  ask above ~below prefix
    Xjid.string_to_jid
    (function Some (`Full f) -> `Ok f | _ -> `Invalid)
    term >>= fun jid ->

  let ((_, dom), _) = jid in
  let above =
    let txt = "Jabber ID: " ^ Xjid.full_jid_to_string jid in
    above @ [I.string A.empty txt]
  in

  (* Priority *)
  let below = [I.string A.empty "between 0 and 128"] in
  ask above ~below "Priority: " ~default:"0"
    safe_int_of_string
    (function
      | p when p = 0 -> `Ok None
      | p when p >= 0 && p < 128 -> `Ok (Some p)
      | _ -> `Invalid)
    term >>= fun priority ->

  let above =
    let txt = "Priority: " ^ string_of_int_option "0 (default)" priority in
    above @ [I.string A.empty txt]
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
    above @ [I.string A.empty servername]
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
    let port = "Port: " ^ string_of_int_option "5222 (default)" port in
    above @ [I.string A.empty port]
  in

  (* Password *)
  (let prefix = "Password (if empty, will be asked at every startup): " in
   read_password ~above ~prefix term >|= function
   | "" -> None
   | x -> Some x) >>= fun password ->

  let above =
    let pw = "Password: " in
    let chars = match password with
      | None -> I.string A.empty "will be asked at startup"
      | Some _ -> I.uchar A.empty 0x2605 5 1
    in
    above @ [I.(string A.empty pw <|> chars)]
  in

  (* Fingerprint authenticator *)
  let prefix =
    "Certificate fingerprint (leave empty to instead specify CA file): "
  and below =
    let str1, str2 =
      let hostport =
        Utils.option dom (fun x -> x) hostname ^ ":" ^
        string_of_int (Utils.option 5222 (fun x -> x) port)
      in
      ("If you have `tlsclient` installed, run: "
      ^ "`tlsclient -z --starttls=xmpp " ^ hostport ^ "`",
      "Alternatively: `openssl s_client -connect " ^ hostport
      ^ " -starttls xmpp | openssl x509 -sha256 -fingerprint -noout`")
    in
    [I.(string A.empty str1 <-> string A.empty str2)]
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
    above @ [I.string A.empty txt]
  in

  (* SubjectAlternativeName *)
  let prefix = "Server name in certificate: " in
  read_line ~above ~prefix ~default:dom term >|= (function
    | x when x = dom -> None
    | x -> Some x) >>= fun certificate_hostname ->

  let above =
    let name = Utils.option (dom ^ " (default)") (fun x -> x) certificate_hostname in
    let txt = "Certificate server name: " ^ name in
    above @ [I.string A.empty txt]
  in

  (* otr config *)
  let prefix = "OTR protocol version 3 support? " in
  read_yes_no ~above ~prefix true term >>= fun v3 ->
  let above' versions =
    let text = "OTR versions: " in
    let v = String.concat ", " (List.map Otr.State.version_to_string versions) in
    above @ [I.string A.empty (text ^ v)]
  in
  let versions = if v3 then [ `V3 ] else [] in
  let prefix = "OTR protocol version 2 support? " in
  read_yes_no ~above:(above' versions) ~prefix true term >>= fun v2 ->

  let versions = versions @ if v2 then [ `V2 ] else [] in
  (match versions with
   | [] -> Lwt.fail (Invalid_argument "no OTR version selected")
   | _ -> Lwt.return_unit) >>= fun () ->

  let above = above' versions in

  let above' pols =
    let txt = "OTR policies: " in
    let p = String.concat ", " (List.map Otr.State.policy_to_string pols) in
    above @ [I.string A.empty (txt ^ p)]
  in

  let prefix = "Require encryption? " in
  read_yes_no ~above ~prefix true term >>= fun require ->

  let pols = if require then [ `REQUIRE_ENCRYPTION ] else [] in
  let prefix = "Send whitespace tag? " in
  read_yes_no ~above:(above' pols) ~prefix true term >>= fun ws_tag ->

  let pols =
    pols @ if ws_tag then [ `SEND_WHITESPACE_TAG ] else []
  in
  let prefix = "Whitespace tag starts key exchange? " in
  read_yes_no ~above:(above' pols) ~prefix true term >>= fun ws_start ->

  let pols =
    pols @ if ws_start then [ `WHITESPACE_START_AKE ] else []
  in
  let prefix = "Error starts key exchange? " in
  read_yes_no ~above:(above' pols) ~prefix true term >>= fun err_start ->

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
    above' pols @ [I.string A.empty ("Your OTR fingerprint is " ^ fp)]
  in

  (read_line ~above ~prefix:"Path to notification callback: " term >|= function
   | "" -> None
   | x -> Some x) >|= fun notification_callback ->

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
      notification_callback
    })
