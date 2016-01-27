open Cli_support
open Lwt.Infix
open Notty

let safe_int_of_string x =
  try int_of_string x with Failure _ -> -1

let string_of_int_option default = function
  | None -> default
  | Some x -> string_of_int x

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
  let prefix = "Jabber ID [user@server/resource]: " in
  ask above prefix Xjid.string_to_jid (function Some (`Full f) -> `Ok f | _ -> `Invalid) term >>= fun jid ->

  let ((_, dom), _) = jid in
  let above =
    let txt = "Jabber ID: " ^ Xjid.full_jid_to_string jid in
    above @ [I.string A.empty txt]
  in

  (* Priority *)
  ask above "Priority: " ~default:"0"
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
  ask above "Port: " ~default:"5222"
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
      | Some _ -> I.uchar A.empty (`Uchar 0x2605) 5 1
    in
    above @ [I.(string A.empty pw <|> chars)]
  in

  (* Fingerprint authenticator *)
  let prefix =
    "Certificate fingerprint (leave empty to instead specify CA file): "
  and below =
    let str =
      let hostport =
        Utils.option dom (fun x -> x) hostname ^ ":" ^
        string_of_int (Utils.option 5222 (fun x -> x) port)
      in
      "run `openssl s_client -connect " ^ hostport ^
      " -starttls xmpp | openssl x509 -sha256 -fingerprint -noout`"
    in
    [I.string A.empty str]
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
    ask above "CA file: " (fun x -> x) (fun x -> `Ok x) term >>= fun trust_anchor ->
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
