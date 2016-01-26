open Cli_support
open Lwt.Infix
open Notty

let safe_int_of_string x =
  try Lwt.return (int_of_string x) with
  | Failure _ -> Lwt.fail (Invalid_argument "int_of_string")

let string_of_int_option default = function
  | None -> default
  | Some x -> string_of_int x

let configure term () =
  let above =
    let greet =
      "Welcome to Jackline configuration. You will be guided through the setup. \
       Input format will be explained by example in square brackets. \
       You can accept a default value by pressing enter."
    in
    [I.string A.empty greet]
  in

  (* JID *)
  let prefix = "Enter your jabber ID [user@server/resource]: " in
  read_line ~above ~prefix term >>= fun jid ->
  ( match Xjid.string_to_jid jid with
    | Some (`Full f) -> Lwt.return f
    | _ -> Lwt.fail (Invalid_argument "Jabber ID") ) >>= fun jid ->

  let ((_, dom), _) = jid in
  let above =
    let txt = "Jabber ID: " ^ Xjid.full_jid_to_string jid in
    above @ [I.string A.empty txt]
  in

  (* Priority *)
  read_line ~above ~prefix:"Enter priority (default 0): " term >>= fun prio ->
  (match prio with
   | "" -> Lwt.return None
   | x -> safe_int_of_string x >>= function
          | prio when prio > 0 && prio < 127 -> Lwt.return (Some prio)
          | _ -> Lwt.fail (Invalid_argument "Priority (between 0 and 127)")
  ) >>= fun priority ->

  let above =
    let txt = "Priority: " ^ string_of_int_option "0 (default)" priority in
    above @ [I.string A.empty txt]
  in

  (* Server name *)
  let prefix =
    let server = Printf.sprintf "Does connecting to server %s look ok to you?" dom in
    server ^ "  If not, please specify servername: "
  in
  read_line ~above ~prefix term >>= fun hostname ->
  let hostname = match hostname with
    | "" -> None
    | x -> Some x (* XXX: check that hostname is valid! *)
  in

  let above =
    let servername = "Servername: " ^ Utils.option dom (fun x -> x) hostname in
    above @ [I.string A.empty servername]
  in

  (* Port *)
  read_line ~above ~prefix:"Enter port (default 5222): " term >>= fun port ->
  ( match port with
    | "" -> Lwt.return None
    | x -> safe_int_of_string x >>= function
           | x when x > 0 && x < 65536 -> Lwt.return (Some x)
           | _ -> Lwt.fail (Invalid_argument "Port number (between 1 and 65535)")
  ) >>= fun port ->

  let above =
    let port = "Port: " ^ string_of_int_option "5222 (default)" port in
    above @ [I.string A.empty port]
  in

  (* Password *)
  (let prefix = "Enter password (if empty, will be asked at every startup): " in
   read_password ~above ~prefix term >|= function
   | "" -> None
   | x -> Some x) >>= fun password ->

  let above =
    let pw = "Password: " in
    let chars = match password with
      | None -> I.string A.empty "none provided, will be asked at startup"
      | Some _ -> I.uchar A.empty (`Uchar 0x2605) 5 1
    in
    above @ [I.(string A.empty pw <|> chars)]
  in

  (* trust anchor *)
  let prefix =
    "Jackline has two distinct ways to authenticate a Jabber server: either a \
     X.509 chain of trust where you provide the anchor OR the SHA256 fingerprint \
     of the X.509 certificate.  If you like to provide a trust anchor, specify \
     the path to the trust anchor file (otherwise leave this empty): "
  in

  read_line ~above ~prefix term >>= (function
   | "" ->
      (* XXX: actually ASK whether we should try to connect now *)
      let prefix =
        let pre = " (run `openssl s_client -connect "
        and post = " -starttls xmpp | openssl x509 -sha256 -fingerprint -noout`): "
        in
        let hostport =
          Utils.option dom (fun x -> x) hostname ^ ":" ^
            string_of_int (Utils.option 5222 (fun x -> x) port)
        in
        "Enter fingerprint " ^ pre ^ hostport ^ post
      in
      read_line ~above ~prefix term >>= fun fp ->
      (try
          let dotted_hex_to_cs hex =
            Nocrypto.Uncommon.Cs.of_hex
              (String.map (function ':' -> ' ' | x -> x) hex)
          in
          let binary = dotted_hex_to_cs fp in
          if Cstruct.len binary <> 32 then
            Lwt.fail (Invalid_argument "fingerprint is either too short or too long")
          else
            Lwt.return fp
        with _ ->
          Lwt.fail (Invalid_argument "please provide only hex characters (or whitespace or colon)") ) >|= fun fp ->
      `Fingerprint fp
   | trust_anchor ->
      Lwt_unix.access trust_anchor [ Unix.F_OK ; Unix.R_OK ] >>= fun () ->
      X509_lwt.certs_of_pem trust_anchor >>= fun tas ->
      match X509.Validation.valid_cas ~time:(Unix.time ()) tas with
      | [] -> Lwt.fail (Invalid_argument "trust anchor file is empty!")
      | _ -> Lwt.return (`Trust_anchor trust_anchor)
   ) >>= fun authenticator ->

  let above =
    let txt =
      match authenticator with
      | `Fingerprint fp -> "Certificate fingerprint: " ^ fp
      | `Trust_anchor file -> "Trust anchor: " ^ file
    in
    above @ [I.string A.empty txt]
  in

  (* SubjectAlternativeName *)
  let prefix = "Server name in certificate (defaults to " ^ dom ^ "): " in
  read_line ~above ~prefix term >|= (function
    | "" -> None
    | x -> Some x) >>= fun certificate_hostname ->

  let above =
    let name = Utils.option (dom ^ " (default)") (fun x -> x) certificate_hostname in
    let txt = "Certificate server name: " ^ name in
    above @ [I.string A.empty txt]
  in

  (* otr config *)
  let prefix = "OTR protocol version 3 support (default: yes)? " in
  read_yes_no ~above ~prefix true term >>= fun v3 ->
  let above' versions =
    let text = "OTR versions: " in
    let v = String.concat ", " (List.map Otr.State.version_to_string versions) in
    above @ [I.string A.empty (text ^ v)]
  in
  let versions = if v3 then [ `V3 ] else [] in
  let prefix = "OTR protocol version 2 support (default: yes)? " in
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

  let prefix = "Require encryption (default: yes)? " in
  read_yes_no ~above ~prefix true term >>= fun require ->

  let pols = if require then [ `REQUIRE_ENCRYPTION ] else [] in
  let prefix = "Send whitespace tag (default: yes)? " in
  read_yes_no ~above:(above' pols) ~prefix true term >>= fun ws_tag ->

  let pols =
    pols @ if ws_tag then [ `SEND_WHITESPACE_TAG ] else []
  in
  let prefix = "Whitespace tag starts key exchange (default: yes)? " in
  read_yes_no ~above:(above' pols) ~prefix true term >>= fun ws_start ->

  let pols =
    pols @ if ws_start then [ `WHITESPACE_START_AKE ] else []
  in
  let prefix = "Error starts key exchange (default: yes)? " in
  read_yes_no ~above:(above' pols) ~prefix true term >>= fun err_start ->

  let pols =
    pols @ if err_start then [ `ERROR_START_AKE ] else []
  in
  let prefix = "Reveal MAC after usage (default: no)? " in
  read_yes_no ~above:(above' pols) ~prefix false term >>= fun reveal ->

  let policies =
    pols @ if reveal then [ `REVEAL_MACS ] else []
  in

  let dsa = Nocrypto.Dsa.generate `Fips1024 in
  let otr_config = Otr.State.config versions policies in

  let above =
    let fp = User.pp_binary_fingerprint (Otr.Utils.own_fingerprint dsa) in
    above' pols @ [I.string A.empty ("Your newly generated OTR fingerprint: " ^ fp)]
  in

  (read_line ~above ~prefix:"Program to execute on notification: " term >|= function
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
