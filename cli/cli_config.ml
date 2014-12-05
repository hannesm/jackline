open Lwt
open React

class read_inputline ~term ~prompt () = object(self)
  inherit LTerm_read_line.read_line ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method show_box = false

  initializer
    self#set_prompt (S.const (LTerm_text.of_string prompt))
end

class read_password term = object(self)
  inherit LTerm_read_line.read_password () as super
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method send_action = function
    | LTerm_read_line.Break ->
        (* Ignore Ctrl+C *)
        ()
    | action ->
        super#send_action action

  initializer
    self#set_prompt (S.const (LTerm_text.of_string "password: "))
end

let rec read_yes_no term msg =
  (new read_inputline ~term ~prompt:(msg ^ " [answer 'y' or 'n']: ") ())#run >>= fun res ->
  match res with
  | "Y" | "y" | "Yes" | "yes" -> return true
  | "N" | "n" | "No"  | "no"  -> return false
  | _ -> read_yes_no term msg

let exactly_one char s =
  String.contains s char &&
  try String.index_from s (succ (String.index s char)) char = 0 with Not_found -> true

let configure term () =
  (new read_inputline ~term ~prompt:"enter jabber id (user@host/resource): " ())#run >>= fun jid ->
  (if not (exactly_one '@' jid) then
     fail (Invalid_argument "not a valid jabber ID (exactly one @ character)")
   else return_unit) >>= fun () ->
  (if not (exactly_one '/' jid) then
     fail (Invalid_argument "not a valid jabber ID (exactly one / character)")
   else return_unit ) >>= fun () ->
  let jid = try Some (JID.of_string (String.lowercase jid)) with _ -> None in
  (match jid with
   | None -> fail (Invalid_argument "bad jabber ID")
   | Some x -> return x) >>= fun jid ->
  let { JID.ldomain } = jid in
  (new read_inputline ~term ~prompt:"enter port [5222]: " ())#run >>= fun port ->
  let port = if port = "" then 5222 else int_of_string port in
  (if port <= 0 || port > 65535 then
     fail (Invalid_argument "invalid port number")
   else return_unit ) >>= fun () ->
  (new read_password term)#run >>= fun password ->
  (* trust anchor *)
  read_yes_no term "Provide trust anchor (alternative: tls server fingerprint)?" >>= fun ta ->
  ( if ta then
      begin
        (new read_inputline ~term ~prompt:"enter path to trust anchor: " ())#run >>= fun trust_anchor ->
        Lwt_unix.access trust_anchor [ Unix.F_OK ; Unix.R_OK ] >|= fun () ->
        (Some trust_anchor, None)
      end
    else
      begin
        (new read_inputline ~term ~prompt:("enter server certificate fingerprint (by running for example `openssl s_client -connect " ^ ldomain ^ ":" ^ (string_of_int port) ^ " -starttls xmpp | openssl x509 -sha256 -fingerprint -noout`): ") ())#run >|= fun fp ->
        (None, Some fp)
      end ) >>= fun (trust_anchor, tls_fingerprint) ->
  (* otr config *)
  LTerm.fprintl term "OTR config" >>= fun () ->
  read_yes_no term "Protocol version 2 support (recommended)" >>= fun v2 ->
  read_yes_no term "Protocol version 3 support (recommended)" >>= fun v3 ->
  read_yes_no term "Require OTR encryption (recommended)" >>= fun require ->
  read_yes_no term "Send whitespace tag (recommended)" >>= fun send_whitespace ->
  read_yes_no term "Whitespaces starts key exchange (recommended)" >>= fun whitespace_starts ->
  read_yes_no term "Error starts key exchange (recommended)" >>= fun error_starts ->
  let dsa = Nocrypto.Dsa.generate `Fips1024 in
  (match v2, v3 with
   | true, true -> return [`V3 ; `V2 ]
   | true, false -> return [ `V2 ]
   | false, true -> return [ `V3 ]
   | false, false -> fail (Invalid_argument "no OTR version selected") ) >>= fun versions ->
  let policies = List.flatten [
      if require then [`REQUIRE_ENCRYPTION] else [] ;
      if send_whitespace then [`SEND_WHITESPACE_TAG] else [] ;
      if whitespace_starts then [`WHITESPACE_START_AKE] else [] ;
      if error_starts then [`ERROR_START_AKE] else [] ]
  in
  let otr_config = { Otr.State.versions = versions ; Otr.State.policies = policies ; Otr.State.dsa = dsa } in
  let config = Config.({ version = empty.version ; jid ; port ; password ; trust_anchor ; tls_fingerprint ; otr_config }) in
  return config
