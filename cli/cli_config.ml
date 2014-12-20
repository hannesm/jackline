open Lwt
open React

class read_inputline ~term ~prompt () = object(self)
  inherit LTerm_read_line.read_line ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method! show_box = false

  initializer
    self#set_prompt (S.const (LTerm_text.of_string prompt))
end

class read_password term = object(self)
  inherit LTerm_read_line.read_password () as super
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method! send_action = function
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
     fail (Invalid_argument "invalid jabber ID (needs exactly one @ character)")
   else return_unit) >>= fun () ->
  (if not (exactly_one '/' jid) then
     fail (Invalid_argument "invalid jabber ID (needs exactly one / character)")
   else return_unit ) >>= fun () ->
  let jid = try Some (JID.of_string (String.lowercase jid)) with _ -> None in
  (match jid with
   | None -> fail (Invalid_argument "invalid jabber ID")
   | Some x -> return x) >>= fun jid ->
  let { JID.ldomain ; _ } = jid in
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
        (new read_inputline ~term ~prompt:"enter path to trust anchor file: " ())#run >>= fun trust_anchor ->
        Lwt_unix.access trust_anchor [ Unix.F_OK ; Unix.R_OK ] >>= fun () ->
        X509_lwt.certs_of_pem trust_anchor >>= fun tas ->
        let tas = Certificate.valid_cas ~time:(Unix.time ()) tas in
        if List.length tas = 0 then
          fail (Invalid_argument "trust anchors are empty!")
        else
          return (`Trust_anchor trust_anchor)
      end
    else
      begin
        (new read_inputline ~term ~prompt:("enter server certificate fingerprint (run `openssl s_client -connect " ^ ldomain ^ ":" ^ (string_of_int port) ^ " -starttls xmpp | openssl x509 -sha256 -fingerprint -noout`): ") ())#run >>= fun fp ->
        (try
           let binary = X509.Cs.dotted_hex_to_cs fp in
           if Cstruct.len binary <> 32 then
             fail (Invalid_argument "fingerprint is either too short or too long")
           else
             return fp
         with _ ->
           fail (Invalid_argument "please provide only hex characters (or whitespace or colon)") ) >|= fun fp ->
        `Fingerprint fp
      end ) >>= fun authenticator ->
  (* otr config *)
  LTerm.fprintl term "OTR config" >>= fun () ->
  let ask_list xs to_string prefix suffix =
    Lwt_list.fold_left_s (fun acc v ->
      read_yes_no term (prefix ^ (to_string v) ^ suffix) >|= function
      | true -> v :: acc
      | false -> acc)
      [] xs
  in
  ask_list
    Otr.State.all_versions
    Otr.State.version_to_string
    "Protocol "
    " support (recommended)" >>= fun versions ->
  ( if List.length versions = 0 then
      fail (Invalid_argument "no OTR version selected")
    else
      return versions ) >>= fun versions ->

  ask_list
    Otr.State.all_policies
    Otr.State.policy_to_string
    ""
    " (recommended)" >|= fun policies ->
  let dsa = Nocrypto.Dsa.generate `Fips1024 in
  let otr_config = {
    Otr.State.versions = versions ;
    Otr.State.policies = policies ;
    Otr.State.dsa = dsa
  } in
  let config = Config.({
      version = current_version ;
      jid ;
      port ;
      password ;
      authenticator ;
      otr_config
    }) in
  config
