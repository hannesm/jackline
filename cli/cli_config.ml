open Lwt
open React

class read_inputline ~term ~prompt () = object(self)
  inherit LTerm_read_line.read_line ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method! show_box = false

  initializer
    self#set_prompt (S.const (LTerm_text.of_string prompt))
end

class read_password term ~prompt = object(self)
  inherit LTerm_read_line.read_password () as super
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method! send_action = function
    | LTerm_read_line.Break ->
        (* Ignore Ctrl+C *)
        ()
    | action ->
        super#send_action action

  initializer
    self#set_prompt (S.const (LTerm_text.of_string prompt))
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
  LTerm.fprintl term "Welcome to Jackline configuration. You will be guided through the setup, question marked with \"[optional]\" are for advanced users and can be ignored." >>= fun () ->
  (new read_inputline ~term ~prompt:"enter jabber id (user@host/resource): " ())#run >>= fun jid ->
  (if not (exactly_one '@' jid) then
     fail (Invalid_argument "invalid jabber ID (needs exactly one @ character)")
   else return_unit) >>= fun () ->
  (if not (exactly_one '/' jid) then
     fail (Invalid_argument "invalid jabber ID (needs exactly one / character)")
   else return_unit ) >>= fun () ->
  (match User.Jid.string_to_jid jid with
   | None | Some (`Bare _) -> fail (Invalid_argument "invalid jabber ID")
   | Some (`Full f) -> return f) >>= fun jid ->

  (new read_inputline ~term ~prompt:"[optional] enter priority (defaults to 0): " ())#run >>= fun prio ->
  (if prio = "" then
     return None
   else
     let prio = int_of_string prio in
     if prio < 0 || prio > 127 then
       fail (Invalid_argument "invalid priority (range allowed is from 0 to 127)")
     else return (Some prio)) >>= fun priority ->

  read_yes_no term "[optional] configure hostname manually?" >>= fun hostname ->
  ( if hostname then
      begin
        (new read_inputline ~term ~prompt:"enter hostname or IP: " ())#run >>= fun hostname ->
        (* XXX: check that hostname is valid! *)
        let hostname = match hostname with
          | "" -> None
          | x -> Some x
        in
        (new read_inputline ~term ~prompt:"enter port [5222]: " ())#run >>= fun port ->
        (if port = "" then
           return None
         else
           let port = int_of_string port in
           if port <= 0 || port > 65535 then
             fail (Invalid_argument "invalid port number")
           else return (Some port)) >>= fun (port) ->
        return (hostname, port)
      end
    else
      return (None, None) ) >>= fun (hostname, port) ->

  (new read_password term ~prompt:"[optional] password (otherwise will be asked at startup): ")#run >>= fun password ->
  let password = if password = "" then None else Some password in

  (* trust anchor *)
  read_yes_no term "Provide a pem-encoded trust anchor (alternative: SHA256 digest of the server certificate)?" >>= fun ta ->
  ( if ta then
      begin
        (new read_inputline ~term ~prompt:"enter path to trust anchor file: " ())#run >>= fun trust_anchor ->
        Lwt_unix.access trust_anchor [ Unix.F_OK ; Unix.R_OK ] >>= fun () ->
        X509_lwt.certs_of_pem trust_anchor >>= fun tas ->
        let tas = X509.Validation.valid_cas ~time:(Unix.time ()) tas in
        if List.length tas = 0 then
          fail (Invalid_argument "trust anchors are empty!")
        else
          return (`Trust_anchor trust_anchor)
      end
    else
      begin
         (* XXX: actually ASK whether we should try to connect now *)
        let pre = " (run `openssl s_client -connect "
        and post = " -starttls xmpp | openssl x509 -sha256 -fingerprint -noout`)"
        in
        let ((_, dom), _) = jid in
        let hostport = match hostname, port with
          | Some h, Some p -> pre ^ h ^ ":" ^ (string_of_int p) ^ post
          | None, Some p -> pre ^ dom ^ ":" ^ (string_of_int p) ^ post
          | Some h, None -> pre ^ h ^ ":5222" ^ post
          | None, None -> pre ^ dom ^ ":5222" ^ post
        in
        (new read_inputline ~term ~prompt:("enter server certificate fingerprint" ^ hostport ^ ": ") ())#run >>= fun fp ->
        (try
           let dotted_hex_to_cs hex =
             Nocrypto.Uncommon.Cs.of_hex
               (String.map (function ':' -> ' ' | x -> x) hex)
           in
           let binary = dotted_hex_to_cs fp in
           if Cstruct.len binary <> 32 then
             fail (Invalid_argument "fingerprint is either too short or too long")
           else
             return fp
         with _ ->
           fail (Invalid_argument "please provide only hex characters (or whitespace or colon)") ) >|= fun fp ->
        `Fingerprint fp
      end ) >>= fun authenticator ->

  (new read_inputline ~term ~prompt:("[optional] enter name in certificate (by default, domain of JID will be used): ") ())#run >>= fun certname ->
  let certificate_hostname = if certname = "" then None else Some certname in

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
    " (recommended)" >>= fun policies ->
  let dsa = Nocrypto.Dsa.generate `Fips1024 in
  let otr_config = Otr.State.config versions policies in

  (new read_inputline ~term ~prompt:"[optional] full path to program called on notifications: " ())#run >|= fun cb ->
  let notification_callback = if String.length cb = 0 then None else Some cb in

  let config = Xconfig.({
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
    }) in
  config
