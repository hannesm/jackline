
open Sexplib.Conv

type bare_jid = string * string with sexp
type full_jid = bare_jid * string with sexp

type t = [
  | `Full of full_jid
  | `Bare of bare_jid
] with sexp

let t_to_bare = function
  | `Full (b, _) -> b
  | `Bare b -> b

let resource = function
  | `Full (_, r) -> Some r
  | `Bare _ -> None

let bare_jid_to_string (u, d) = u ^ "@" ^ d
let full_jid_to_string (b, r) = bare_jid_to_string b ^ "/" ^ r

let jid_to_string = function
  | `Full full -> full_jid_to_string full
  | `Bare bare -> bare_jid_to_string bare

let string_to_jid_helper s =
  let parts = Stringext.split ~max:2 ~on:'@' s in
  if List.length parts = 2 then
    let user = List.nth parts 0
    and rest = List.nth parts 1
    in
    let domain, resource =
      if String.contains rest '/' then
        let parts = Stringext.split ~max:2 ~on:'/' rest in
        (List.hd parts,
         if List.length parts = 2 then
           Some (List.nth parts 1)
         else
           None)
      else
        (rest, None)
    in
    Some (user, domain, resource)
  else
    None

let string_to_bare_jid s =
  match string_to_jid_helper s with
  | Some (u, d, _) -> Some (u, d)
  | None -> None

let string_to_full_jid s =
  match string_to_jid_helper s with
  | Some (u, d, Some r) -> Some ((u, d), r)
  | _ -> None

let string_to_jid s =
  match string_to_jid_helper s with
  | Some (u, d, Some r) -> Some (`Full ((u, d), r))
  | Some (u, d, None) -> Some (`Bare (u, d))
  | None -> None

let bare_jid_equal (u, d) (u', d') = u = u' && d = d'

(*
   xmpp resources should be unique for each client, thus multiple
   sessions between two contacts can be established. great idea!
   unfortunately the real world is crap. look at some resources:
   -mcabber.123456 mcabber.123457
   -276687891418300495410099 276687891418300495410010 ...
   -D87A6DD1
   -gmail.1234D33 ...
   -5d234568880
   -BitlBee7D0D5864
   -AAAA AAAAB (size increases/decreases (because some use random and print without leading 0s))
   -23d22ef2-6bf9-4531-abb2-e42418a4713b, 22fa77f2-9333-41b9-81e1-4cebf042ac18 (agl/xmpp-client)

   thus I have some magic here to uniquify sessions... the idea is
   (read: hand-wavy):
    if two resources share a common prefix and have some random hex numbers,
    they are similar!

    or, if they are both UUIDs (some servers use UUID if the client doesn't come up with a resource)

   this naive obviously fails:
     user X with AAAA comes online, user X with AAAB comes online
     (read: these are similar) -- then AAAA goes offline.. AAAB is
     still online (and this order of events happens on a reconnect due
     to timeout)

   thus only the otr ctx is copied over, and the dispose flag is set...
   when a contact goes offline where dispose is set, the session is removed
 *)

let hex_chars start s =
  let hex_char = function
    | 'a' .. 'f' | 'A' .. 'F' | '0' .. '9' -> true
    | _ -> false
  in
  let rec go idx s =
    if idx < String.length s then
      if hex_char (String.get s idx) then
        go (succ idx) s
      else
        false
    else
      true
  in
  go start s

let is_uuid s =
  let open String in
  length s = 36 &&
  hex_chars 0 (sub s 0 8) &&
  get s 8 = '-' &&
  hex_chars 0 (sub s 9 4) &&
  get s 13 = '-' &&
  hex_chars 0 (sub s 14 4) &&
  get s 18 = '-' &&
  hex_chars 0 (sub s 19 4) &&
  get s 23 = '-' &&
  hex_chars 0 (sub s 24 12)

let resource_similar a b =
  let alen = String.length a
  and blen = String.length b
  in
  if abs (alen - blen) > 2 then
    false (* they're a bit too much off *)
  else if is_uuid a && is_uuid b then
    true
  else
    let stop = min alen blen in
    let rec equal idx =
      if idx < stop then
        if String.get a idx = String.get b idx then
          equal (succ idx)
        else
          idx
      else
        idx
    in
    let prefix_len = equal 0 in
    hex_chars prefix_len a && hex_chars prefix_len b

(* is jid a part of jid'? *)
let jid_matches jid jid' =
  match jid, jid' with
  | `Bare bare, `Bare bare' -> bare_jid_equal bare bare'
  | `Bare bare, `Full (bare', _) -> bare_jid_equal bare bare'
  | `Full (bare, resource), `Full (bare', resource') -> bare_jid_equal bare bare' && resource_similar resource resource'
  | _ -> false


let xmpp_jid_to_jid jid =
  let { JID.lnode ; JID.ldomain ; JID.lresource ; _ } = jid in
  let bare = (lnode, ldomain) in
  if lresource = "" then
    `Bare bare
  else
    `Full (bare, lresource)

let jid_to_xmpp_jid jid = JID.of_string (jid_to_string jid)
