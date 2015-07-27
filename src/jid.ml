
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

(* is jid a part of jid'? *)
let jid_matches jid jid' =
  match jid, jid' with
  | `Bare bare, `Bare bare' -> bare_jid_equal bare bare'
  | `Bare bare, `Full (bare', _) -> bare_jid_equal bare bare'
  | `Full (bare, resource), `Full (bare', resource') -> bare_jid_equal bare bare' && resource = resource'
  | _ -> false


let xmpp_jid_to_jid jid =
  let { JID.lnode ; JID.ldomain ; JID.lresource ; _ } = jid in
  let bare = (lnode, ldomain) in
  if lresource = "" then
    `Bare bare
  else
    `Full (bare, lresource)

let jid_to_xmpp_jid jid = JID.of_string (jid_to_string jid)
