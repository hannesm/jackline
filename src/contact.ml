
type contact = [ `User of User.user | `Room of Muc.groupchat ]
type resource = [ `Session of User.session | `Member of Muc.member ]

let bare = function
  | `User x -> x.User.bare_jid
  | `Room x -> x.Muc.room_jid

let compare_contact a b =
  match a, b with
  | `Room _, `User _ -> -1
  | `User _, `Room _ -> 1
  | a, b -> Xjid.compare_bare_jid (bare a) (bare b)

let presence = function
  | `Member m -> m.Muc.presence
  | `Session s -> s.User.presence

let resource = function
  | `Member m -> m.Muc.nickname
  | `Session s -> s.User.resource

let active_presence = function
  | `Room r -> Utils.option `Offline (fun m -> m.Muc.presence) (Muc.self_member r)
  | `User x -> Utils.option `Offline (fun s -> s.User.presence) (User.active_session x)

let expanded = function
  | `Room r -> r.Muc.expand
  | `User u -> u.User.expand

let expand = function
  | `Room r -> `Room { r with Muc.expand = not r.Muc.expand }
  | `User u -> `User { u with User.expand = not u.User.expand }

let all_resources = function
  | `Room r -> List.map (fun m -> `Member m) (Muc.sorted_members r)
  | `User u -> List.map (fun s -> `Session s) (User.sorted_sessions u)

let active = function
  | `Room _ -> None
  | `User u -> Utils.option None (fun s -> Some (`Session s)) (User.active_session u)

let full_jid contact r =
  (bare contact, resource r)

let jid contact resource =
  Utils.option
    (`Bare (bare contact))
    (fun r -> `Full (full_jid contact r))
    resource

let active_resources tst contact =
  let bare = bare contact in
  let test r = presence r <> `Offline || tst (`Full (bare, resource r)) in
  List.filter test (all_resources contact)

let preserve_messages = function
  | `User u -> u.User.preserve_messages
  | `Room r -> r.Muc.preserve_messages

let set_preserve_messages b newval =
  match b with
  | `User u -> `User { u with User.preserve_messages = newval }
  | `Room r -> `Room { r with Muc.preserve_messages = newval }

let name = function
  | `User u -> Utils.option None (fun n -> Some ("name: " ^  n)) u.User.name
  | `Room r -> Some ("my nick: " ^ r.Muc.my_nick)

let info b s = match b, s with
  | `User u, None -> User.info u None
  | `User u, Some (`Session s) -> User.info u (Some s)
  | `Room r, None -> Muc.info r
  | `Room _, Some (`Member m) -> Muc.member_info m
  | _ -> assert false

let messages = function
  | `User u -> u.User.message_history
  | `Room r -> r.Muc.message_history

let reset = function
  | `User u -> `User (User.reset_user u)
  | `Room r -> `Room (Muc.reset_room r)

let clear_messages = function
  | `User u -> `User { u with User.message_history = [] }
  | `Room r -> `Room { r with Muc.message_history = [] }

let input_buffer = function
  | `User u -> u.User.input_buffer
  | `Room r -> r.Muc.input_buffer

let set_input_buffer contact data =
  match contact with
  | `User u -> `User { u with User.input_buffer = data }
  | `Room r -> `Room { r with Muc.input_buffer = data }

let new_message contact message =
  match contact with
  | `User u -> `User (User.new_message u message)
  | `Room r -> `Room (Muc.new_message r message)

let readline_history = function
  | `User u -> u.User.readline_history
  | `Room r -> r.Muc.readline_history

let add_readline_history b h =
  match b with
  | `User u -> `User { u with User.readline_history = h :: u.User.readline_history }
  | `Room r -> `Room { r with Muc.readline_history = h :: r.Muc.readline_history }

let color (b : contact) (r : resource option) =
  match b, r with
  | `User u, _                 when u.User.self               -> `Default
  | _      , Some (`Session s) when User.encrypted s.User.otr -> `Good
  | _      , Some (`Session _)                                -> `Bad
  | _      , _                                                -> `Default

let marshal_history contact =
  let open Sexplib.Conv in
  if preserve_messages contact then
    let new_msgs =
      List.filter (fun m ->
        match m.User.direction, m.User.persistent with
        | `Local _, _    -> false
        | _       , true -> false
        | _              -> true)
        (messages contact)
    in
    List.iter (fun x -> x.User.persistent <- true) new_msgs ;
    let hist_version = sexp_of_int 3 in
    if List.length new_msgs > 0 then
      let sexps = List.map User.sexp_of_message new_msgs in
      let sexp = Sexplib.Sexp.(List [ hist_version ; List sexps ]) in
      Some (Sexplib.Sexp.to_string_mach sexp)
    else
      None
  else
    None

module History_updates = struct
  open Sexplib

  let tr_m s =
    let open Sexp in
    let tr_dir = function
      | List [ Atom "From" ; Atom jid ] ->
         (match Xjid.string_to_jid jid with
          | Some jid -> List [ Atom "From" ; Xjid.sexp_of_t jid ]
          | None -> Printf.printf "from failed" ;
                    List [ Atom "From" ; Xjid.sexp_of_t (`Bare ("none", "none")) ])
      | x -> x
    in
    match s with
    | List s ->
       let r = List.fold_left (fun acc s ->
          let s = match s with
            | List [ Atom "direction" ; value ] -> List [ Atom "direction" ; tr_dir value ]
            | x -> x
          in
          s :: acc) [] s
       in
       List (List.rev r)
    | x -> x

  let tr_1 jid s =
    let open Sexp in
    let tr_dir = function
      | List [ Atom "To" ; id ] ->
         List [ Atom "To" ; List [ Xjid.sexp_of_t jid ; id ] ]
      | List [ Atom "Local" ; data ] ->
         List [ Atom "Local" ; List [ Xjid.sexp_of_t jid ; data ] ]
      | x -> x
    in
    match s with
    | List s ->
       let r = List.fold_left (fun acc s ->
          let s = match s with
            | List [ Atom "direction" ; value ] -> List [ Atom "direction" ; tr_dir value ]
            | x -> x
          in
          s :: acc) [] s
       in
       List (List.rev r)
    | x -> x

  let tr_kind = function
    | Sexp.List s -> Sexp.List (Sexp.List [ Sexp.Atom "kind" ; User.sexp_of_chatkind `Chat ] :: s)
    | x -> x
end

let load_history_hlp jid file =
  let open Sexplib in
  let open Sexplib.Conv in
  let load_h = function
    | Sexp.List [ ver ; Sexp.List msgs ] ->
      let version = int_of_sexp ver in
      let msgs = match version with
        | 0 -> List.map History_updates.tr_kind (List.map (History_updates.tr_1 jid) (List.map History_updates.tr_m msgs))
        | 1 -> List.map History_updates.tr_kind (List.map (History_updates.tr_1 jid) msgs)
        | 2 -> List.map History_updates.tr_kind msgs
        | 3 -> msgs
        | _ -> Printf.printf "unknown message format" ; []
      in
      List.map User.message_of_sexp msgs
    | _ -> Printf.printf "parsing history failed" ; []
  in
  match (try Some (Sexp.load_rev_sexps file) with _ -> None) with
  | Some hists -> List.flatten (List.map load_h hists)
  | _ -> []

let set_history b msgs =
  match b with
  | `Room r -> `Room { r with Muc.message_history = msgs }
  | `User u -> `User { u with User.message_history = msgs }

let received b id =
  let tst msg = match msg.User.direction with
    | `To (_, x) when x = id -> true
    | _ -> false
  in
  let msgs =
    List.map
      (fun m -> if tst m then { m with User.received = true } else m)
      (messages b)
  in
  set_history b msgs

let load_history directory contact =
  let bare = bare contact in
  let file = Filename.concat directory (Xjid.bare_jid_to_string bare) in
  let msgs = load_history_hlp (`Bare bare) file in
  set_history contact msgs

let marshal = function
  | `User u -> User.marshal_user u
  | `Room r -> Muc.marshal_room r

let oneline c r =
  match c, r with
  | `User u, None -> User.oneline u
  | `User u, Some (`Session s) -> User.oneline_with_session u s
  | `Room r, None -> Muc.oneline r
  | `Room r, Some (`Member m) -> Muc.oneline_with_member r m
  | _ -> assert false

module StringHash =
  struct
    type t = Xjid.bare_jid
    let equal = Xjid.bare_jid_equal
    let hash = Hashtbl.hash
  end

module Contacts = Hashtbl.Make(StringHash)
type contacts = contact Contacts.t

let create () = Contacts.create 100
let length = Contacts.length

let fold = Contacts.fold
let iter = Contacts.iter

let find_contact t id = try Some (Contacts.find t id) with Not_found -> None
let replace_contact contacts contact = Contacts.replace contacts (bare contact) contact
let remove = Contacts.remove

let find_room t id = match find_contact t id with
  | Some (`Room r) -> Some r
  | _ -> None

let find_user t id = match find_contact t id with
  | Some (`User u) -> Some u
  | _ -> None

let replace_room t r = replace_contact t (`Room r)
let replace_user t r = replace_contact t (`User r)
