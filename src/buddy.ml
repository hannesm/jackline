
type buddy = [ `User of User.user | `Room of Muc.groupchat ]
type resource = [ `Session of User.session | `Member of Muc.member ]

type color = [ `Default | `Good | `Bad ]

let bare = function
  | `User x -> x.User.bare_jid
  | `Room x -> x.Muc.room_jid

let compare_buddy a b =
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
  | `Room r ->
     Utils.option `Offline (fun m ->  m.Muc.presence) (Muc.self_member r)
  | `User x ->
     Utils.option `Offline (fun s -> s.User.presence) (User.active_session x)

let expanded = function
  | `Room r -> r.Muc.expand
  | `User u -> u.User.expand

let expand = function
  | `Room r -> `Room { r with Muc.expand = not r.Muc.expand }
  | `User u -> `User { u with User.expand = not u.User.expand }

let all_resources = function
  | `Room r -> List.map
                 (fun m -> `Member m)
                 (Muc.sorted_members r)
  | `User u -> List.map
                 (fun s -> `Session s)
                 (User.sorted_sessions u)

let active = function
  | `Room r -> None
  | `User u -> Utils.option None (fun s -> Some (`Session s)) (User.active_session u)

let full_jid buddy r =
  (bare buddy, resource r)

let jid buddy resource =
  Utils.option
    (`Bare (bare buddy))
    (fun r -> `Full (full_jid buddy r))
    resource

let active_resources tst = function
  | `Room r ->
     List.map
       (fun m -> `Member m)
       (List.filter
          (fun m -> m.Muc.presence <> `Offline || tst (`Full (r.Muc.room_jid, m.Muc.nickname)))
          (Muc.sorted_members r))
  | `User u ->
     List.map
       (fun s -> `Session s)
       (List.filter
          (fun s -> s.User.presence <> `Offline || tst (`Full (u.User.bare_jid, s.User.resource)))
          (User.sorted_sessions u))

let status = function
  | `Member m -> m.Muc.status
  | `Session s -> s.User.status

let bare_resource_info r =
  let pre = presence r
  and sta = status r
  and res = resource r
  in
  Printf.sprintf "%s: %s %s"
                 res
                 (User.presence_to_string pre)
                 (Utils.option "" (fun s -> " - " ^ s) sta)

let preserve_messages = function
  | `User u -> u.User.preserve_messages
  | `Room r -> r.Muc.preserve_messages

let set_preserve_messages b newval =
  match b with
  | `User u -> `User { u with User.preserve_messages = newval }
  | `Room r -> `Room { r with Muc.preserve_messages = newval }

let name = function
  | `User u -> u.User.name
  | `Room r -> Some (r.Muc.my_nick)

let info = function
  | `User u -> User.info u
  | `Room r -> Muc.info r

let messages = function
  | `User u -> u.User.message_history
  | `Room r -> r.Muc.message_history

let reset = function
  | `User u -> `User (User.reset_user u)
  | `Room r -> `Room (Muc.reset_room r)

let clear_messages = function
  | `User u -> `User { u with User.message_history = [] }
  | `Room r -> `Room { r with Muc.message_history = [] }

let saved_input_buffer = function
  | `User u -> u.User.saved_input_buffer
  | `Room r -> r.Muc.saved_input_buffer

let set_saved_input_buffer buddy str =
  match buddy with
  | `User u -> `User { u with User.saved_input_buffer = str }
  | `Room r -> `Room { r with Muc.saved_input_buffer = str }

let new_message buddy message =
  match buddy with
  | `User u -> `User (User.new_message u message)
  | `Room r -> `Room (Muc.new_message r message)

let readline_history = function
  | `User u -> u.User.readline_history
  | `Room r -> r.Muc.readline_history

let add_readline_history b h =
  match b with
  | `User u -> `User { u with User.readline_history = h :: u.User.readline_history }
  | `Room r -> `Room { r with Muc.readline_history = h :: r.Muc.readline_history }

let color (b : buddy) (r : resource option) =
  match b, r with
  | `User _, None -> `Default
  | `User _, Some (`Session s) -> if User.(encrypted s.otr) then `Good else `Bad
  | `Room _, _ -> `Default

let marshal_history buddy =
  let open Sexplib.Conv in
  if preserve_messages buddy then
    let new_msgs =
      List.filter (fun m ->
        match m.User.direction, m.User.persistent with
        | `Local _, _    -> false
        | _       , true -> false
        | _              -> true)
        (messages buddy)
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

let load_history directory buddy =
  let bare = bare buddy in
  let file = Filename.concat directory (Xjid.bare_jid_to_string bare) in
  let msgs = load_history_hlp (`Bare bare) file in
  set_history buddy msgs

let store = function
  | `User u -> User.store_user u
  | `Room r -> Muc.store_room r

module StringHash =
  struct
    type t = Xjid.bare_jid
    let equal = Xjid.bare_jid_equal
    let hash = Hashtbl.hash
  end

module Buddies = Hashtbl.Make(StringHash)
type buddies = buddy Buddies.t

let create () = Buddies.create 100
let length = Buddies.length

let fold = Buddies.fold
let iter = Buddies.iter

let find_buddy t id = try Some (Buddies.find t id) with Not_found -> None
let replace_buddy buddies buddy = Buddies.replace buddies (bare buddy) buddy
let remove = Buddies.remove

let find_room t id = match find_buddy t id with
  | Some (`Room r) -> Some r
  | _ -> None

let find_user t id = match find_buddy t id with
  | Some (`User u) -> Some u
  | _ -> None

let replace_room t r = replace_buddy t (`Room r)
let replace_user t r = replace_buddy t (`User r)
