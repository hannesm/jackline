
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

let active_presence = function
  | `Room r -> presence (`Member (Muc.self_member r))
  | `User x -> Utils.option
                 `Offline
                 (fun s -> presence (`Session s))
                 (User.active_session x)

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

let full_jid buddy resource =
  match buddy, resource with
  | `Room r, `Member m -> (r.Muc.room_jid, m.Muc.nickname)
  | `User u, `Session s -> (u.User.bare_jid, s.User.resource)
  | _ -> assert false

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

let preserve_messages = function
  | `User u -> u.User.preserve_messages
  | `Room r -> r.Muc.preserve_messages

let messages = function
  | `User u -> u.User.message_history
  | `Room r -> r.Muc.message_history

let reset = function
  | `User u -> `User (User.reset_user u)
  | `Room r -> `Room (Muc.reset_room r)

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
    let hist_version = sexp_of_int 2 in
    if List.length new_msgs > 0 then
      let sexps = List.map User.sexp_of_message new_msgs in
      let sexp = Sexplib.Sexp.(List [ hist_version ; List sexps ]) in
      Some (Sexplib.Sexp.to_string_mach sexp)
    else
      None
  else
    None

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
