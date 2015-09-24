
type buddy = [ `User of User.user | `Room of Muc.groupchat ]

let bare = function
  | `User x -> x.User.bare_jid
  | `Room x -> x.Muc.room_jid

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
