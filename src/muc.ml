
type features = [
  | `Hidden | `Public
  | `Persistent | `Temporary
  | `Password_protected | `Unsecured
  | `Open | `Members_only
  | `Moderated | `Non_moderated
  | `Non_anonymous | `Semi_anonymous
  | `Unavailable_shown | `Non_unavailable_shown
  | `Logged | `Non_logged
]

let feature = function
  | "muc_hidden" -> `Hidden
  | "muc_public" -> `Public
  | "muc_persistent" -> `Persistent
  | "muc_temporary" -> `Temporary
  | "muc_passwordprotected" -> `Password_protected
  | "muc_unsecured" -> `Unsecured
  | "muc_open" -> `Open
  | "muc_membersonly" -> `Members_only
  | "muc_moderated" -> `Moderated
  | "muc_unmoderated" -> `Non_moderated
  | "muc_nonanonymous" -> `Non_anonymous
  | "muc_semianonymous" -> `Semi_anonymous
(*  | "muc_rooms" -> ?? *)

let statuses = [
  (100, "any occupant can see full JID") ;
  (101, "affiliation changed while not in the room") ;
  (102, "room now shows unavailable members") ;
  (103, "room now does not show unavailable members") ;
  (104, "non-privacy-related room configuration change has occurred") ;
  (110, "self-presence") ;
  (170, "logging enabled") ;
  (171, "logging disabled") ;
  (172, "room is now non-anonymous") ;
  (173, "room is now semi-anonymous") ;
  (201, "room has been created") ;
  (210, "nick modified") ;
  (301, "banned from room") ;
  (303, "nick changed") ;
  (307, "kicked from room") ;
  (321, "removed from room because of affiliation change") ;
  (322, "removed from room because of config change: now members-only") ;
  (332, "removed from room because service shutting down")
]

let features_from_status features id =
  let exclude f = List.filter (fun x -> x <> f) features in
  match id with
  | 100 -> `Non_anonymous :: exclude `Semi_anonymous
  | 102 -> `Unavailable_shown :: exclude `Non_unavailable_shown
  | 103 -> `Non_unavailable_shown :: exclude `Unavailable_shown
  | 170 -> `Logged :: exclude `Non_logged
  | 171 -> `Non_logged :: exclude `Logged
  | 172 -> `Non_anonymous :: exclude `Semi_anonymous
  | 173 -> `Semi_anonymous :: exclude `Non_anonymous
  | _ -> features

let feature_to_string = function
  | `Hidden -> "hidden"
  | `Public -> "public"
  | `Persistent -> "persistent"
  | `Temporary -> "temporary"
  | `Password_protected -> "password protected"
  | `Unsecured -> "unsecured"
  | `Open -> "open"
  | `Members_only -> "members only"
  | `Moderated -> "moderated"
  | `Non_moderated -> "not moderated"
  | `Non_anonymous -> "not anonymous"
  | `Semi_anonymous -> "semi anonymous"
  | `Unavailable_shown -> "unavailable shown"
  | `Non_unavailable_shown -> "unavailable not shown"
  | `Logged -> "logged"
  | `Non_logged -> "not logged"

type affiliation = [
  | `Owner
  | `Admin
  | `Member
  | `Outcast
  | `None
]

type role = [
  | `Moderator
  | `Participant
  | `Visitor
  | `None
]

let role_to_char = function
  | `Moderator -> "m"
  | `Participant -> "p"
  | `Visitor -> "v"
  | `None -> "n"

type member = {
  jid : Xjid.t option ;
  nickname : string ;
  affiliation : affiliation ;
  role : role ;
  presence : User.presence ;
  status : string option ;
}

type groupchat = {
  room_jid : Xjid.bare_jid ;
  topic : string ;
  my_nick : string ;
  my_presence : User.presence ;
  members : member list ;
  features : features list ;
  expand : bool ;
  messages : User.message list ;
}
