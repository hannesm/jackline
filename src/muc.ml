open Astring

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
  | _ -> assert false
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

let affiliation_to_string = function
  | `Owner -> "owner"
  | `Admin -> "admin"
  | `Member -> "member"
  | `Outcast -> "outcast"
  | `None -> "none"

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

let role_to_string = function
  | `Moderator -> "moderator"
  | `Participant -> "participant"
  | `Visitor -> "visitor"
  | `None -> "none"

type member = {
  jid : Xjid.t option ;
  nickname : string ;
  affiliation : affiliation ;
  role : role ;
  presence : User.presence ;
  status : string option ;
}

let new_member nickname ?(jid=None) affiliation role presence status =
  { jid ; nickname ; affiliation ; role ; presence ; status }

type groupchat = {
  room_jid : Xjid.bare_jid ; (* persistent *)
  topic : string option ;
  my_nick : string ; (* persistent *)
  members : member list ;
  features : features list ;
  expand : bool ;
  preserve_messages : bool ;
  message_history : User.message list ; (* persistent if preserve_messages *)
  input_buffer : (Uchar.t list * Uchar.t list) ;
  readline_history : string list ;
  history_position : int ;
  autojoin : bool ;
  password : string option ; (* persistent *)
  alias : string option ; (* persistent *)
}

let new_room ~jid ?password ?topic ?alias ~my_nick ?(members=[]) ?(features=[]) ?(autojoin = true) ?(preserve_messages=false) () =
  { room_jid = jid ; password ; topic ; alias ; my_nick ; members ; features ; expand = false ; preserve_messages ; message_history = [] ; input_buffer = ([], []) ; readline_history = [] ; history_position = 0 ; autojoin }

let short_member_info m =
  Printf.sprintf " %s %s (role: %s) (affiliation: %s)" (User.presence_to_char m.presence) m.nickname (role_to_string m.role) (affiliation_to_string m.affiliation)

let topic_to_s r =
  Utils.option
    "no topic"
    (fun x -> String.concat ~sep:" " (String.cuts ~sep:"\n" x))
    r.topic

let info r =
  let topic = topic_to_s r
  and features = "features: " ^ String.concat ~sep:"," (List.map feature_to_string r.features)
  and members = List.filter (fun m -> m.role <> `None ) r.members
                |> List.map short_member_info
  in
  [ topic ; features ] @ members

let member_info m =
  let jid = Utils.option [] (fun x -> ["real jid: " ^ Xjid.jid_to_string x]) m.jid
  and aff = affiliation_to_string m.affiliation
  and role = role_to_string m.role
  and nick = m.nickname
  in
  [ "nickname: " ^ nick ] @ jid @ [ "affiliation: " ^ aff ; "role: " ^ role ]

let member r = function
  | `Bare _ -> None
  | `Full (_, nick) ->
     try Some (List.find (fun x -> x.nickname = nick) r.members)
     with Not_found -> None

let reset_room r =
  let members =
    List.map (fun m -> { m with presence = `Offline }) r.members
  in
  { r with members }

let sorted_members r =
  List.sort (fun a b -> String.compare a.nickname b.nickname) r.members

let self_member r = member r (`Full (r.room_jid, r.my_nick))

let new_message room message =
  { room with message_history = message :: room.message_history }

open Sexplib
open Sexplib.Conv

let db_version = 1

let t_of_sexp t _version =
  match t with
  | Sexp.List l ->
    (match
       List.fold_left (fun (room_jid, my_nick, preserve_messages, autojoin, password, alias) v -> match v with
           | Sexp.List [ Sexp.Atom "room_jid" ; jabberid ] ->
             assert (room_jid = None);
             let room_jid = Xjid.bare_jid_of_sexp jabberid in
             (Some room_jid, my_nick, preserve_messages, autojoin, password, alias)
           | Sexp.List [ Sexp.Atom "my_nick" ; Sexp.Atom nick ] ->
             assert (my_nick = None);
             (room_jid, Some nick, preserve_messages, autojoin, password, alias)
           | Sexp.List [ Sexp.Atom "preserve_messages" ; hf ] ->
             assert (preserve_messages = None) ;
             let preserve_messages = bool_of_sexp hf in
             (room_jid, my_nick, Some preserve_messages, autojoin, password, alias)
           | Sexp.List [ Sexp.Atom "autojoin" ; hf ] ->
             assert (autojoin = None) ;
             let autojoin = bool_of_sexp hf in
             (room_jid, my_nick, preserve_messages, Some autojoin, password, alias)
           | Sexp.List [ Sexp.Atom "password" ; pw ] ->
             assert (password = None) ;
             let pw = option_of_sexp string_of_sexp pw in
             (room_jid, my_nick, preserve_messages, autojoin, pw, alias)
           | Sexp.List [ Sexp.Atom "alias" ; al ] ->
             assert (alias = None) ;
             let al = option_of_sexp string_of_sexp al in
             (room_jid, my_nick, preserve_messages, autojoin, password, Some al)
           | _ -> assert false)
         (None, None, None, None, None, None) l
     with
     | Some room_jid, Some my_nick, Some preserve_messages, Some autojoin, password, Some alias ->
       Some (new_room ~jid:room_jid ?password ?alias ~my_nick ~preserve_messages ~autojoin ())
     | Some room_jid, Some my_nick, Some preserve_messages, Some autojoin, password, None ->
       Some (new_room ~jid:room_jid ?password ~my_nick ~preserve_messages ~autojoin ())
     | _ -> None )
  | _ -> None

let record kvs =
  Sexp.List (List.map (fun (k, v) -> (Sexp.List [Sexp.Atom k; v])) kvs)

let sexp_of_t t =
  record [
    "room_jid"          , Xjid.sexp_of_bare_jid t.room_jid ;
    "my_nick"           , sexp_of_string t.my_nick ;
    "preserve_messages" , sexp_of_bool t.preserve_messages ;
    "autojoin"          , sexp_of_bool t.autojoin ;
    "password"          , sexp_of_option sexp_of_string t.password ;
    "alias"              , sexp_of_option sexp_of_string t.alias ;
  ]

let marshal_room room =
  let sexp = sexp_of_t room in
  Some Sexp.(List [ sexp_of_int db_version ; sexp ])

let load_room bytes =
  try (match Sexp.of_string bytes with
       | Sexp.List [ ver ; user ] ->
          let version = int_of_sexp ver in
          (try t_of_sexp user version with _ -> None)
       | _ -> None)
    with _ -> None

let oneline room =
  let presence = Utils.option `Offline (fun x -> x.presence) (self_member room)
  and size = List.length (List.filter (fun m -> m.presence <> `Offline) room.members)
  in
  let name =
    match room.alias with
    | Some alias -> alias
    | None -> Xjid.bare_jid_to_string room.room_jid
  in
  Printf.sprintf
    "%s(%d) %s - %s"
    (User.presence_to_char presence)
    size
    name
    (topic_to_s room)

let oneline_with_member _ m =
  let presence = m.presence
  and role = m.role
  in
  Printf.sprintf
    " %s%s %s"
    (User.presence_to_char presence)
    (role_to_char role)
    m.nickname
