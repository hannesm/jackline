
open Lwt

open LTerm_style
open LTerm_text
open LTerm_geom
open CamomileLibraryDyn.Camomile
open React

let rec take_rev x l acc =
  match x, l with
  | 0, _ -> acc
  | n, [] -> acc
  | n, x :: xs -> take_rev (pred n) xs (x :: acc)

let rec take_fill ?(neutral = "") x l acc =
  match x, l with
  | 0, _     -> List.rev acc
  | n, x::xs -> take_fill ~neutral (pred n) xs (x::acc)
  | n, []    -> take_fill ~neutral (pred n) [] (neutral::acc)

let rec pad_l neutral x l =
  match x - (List.length l) with
  | 0 -> l
  | d when d > 0 ->  pad_l neutral x (neutral :: l)
  | d -> assert false

let pad x s =
  match x - (String.length s) with
  | 0 -> s
  | d when d > 0 -> s ^ (String.make d ' ')
  | d (* when d < 0 *) -> String.sub s 0 x

type ui_state = {
  user : User.user ; (* set initially *)
  session : User.session ; (* set initially *)
  mutable log : (Unix.tm * string * string) list ; (* set by xmpp callbacks -- should be time * string list *)
  active_chat : User.user option ; (* modified by user (scrolling through buddies) *)
  users : User.users ; (* extended by xmpp callbacks *)
  notifications : User.user list ; (* or a set? adjusted once messages drop in, reset when chat becomes active *)
  (* events : ?? list ; (* primarily subscription requests - anything else? *) *)
}

let empty_ui_state user session users = {
  user ;
  session ;
  log = [] ;
  active_chat = Some user ;
  users ;
  notifications = []
}

let make_prompt size time network state =
  let tm = Unix.localtime time in

  (* network should be an event, then I wouldn't need a check here *)
  (if List.length state.log = 0 || List.hd state.log <> network then
     state.log <- (network :: state.log)) ;

  let print (lt, from, msg) =
    let time = Printf.sprintf "[%02d:%02d:%02d] " lt.Unix.tm_hour lt.Unix.tm_min lt.Unix.tm_sec in
    time ^ from ^ ": " ^ msg
  in
  let logs =
    let entries = take_rev 6 state.log [] in
    let ent = List.map print entries in
    let msgs = pad_l "" 6 ent in
    String.concat "\n" msgs
  in

  let session = state.session in
  let status = User.presence_to_string session.User.presence in
  let jid = state.user.User.jid ^ "/" ^ session.User.resource in

  let main_size = size.rows - 6 (* log *) - 3 (* status + readline *) in
  assert (main_size > 0) ;

  let buddy_width = 24 in

  let buddies =
    User.Users.fold (fun k u acc ->
        let session = User.good_session u in
        let s = match session with
          | None -> `Offline
          | Some s -> s.User.presence
        in
        let fg = match session with
          | None -> black
          | Some x -> match Otr.State.(x.User.otr.state.message_state) with
            | `MSGSTATE_ENCRYPTED _ -> lgreen
            | _ -> black
        in
        let f, t =
          if u = state.user then
            ("{", "}")
          else
            User.subscription_to_chars u.User.subscription
        in
        let bg = match state.active_chat with
          | None -> white
          | Some x when x = u -> lcyan
          | Some _ -> white
        in
        let item =
          let data = Printf.sprintf " %s%s%s %s" f (User.presence_to_char s) t k in
          pad buddy_width data
        in
        [B_fg fg ; B_bg bg ; S item ; E_bg ; E_fg ] :: acc)
      state.users []
  in
  (* handle overflowings: text might be too long for one row *)

  let buddylist =
    (* let lst = take_fill ~neutral:([ S (pad buddy_width "") ]) main_size buddies [] in *)
    let lst = take_rev main_size buddies [] in
    let lst = pad_l [ S (String.make buddy_width ' ') ] main_size lst in
    List.map (fun x -> x @ [ B_fg lcyan ; S (Zed_utf8.singleton (UChar.of_int 0x2502)) ; E_fg ; S "\n" ]) lst
  in
  let hline =
    (Zed_utf8.make buddy_width (UChar.of_int 0x2500)) ^
    (Zed_utf8.singleton (UChar.of_int 0x2534)) ^
    (Zed_utf8.make (size.cols - (succ buddy_width)) (UChar.of_int 0x2500))
  in

  eval (
    List.flatten buddylist @ [

    B_fg lcyan;
    S hline ;
    E_fg;
    S "\n" ;

    S logs ;
    S "\n" ;

    B_bold true;

    B_fg lcyan;
    S"─( ";
    B_fg lmagenta; S(Printf.sprintf "%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min); E_fg;
    S" )─< ";
    B_fg lblue; S jid; E_fg;
    S" >─";
    S(Zed_utf8.make
        (size.cols - 22 - String.length jid - String.length status)
        (UChar.of_int 0x2500));
    S"[ ";
    B_fg (if session.User.presence = `Offline then lred else lgreen); S status; E_fg;
    S" ]─";
    E_fg;
    S"\n";

    E_bold;
  ])

let commands =
  [ "/connect" ; "/add" ; "/status" ; "/quit" ]

let time =
  let time, set_time = S.create (Unix.time ()) in
  (* Update the time every 60 seconds. *)
  ignore (Lwt_engine.on_timer 60.0 true (fun _ -> set_time (Unix.time ())));
  time

class read_line ~term ~network ~history ~state ~completions = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method completion =
    let prefix  = Zed_rope.to_string self#input_prev in
    let completions = List.filter (fun f -> Zed_utf8.starts_with f prefix) completions in
    self#set_completion 0 (List.map (fun f -> (f, " ")) completions)

  method show_box = false

  initializer
    self#set_prompt (S.l3 (fun size time network -> make_prompt size time network state)
                       self#size time network)
end

let rec loop (config : Config.t) term hist state network s_n =
  let completions = commands in
  let history = LTerm_history.contents hist in
  match_lwt
    try_lwt
      lwt command = (new read_line ~term ~history ~completions ~state ~network)#run in
      return (Some command)
    with
      | Sys.Break -> return None
      | LTerm_read_line.Interrupt -> return (Some "/quit")
  with
   | Some command when (String.length command > 0) && String.get command 0 = '/' ->
       LTerm_history.add hist command;
       let cmd =
         let ws = try String.index command ' ' with Not_found -> String.length command in
         String.sub command 1 (pred ws)
       in
       let cont = match String.trim cmd with
         | "quit" -> false
         | "connect" ->
           let otr_config = config.Config.otr_config in
           let cb jid msg =
               let now = Unix.localtime (Unix.time ()) in
               s_n (now, jid, msg)
           in
           let (user_data : Xmpp_callbacks.user_data) = Xmpp_callbacks.({
             otr_config ;
             users = state.users ;
             received = cb
           }) in
           Xmpp_callbacks.connect config user_data () ;
           true
         | _ -> Printf.printf "NYI" ; true
       in
       if cont then
         loop config term hist state network s_n
       else
         return state
   | Some message ->
       LTerm_history.add hist message;
       loop config term hist state network s_n
   | None -> loop config term hist state network s_n

let () =
  Lwt_main.run (
    ignore (LTerm_inputrc.load ());
    (* look for -f command line flag *)
    Lwt_unix.getlogin () >>= fun user ->
    Lwt_unix.getpwnam user >>= fun pw_ent ->
    let cfgdir =
      let home = pw_ent.Lwt_unix.pw_dir in
      Filename.concat home ".config"
    in
    Xmpp_callbacks.init cfgdir >>= fun (config, users) ->
    let history = LTerm_history.create [] in
    let user = User.find_or_add config.Config.jid users in
    let session = User.ensure_session config.Config.jid config.Config.otr_config user in
    let state = empty_ui_state user session users in
    let n, s_n = S.create (Unix.localtime (Unix.time ()), "nobody", "nothing") in
    Lazy.force LTerm.stdout >>= fun term ->
    loop config term history state n s_n >>= fun state ->
    Printf.printf "now dumping state %d\n%!" (User.Users.length state.users) ;
    print_newline () ;
    (* dump_users cfgdir x.users *)
    return ()
  )
