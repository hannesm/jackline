open Lwt

let before_exit wake =
  Printf.printf "just about to finish up\n%!" ;
  Lwt.wakeup wake

open GText

let add_text (t : buffer) (s : string) =
  let iter = t#get_iter `END in
  t#insert ~iter s

open Xmpp_callbacks

let config_dialog config parent () =
  let open Config in
  Printf.printf "current jid %s\n" (JID.string_of_jid config.jid) ;
  Printf.printf "server port %d\n" config.port ;
  Printf.printf "password %s\n" config.password ;
  Printf.printf "trust anchor %s\n" config.trust_anchor ;
  (match config.otr_config with
   | None -> Printf.printf "no OTR\n"
   | Some x -> Printf.printf "some OTR\n") ;
  let dialog = GWindow.dialog ~title:"barf" ~resizable:true ~modal:true ~parent () in
  let ok = GButton.button ~stock:`OK ~packing:dialog#action_area#add () in
  dialog#show () ;
  ()

let () = Lwt_main.run (
    ignore (GMain.init ());
    Lwt_glib.install ();

    (* configuration *)
    init () >>= fun data ->

    (* Thread which is wakeup when the main window is closed. *)
    let waiter, wakener = Lwt.wait () in

    let window = GWindow.window ~width:320 ~height:240 ~title:"XMPP Client" () in
    let vbox = GPack.vbox ~packing:window#add () in

    (* Menu bar *)
    let menubar = GMenu.menu_bar ~packing:vbox#pack () in
    let factory = new GMenu.factory menubar in
    let accel_group = factory#accel_group in
    let menu = factory#add_submenu "Action" in

    (* Multi-line text editor *)
    let textbuf = GText.buffer () in
    let textview = GText.view ~buffer:textbuf ~editable:false ~packing:vbox#add () in

    (* Button *)
    let button = GButton.button ~label:"Push me!" ~packing:vbox#add () in
    ignore (button#connect#clicked ~callback:(fun () -> Printf.printf "blablabla\n%!"));

    let callbacks = {
      received = add_text textbuf
    } in
    (* Action menu *)
    let factory = new GMenu.factory menu ~accel_group in
    ignore (factory#add_item "Config" ~callback:(config_dialog data.config window));
    ignore (factory#add_item "Connect" ~callback:(connect (data, callbacks)));
    ignore (factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback:(before_exit wakener));

    (* Display the windows and enter Gtk+ main loop *)
    window#add_accel_group accel_group;

    (* Quit when the window is closed. *)
    ignore (window#connect#destroy (Lwt.wakeup wakener));

    (* Show the window. *)
    window#show ();

    (* Wait for it to be closed. *)
    waiter
  )
