open Lwt

let before_exit wake =
  Printf.printf "just about to finish up\n%!" ;
  Lwt.wakeup wake

open GText

let add_text (t : buffer) (s : string) =
  let iter = t#get_iter `END in
  t#insert s

let () = Lwt_main.run (
    ignore (GMain.init ());
    Lwt_glib.install ();

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

    (* Action menu *)
    let factory = new GMenu.factory menu ~accel_group in
    ignore (factory#add_item "Connect" ~callback:(Xmpp_callbacks.connect (add_text textbuf)));
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
