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
  let dialog = GWindow.dialog ~title:"Configuration" ~resizable:true ~modal:true ~parent () in
  let labelled_entry name value =
    let hbox = GPack.hbox ~packing:dialog#vbox#add () in
    ignore (GMisc.label ~text:name ~packing:hbox#add ()) ;
    GEdit.entry ~text:value ~packing:hbox#add ()
  in
  let jid = labelled_entry "Jabber ID" (JID.string_of_jid config.jid) in
  let port = labelled_entry "port" (string_of_int config.port) in
  let password = labelled_entry "password" config.password in
  let trust_anchor =
    let hbox = GPack.hbox ~packing:dialog#vbox#add () in
    ignore (GMisc.label ~text:"Trust anchor" ~packing:hbox#add ()) ;
    let anchor = GFile.chooser_button ~action:`OPEN ~packing:hbox#add () in
    anchor#set_filter (GFile.filter ~patterns:["*.pem"] ()) ;
    anchor
  in
  trust_anchor#set_filename config.trust_anchor ;
  let otr =
    let hex x = match Hex.of_string ~pretty:true (Cstruct.to_string x) with `Hex e -> e in
    let hbox = GPack.hbox ~packing:dialog#vbox#add () in
    let text cfg = match cfg.otr_config with
      | None -> "No OTR"
      | Some cfg -> hex (Otr.Crypto.OtrDsa.priv_fingerprint cfg.Otr.State.dsa)
    in
    let fp_label = GMisc.label ~text:"OTR Fingerprint" ~packing:hbox#add () in
    let fp = GMisc.label ~text:(text config) ~packing:hbox#add () in
    let gen = GButton.button ~label:"generate" ~packing:hbox#add () in
    let gen_cb () =
      let dsa = Nocrypto.Dsa.generate `Fips1024 in
      fp#set_text (hex (Otr.Crypto.OtrDsa.priv_fingerprint dsa))
      (* preserve dsa somewhere! *)
    in
    ignore (gen#connect#clicked ~callback:gen_cb) ;
    let policies =
      let hbox = GPack.hbox ~packing:dialog#vbox#add () in
      List.map (fun p ->
          let label = Otr.State.policy_to_string p in
          GButton.toggle_button ~label ~packing:hbox#add ())
        Otr.State.policies
    in
    let versions =
      let hbox = GPack.hbox ~packing:dialog#vbox#add () in
      List.map (fun v ->
          let label = Otr.State.version_to_string v in
          GButton.toggle_button ~label ~packing:hbox#add ())
        Otr.State.versions
    in
    ()
  in
  let ok = GButton.button ~stock:`OK ~packing:dialog#action_area#add () in
  (* ok callback *)
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
