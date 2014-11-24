open Lwt

open Xmpp_callbacks

let before_exit ctx wake () =
  Printf.printf "just about to finish up\n%!" ;
  (* save config to disk *)
  let cfgdir = Glib.get_user_config_dir () in
  Lwt.ignore_result (dump_data cfgdir ctx) ;
  Lwt.wakeup wake ()

open GText

let add_text (t : buffer) (s : string) =
  let iter = t#get_iter `END in
  t#insert ~iter s

let ctx = { config = Config.empty ; users = [] }

let config_dialog parent () =
  let open Config in
  let config = ctx.config in
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
    anchor#set_filter (GFile.filter ~patterns:["*.pem" ; "*.crt"] ()) ;
    anchor
  in
  ignore (trust_anchor#set_filename config.trust_anchor) ;
  let otr =
    let open Otr.State in
    let hbox = GPack.hbox ~packing:dialog#vbox#add () in
    let otr_cfg = config.otr_config in
    let my_dsa = ref otr_cfg.dsa in
    let fingerprint () =
      let hex x = match Hex.of_string ~pretty:true (Cstruct.to_string x) with
          `Hex e -> e
      in
      if !my_dsa = dsa0 then
        "No OTR"
      else
        hex (Otr.Crypto.OtrDsa.priv_fingerprint !my_dsa)
    in
    ignore (GMisc.label ~text:"OTR Fingerprint" ~packing:hbox#add ()) ;
    let fp = GMisc.label ~text:(fingerprint ()) ~packing:hbox#add () in
    let gen = GButton.button ~label:"generate" ~packing:hbox#add () in
    let gen_cb () =
      my_dsa := Nocrypto.Dsa.generate `Fips1024 ;
      fp#set_text (fingerprint ())
    in
    ignore (gen#connect#clicked ~callback:gen_cb) ;
    let ps =
      let hbox = GPack.hbox ~packing:dialog#vbox#add () in
      let bs = List.map
          (fun p ->
             let label = policy_to_string p in
             GButton.toggle_button ~label ~packing:hbox#add ())
          policies
      in
      List.combine bs policies
    in
    let vs =
      let hbox = GPack.hbox ~packing:dialog#vbox#add () in
      let bs = List.map
          (fun v ->
             let label = version_to_string v in
             GButton.toggle_button ~label ~packing:hbox#add ())
          versions
      in
      List.combine bs versions
    in
    let toggle lst items =
      List.iter (fun (b, v) -> if List.mem v items then b#set_active true) lst
    in
    toggle ps otr_cfg.policies ;
    toggle vs otr_cfg.versions ;
    let res () =
      let dsa = !my_dsa in
      if dsa = dsa0 then assert false ;
      let toggled lst =
        List.fold_left (fun acc (x, y) -> if x#active then y :: acc else acc)
          [] lst
      in
      let policies = toggled ps in
      let versions = toggled vs in
      { policies ; versions ; dsa }
    in
    res
  in
  let ok = GButton.button ~stock:`OK ~packing:dialog#action_area#add () in
  let ok_cb () =
    (* there should be some sort of validation *)
    let jid = JID.of_string jid#text in
    let port = int_of_string port#text in
    let password = password#text in
    let trust_anchor = match trust_anchor#filename with
      | None -> assert false
      | Some x -> x
    in
    let otr_config = otr () in
    let config = { version = 0 ; jid ; port ; password ; trust_anchor ; otr_config } in
    Printf.printf "returning from config dialog with:\n - config: %s\n%!"
      (Sexplib.Sexp.to_string_hum (Config.sexp_of_t config)) ;
    ctx.config <- config ;
    let cfgdir = Glib.get_user_config_dir () in
    Lwt.ignore_result (dump_config cfgdir config) ;
    Printf.printf "ctx.config now: %s\n%!"
      (Sexplib.Sexp.to_string_hum (Config.sexp_of_t ctx.config)) ;
    dialog#destroy ()
  in
  ignore (ok#connect#clicked ~callback:ok_cb) ;
  let canc = GButton.button ~stock:`CANCEL ~packing:dialog#action_area#add () in
  ignore (canc#connect#clicked ~callback:dialog#destroy) ;
  dialog#show () ;
  ()

let () = Lwt_main.run (
    ignore (GMain.init ());
    Lwt_glib.install ();

    (* configuration *)
    let cfgdir = Glib.get_user_config_dir () in
    init cfgdir >>= fun { config ; users } ->
    ctx.config <- config ;
    ctx.users <- users ;

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
    ignore (factory#add_item "Config" ~callback:(config_dialog window));
    ignore (factory#add_item "Connect" ~callback:(connect (ctx, callbacks)));
    ignore (factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback:(before_exit ctx wakener));

    (* Display the windows and enter Gtk+ main loop *)
    window#add_accel_group accel_group;

    (* Quit when the window is closed. *)
    ignore (window#connect#destroy (before_exit ctx wakener));

    (* Show the window. *)
    window#show ();

    (* Wait for it to be closed. *)
    waiter
  )
