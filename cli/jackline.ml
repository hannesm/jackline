open Lwt
open React

let () =
  Lwt_main.run (
    ignore (LTerm_inputrc.load ());
    Tls_lwt.rng_init () >>= fun () ->

    Lazy.force LTerm.stdout >>= fun term ->

    let cfgdir =
      let home = Unix.getenv "HOME" in
      let cfgdir = Filename.concat home ".config" in
      Xmpp_callbacks.xmpp_config cfgdir
    in

    (match Sys.argv with
     | [| _ ; "-f" ; dir |] -> return dir
     | [| _ |] -> return cfgdir
     | _ -> fail (Invalid_argument ("Usage: " ^ Sys.argv.(0) ^ " [-f config_directory (defaults to " ^ cfgdir ^ ")]"))
    ) >>= fun cfgdir ->

    Xmpp_callbacks.load_config cfgdir >>= ( function
     | None ->
       Cli_config.configure term () >>= fun config ->
       Xmpp_callbacks.dump_config cfgdir config >|= fun () ->
       config
     | Some cfg -> return cfg ) >>= fun config ->

    Xmpp_callbacks.load_users cfgdir >>= fun (users) ->

    let history = LTerm_history.create [] in
    let user = User.find_or_add config.Config.jid users in
    let session = User.ensure_session config.Config.jid config.Config.otr_config user in
    let state = Cli_state.empty_ui_state user session users in
    let n, s_n = S.create (Unix.localtime (Unix.time ()), "nobody", "nothing") in
    Cli_client.loop config term history state None n s_n >>= fun state ->
    Xmpp_callbacks.dump_users cfgdir state.Cli_state.users
  )
