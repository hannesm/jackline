(* #use this to setup the interactive environment. *)

#require "erm_xmpp, otr, tls, tls.lwt, lwt, sexplib, sexplib.syntax, hex, nocrypto, lambda-term, react, lwt.syntax";;

#directory "_build/src";;
#load_rec "xmpp_client.cmo";;

#directory "_build/cli";;
#load_rec "xmpp_client_cli.cmo";;
