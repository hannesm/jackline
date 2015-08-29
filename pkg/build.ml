#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let cmdliner = Env.bool "cmdliner"

let () = Pkg.describe "jackline" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.library "src/config";
    Pkg.lib ~exts:Exts.library "src/escape";
    Pkg.lib ~exts:Exts.library "src/persistency";
    Pkg.lib ~exts:Exts.library "src/user";
    Pkg.lib ~exts:Exts.library "src/utils";
    Pkg.lib ~exts:Exts.library "src/xmpp_callbacks";
    Pkg.lib ~exts:Exts.library "src/xmpp_connection";
    Pkg.lib ~exts:Exts.library "cli/cli_client";
    Pkg.lib ~exts:Exts.library "cli/cli_commands";
    Pkg.lib ~exts:Exts.library "cli/cli_config";
    Pkg.lib ~exts:Exts.library "cli/cli_state";
    Pkg.bin ~auto:true "bin/jackline";
    Pkg.doc "README.md"; ]
