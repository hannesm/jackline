#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () = Pkg.describe "jackline" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:[".cmo"] "src/xmpp_client";
    Pkg.lib ~exts:[".cmo"] "cli/xmpp_client_cli";
    Pkg.bin ~auto:true "bin/jackline";
    Pkg.doc "README.md"; ]
