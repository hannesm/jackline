#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () = Pkg.describe "jackline" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.bin ~auto:true "bin/jackline";
    Pkg.doc "README.md"; ]
