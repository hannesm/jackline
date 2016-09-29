#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "jackline" @@ fun _c ->
  Ok [ Pkg.bin "bin/jackline" ~dst:"jackline" ]
