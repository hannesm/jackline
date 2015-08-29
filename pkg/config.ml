#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

module Config = struct
  include Config_default
  let vars =
    [ "NAME", "jackline";
      "VERSION", Git.describe () ;
      "MAINTAINER", "Hannes Mehnert <hannes\\@mehnert.org>" ]
end
