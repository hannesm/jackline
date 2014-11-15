
open Sexplib
open Sexplib.Conv

type t = {
  username : string ;
  server : string ;
  password : string ;
  trust_anchor : string ;
  otr_config : Otr.State.config ;
} with sexp

let load_config bytes =
  t_of_sexp (Sexp.of_string bytes)

let store_config t =
  Sexp.to_string_mach (sexp_of_t t)

