let option none some = function
  | None   -> none
  | Some x -> some x

let today () =
  let now = Unix.time () in
  let day = 24 * 60 * 60 in
  Int64.(to_float (sub (of_float now) (rem (of_float now) (of_int day))))

let rec find_index id i = function
  | []                              -> 0
  | x::_ when Xjid.jid_matches id x -> i
  | _::xs                           -> find_index id (succ i) xs

let version = "%%VERSION%%"
