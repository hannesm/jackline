let option none some = function
  | None   -> none
  | Some x -> some x

let today () =
  let now = Unix.time () in
  let day = 24 * 60 * 60 in
  Int64.(to_float (sub (of_float now) (rem (of_float now) (of_int day))))

let version = "%%VERSION%%"
