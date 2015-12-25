let option none some = function
  | None   -> none
  | Some x -> some x

let today () =
  let now = Ptime_clock.now () in
  let date, _ = Ptime.to_date_time now in
  date

let rec find_index id i = function
  | []                              -> 0
  | x::_ when Xjid.jid_matches id x -> i
  | _::xs                           -> find_index id (succ i) xs

let version = "%%VERSION%%"
