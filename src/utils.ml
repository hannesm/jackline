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

let rec drop x l =
  match x, l with
  | 0, xs      -> xs
  | n, _ :: xs -> drop (pred n) xs
  | _, []      -> []

let version = "%%VERSION%%"
