let option none some = function
  | None   -> none
  | Some x -> some x

let rec find_index p id i = function
  | []               -> 0
  | x::_ when p id x -> i
  | _::xs            -> find_index p id (succ i) xs

let rec drop x l =
  match x, l with
  | 0, xs      -> xs
  | n, _ :: xs -> drop (pred n) xs
  | _, []      -> []

let take_rev x l =
  let rec step x l acc =
    match x, l with
    | 0, _       -> acc
    | _, []      -> acc
    | n, x :: xs -> step (pred n) xs (x :: acc)
  in
  step x l []

let take x l = List.rev (take_rev x l)

let unicode : bool ref = ref true

let validate_utf8 txt =
  let rec loop d buf = match Uutf.decode d with
    | `Await -> Buffer.contents buf
    | `End -> Buffer.contents buf
    | `Malformed _ -> if !unicode then Uutf.Buffer.add_utf_8 buf 0xFFFD; loop d buf
    | `Uchar x when x = 0x0009 || x = 0x000A -> (* tab and newline *) Uutf.Buffer.add_utf_8 buf x ; loop d buf
    | `Uchar 0x000D | `Uchar 0x007F (* carriage return and DEL *)
    (* See https://en.wikipedia.org/wiki/Unicode_control_characters / https://en.wikipedia.org/wiki/Bi-directional_text *)
    | `Uchar 0x200E | `Uchar 0x200F (* left-to-right / right-to-left *)
    | `Uchar 0x202A | `Uchar 0x202D (* left-to-right embedding / override *)
    | `Uchar 0x202B | `Uchar 0x202E (* right-to-left embedding / override *)
    | `Uchar 0x202C  (* pop directional format *)
    | `Uchar 0x2066 | `Uchar 0x2067 (* l-t-r isolate r-t-l isolate *)
    | `Uchar 0x2068 | `Uchar 0x2069 (* first strong isolate / pop directional isolate *)
    | `Uchar 0x2028 | `Uchar 0x2029 (* line separator / page separator *) ->
      if !unicode then Uutf.Buffer.add_utf_8 buf 0xFFFD ; loop d buf
    | `Uchar x when x < 0x20 ->
      (* other control characters *)
      if !unicode then Uutf.Buffer.add_utf_8 buf 0xFFFD ; loop d buf
    | `Uchar x when x >= 0x0080 && x <= 0x009F ->
      (* ctrl chars used in conjunction with ISO 8859 character sets (C0/C1) *)
      if !unicode then Uutf.Buffer.add_utf_8 buf 0xFFFD ; loop d buf

    | `Uchar x -> Uutf.Buffer.add_utf_8 buf x ; loop d buf
  in
  let nln = `Readline 0x000A in
  loop (Uutf.decoder ~nln ~encoding:`UTF_8 (`String txt)) (Buffer.create (String.length txt))

let version = "%%VERSION%%"
