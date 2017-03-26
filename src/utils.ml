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
  let dft buf = if !unicode then Uutf.Buffer.add_utf_8 buf (Uchar.of_int 0xFFFD) in
  let rec loop d buf = match Uutf.decode d with
    | `Await -> Buffer.contents buf
    | `End -> Buffer.contents buf
    | `Malformed _ -> dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x000A -> (* newline *) Uutf.Buffer.add_utf_8 buf (Uchar.of_int 0x000A) ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x0009 -> (* tab -> 4 spaces *) Uutf.Buffer.add_utf_8 buf (Uchar.of_int 0x0020) ; Uutf.Buffer.add_utf_8 buf (Uchar.of_int 0x0020) ; Uutf.Buffer.add_utf_8 buf (Uchar.of_int 0x0020) ; Uutf.Buffer.add_utf_8 buf (Uchar.of_int 0x0020) ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x007F -> (* DEL *) dft buf ; loop d buf
    (* See https://en.wikipedia.org/wiki/Unicode_control_characters / https://en.wikipedia.org/wiki/Bi-directional_text *)
    | `Uchar x when Uchar.to_int x = 0x200E -> (* left-to-right *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x200F -> (* right-to-left *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x202A -> (* left-to-right embedding *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x202D -> (* left-to-right override *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x202B -> (* right-to-left embedding *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x202E -> (* right-to-left override *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x202C -> (* pop directional format *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x2066 -> (* l-t-r isolate *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x2067 -> (* r-t-l isolate *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x2068 -> (* first strong isolate *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x2069 -> (* pop directional isolate *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x2028 -> (* line separator *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x = 0x2029 -> (* page separator *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x < 0x20 -> (* other control characters *) dft buf ; loop d buf
    | `Uchar x when Uchar.to_int x >= 0x0080 && Uchar.to_int x <= 0x009F ->
      (* ctrl chars used in conjunction with ISO 8859 character sets (C0/C1) *)
      dft buf ; loop d buf

    | `Uchar x ->
      let c = if !unicode then x else if Uchar.to_int x > 0xff then Uchar.of_int 0x3f else x in
      Uutf.Buffer.add_utf_8 buf c ; loop d buf
  in
  let nln = `Readline (Uchar.of_int 0x000A) in
  loop (Uutf.decoder ~nln ~encoding:`UTF_8 (`String txt)) (Buffer.create (String.length txt))

let version = "%%VERSION_NUM%%"
