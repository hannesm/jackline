
let entities =
  [ '&', "&amp;" ;
    '\'', "&apos;" ;
    '>', "&gt;" ;
    '<', "&lt;" ;
    '"', "&quot;" ]

let escape str =
  let escape1 char replacement data =
    let pieces = Stringext.split data ~on:char in
    String.concat replacement pieces
  in
  List.fold_left
    (fun s (c, r) -> escape1 c r s)
    str entities

let rec cutall str on =
  match Stringext.cut str ~on with
  | None -> [str]
  | Some (l, r) -> l :: (cutall r on)

let unescape str =
  let unescape1 on replacement data =
    let pieces = cutall data on in
    String.concat replacement pieces
  in
  List.fold_left
    (fun s (repl, on) ->
     let repl = String.make 1 repl in
     unescape1 on repl s)
    str (List.rev entities)

let rec strip_tags str =
  match Stringext.cut str ~on:"<" with
  | None -> str
  | Some (l, r) ->
     match Stringext.cut r ~on:">" with
     | None -> str (* unmatched < *)
     | Some (_, r) -> l ^ (strip_tags r)
