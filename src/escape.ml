
let entities =
  [ "&", "&amp;" ;
    "'", "&apos;" ;
    ">", "&gt;" ;
    "<", "&lt;" ;
    "\"", "&quot;" ]

let escape str =
  let escape1 sep replacement data =
    let pieces = Astring.String.cuts ~sep data in
    String.concat replacement pieces
  in
  List.fold_left
    (fun s (c, r) -> escape1 c r s)
    str entities

let unescape str =
  let unescape1 sep replacement data =
    let pieces = Astring.String.cuts ~sep data in
    String.concat replacement pieces
  in
  List.fold_left (fun s (repl, on) -> unescape1 on repl s)
    str (List.rev entities)

let rec strip_tags str =
  match Astring.String.cut ~sep:"<" str with
  | None -> str
  | Some (l, r) ->
     match Astring.String.cut ~sep:">" r with
     | None -> str (* unmatched < *)
     | Some (_, r) -> l ^ (strip_tags r)
