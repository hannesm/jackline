
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
  let open Astring.String in
  match cut ~sep:"<" str with
  | None -> str
  | Some (l, r) ->
     match cut ~sep:">" r with
     | None -> str (* unmatched < *)
     | Some (data, r) ->
       (* data contains the tag:
            - <font>blablbala</font> --> find /font
            - <balbalbla>klklklkl --> ignore
            - <img src=... /> --> cut
            - <a href=...>aaa</a>
       *)
       if data = "br" || data = "br/" || data = "BR" || data = "BR/" then
         (* special case for stupid clients (pidgin) sending <br>
            Adium uses <BR> for a change..
            (replace <br/> with newline as well) *)
         l ^ "\n" ^ strip_tags r
       else if get data (pred (length data)) = '/' then
         (* <br/> case *)
         l ^ strip_tags r
       else
         let s = match cut ~sep:" " data with
           | None -> data
           | Some (l, _) -> l
         in
         match cut ~sep:("</" ^ s ^ ">") r with
         | None -> l ^ "<" ^ data ^ ">" ^ strip_tags r
         | Some (btw, after) -> l ^ strip_tags btw ^ strip_tags after
