
open Lwt.Infix

open Notty

module T = Notty_lwt.Term

let wrap ?width ?height image =
  let rec doit p f i acc =
    if p i then
      let i = f i in
      doit p f i (i :: acc)
    else
      image :: List.rev acc
  in
  match width, height with
  | Some w, None ->
    let is = doit (fun i -> I.width i > w) (I.hcrop w 0) image [] in
    I.vcat is
  | None, Some h ->
    let is = doit (fun i -> I.height i > h) (I.vcrop 0 h) image [] in
    I.hcat is
  | None, None | Some _, Some _ -> assert false

let rewrap term above below (prefix, inp, inp2) (width, _) =
  let content = wrap ~width I.(prefix <|> inp <|> inp2) in
  let above = I.vcat (List.map (wrap ~width) above) in
  let below = I.vcat (List.map (wrap ~width) below) in
  let image = I.(above <-> content <-> below) in
  T.image term image >>= fun () ->
  let col, row =
    let col = I.width prefix + I.width inp in
    let h =
      let content = wrap ~width I.(prefix <|> inp) in
      I.(height (above <-> content)) in
    let height = if col mod width = 0 then succ h else h in
    (succ (col mod width), height)
  in
  T.cursor term (Some (col, row))

let str_to_char_list str =
  Astring.String.fold_right (fun ch acc -> int_of_char ch :: acc) str []

let read_line ?(above = []) ?(prefix = "") ?default ?(below = []) term =
  let rec go pre post =
    let inp = Array.of_list pre in
    let inp2 = Array.of_list post in
    let iprefix = I.string A.empty prefix
    and iinp = I.uchars A.(st reverse) inp
    and iinp2 = I.uchars A.(st reverse) inp2
    in
    rewrap term above below (iprefix, iinp, iinp2) (T.size term) >>= fun () ->
    Lwt_stream.next (T.events term) >>= function
      | `Key (`Enter, []) ->
         let buf = Buffer.create (Array.length inp + Array.length inp2) in
         Array.iter (Uutf.Buffer.add_utf_8 buf) inp ;
         Array.iter (Uutf.Buffer.add_utf_8 buf) inp2 ;
         Lwt.return (Buffer.contents buf)
      | `Key (`Backspace, []) ->
         (match List.rev pre with
          | [] -> go pre post
          | _::tl -> go (List.rev tl) post)
      | `Key (`Delete, []) ->
         (match post with
          | [] -> go pre post
          | _::tl -> go pre tl)
      | `Key (`Home, []) -> go [] (pre @ post)
      | `Key (`End, []) -> go (pre @ post) []
      | `Key (`Arrow `Right, []) ->
         (match post with
          | [] -> go pre post
          | hd::tl -> go (pre @ [hd]) tl)
      | `Key (`Arrow `Left, []) ->
         (match List.rev pre with
          | [] -> go [] post
          | hd::tl -> go (List.rev tl) (hd :: post))
      | `Key (`Uchar chr, []) -> go (pre @ [chr]) post
      | _ -> go pre post
  in
  let pre = Utils.option [] str_to_char_list default in
  go pre []

let read_password ?(above = []) ?(prefix = "") ?(below = []) term =
  let rec go pre =
    let w = I.(width (uchars A.empty (Array.of_list pre))) in
    let input = I.uchar A.(st reverse) 0x2605 w 1 in
    let prefix = I.string A.empty prefix in
    rewrap term above below (prefix, input, I.empty) (T.size term) >>= fun () ->
    Lwt_stream.next (T.events term) >>= function
      | `Key (`Enter, []) ->
         let buf = Buffer.create w in
         Array.iter (Uutf.Buffer.add_utf_8 buf) (Array.of_list pre) ;
         Lwt.return (Buffer.contents buf)
      | `Key (`Backspace, []) ->
         (match List.rev pre with
          | [] -> go pre
          | _::tl -> go (List.rev tl))
      | `Key (`Uchar chr, []) -> go (pre @ [chr])
      | _ -> go pre
  in
  go []

let rec read_yes_no ?above ?prefix ?below default term =
  let def = if default then "yes" else "no" in
  read_line ?above ?below ?prefix ~default:def term >>= function
    | "" -> Lwt.return default
    | "y" | "Y" | "yes" | "Yes" | "YES" -> Lwt.return true
    | "n" | "N" | "no" | "No" | "NO" -> Lwt.return false
    | _ -> read_yes_no ?above ?prefix ?below default term

