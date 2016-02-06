
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

let readline_input = function
  | `Key (`Backspace, []) ->
    `Ok (fun (pre, post) ->
        match List.rev pre with
        | [] -> (pre, post)
        | _::tl -> (List.rev tl, post))
  | `Key (`Delete, []) ->
    `Ok (fun (pre, post) ->
        match post with
        | [] -> (pre, post)
        | _::tl -> (pre, tl))
  | `Key (`Home, []) -> `Ok (fun (pre, post) -> [], pre @ post)
  | `Key (`End, []) -> `Ok (fun (pre, post) -> pre @ post, [])
  | `Key (`Arrow `Right, []) ->
    `Ok (fun (pre, post) ->
        match post with
        | [] -> (pre, post)
        | hd::tl -> (pre @ [hd], tl))
  | `Key (`Arrow `Left, []) ->
    `Ok (fun (pre, post) ->
        match List.rev pre with
        | [] -> ([], post)
        | hd::tl -> (List.rev tl, hd :: post))
  | `Key (`Uchar chr, []) -> `Ok (fun (pre, post) -> pre @ [chr], post)
  | k -> `Unhandled k

let emacs_bindings = function
  | `Key (`Uchar 0x41, [`Ctrl]) (* C-a *) -> `Ok (fun (pre, post) -> ([], pre @ post))
  | `Key (`Uchar 0x45, [`Ctrl]) (* C-e *) -> `Ok (fun (pre, post) -> (pre @ post, []))

  | `Key (`Uchar 0x4b, [`Ctrl]) (* C-k *) -> `Ok (fun (pre, _) -> (pre, []))
  | `Key (`Uchar 0x55, [`Ctrl]) (* C-u *) -> `Ok (fun (_, post) -> ([], post))

  | `Key (`Uchar 0x46, [`Ctrl]) (* C-f *) ->
    `Ok (fun (pre, post) ->
        match post with
        | [] -> (pre, post)
        | hd::tl -> (pre @ [hd], tl))
  | `Key (`Uchar 0x42, [`Ctrl]) (* C-b *) ->
    `Ok (fun (pre, post) ->
        match List.rev pre with
        | [] -> ([], post)
        | hd::tl -> (List.rev tl, hd :: post))

  | k -> `Unhandled k

let str_to_char_list str =
  Astring.String.fold_right (fun ch acc -> int_of_char ch :: acc) str []

let char_list_to_str xs =
  let inp = Array.of_list xs in
  let buf = Buffer.create (Array.length inp) in
  Array.iter (Uutf.Buffer.add_utf_8 buf) inp ;
  Buffer.contents buf

let read_line ?(above = []) ?(prefix = "") ?default ?(below = []) term =
  let rec go (pre, post) =
    let iprefix = I.string A.empty prefix
    and iinp =
      let inp = Array.of_list pre in
      I.uchars A.(st reverse) inp
    and iinp2 =
      let inp2 = Array.of_list post in
      I.uchars A.(st reverse) inp2
    in
    rewrap term above below (iprefix, iinp, iinp2) (T.size term) >>= fun () ->
    Lwt_stream.next (T.events term) >>= fun e ->
    match readline_input e with
    | `Ok f -> go (f (pre, post))
    | `Unhandled k ->
      match emacs_bindings k with
      | `Ok f -> go (f (pre, post))
      | `Unhandled k ->
        match k with
        | `Key (`Enter, []) -> Lwt.return (char_list_to_str (pre @ post))
        | _ -> go (pre, post)
  in
  let pre = Utils.option [] str_to_char_list default in
  go (pre, [])

let read_password ?(above = []) ?(prefix = "") ?(below = []) term =
  let rec go pre =
    let w = I.(width (uchars A.empty (Array.of_list pre))) in
    let input = I.uchar A.(st reverse) 0x2605 w 1 in
    let prefix = I.string A.empty prefix in
    rewrap term above below (prefix, input, I.empty) (T.size term) >>= fun () ->
    Lwt_stream.next (T.events term) >>= function
      | `Key (`Enter, []) -> Lwt.return (char_list_to_str pre)
      | `Key (`Backspace, []) ->
         (match List.rev pre with
          | [] -> go pre
          | _::tl -> go (List.rev tl))
      | `Key (`Uchar chr, []) -> go (pre @ [chr])
      | _ -> go pre
  in
  go []

let rec read_yes_no ?above ?prefix ?below def term =
  let default = if def then "yes" else "no" in
  read_line ?above ?below ?prefix ~default term >>= function
    | "" -> Lwt.return def
    | "y" | "Y" | "yes" | "Yes" | "YES" -> Lwt.return true
    | "n" | "N" | "no" | "No" | "NO" -> Lwt.return false
    | _ -> read_yes_no ?above ?prefix ?below def term

