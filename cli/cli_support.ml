open Notty

(* Some utilities on top of Notty which I need (might be useful elsewhere?) *)

let hdash a w = I.uchar a 0x2500 w 1
and vdash a h = I.uchar a 0x2502 1 h
and star a w = I.uchar a 0x2605 w 1

let wrap w image =
  let w1 = I.width image in
  let rec go i =
    if (w1 - i) <= w then
      [ I.hcrop i 0 image ]
    else
      I.hcrop i (w1 - i - w) image :: go (i + w)
  in
  let vs = go 0 in
  I.vcat vs

let render_wrapped_list width fmt entries =
  let formatted = List.map fmt entries in
  I.vcat (List.map (wrap width) formatted)

let split_on_nl a m =
  List.map (I.string a) (Astring.String.cuts ~sep:"\n" ~empty:false m)

let v_space f width left right =
  let len = width - I.(width left + width right) in
  if len <= 0 then
    I.hpad 0 len I.(left <|> right)
  else
    let fill = I.tile len 1 f in
    I.hcat [ left ; fill ; right ]

let v_center left right width =
  let lw = I.width left
  and rw = I.width right
  in
  match rw, lw >= width with
  | 0, true -> (I.hcrop (lw - width + 1) 0 left, width)
  | 0, false -> (left, succ lw)
  | _, _ ->
    if lw + rw >= width then
      let leftw = min (max (width - rw) (width / 2)) lw in
      let rightw = width - leftw in
      let l = I.hcrop (lw - leftw) 0 left
      and r = I.hcrop 0 (rw - rightw) right
      in
      (I.(l <|> r), succ leftw)
    else
      (I.(left <|> right), succ lw)

let str_to_char_list str =
  Astring.String.fold_right (fun ch acc -> int_of_char ch :: acc) str []

let char_list_to_str xs =
  let inp = Array.of_list xs in
  let buf = Buffer.create (Array.length inp) in
  Array.iter (Uutf.Buffer.add_utf_8 buf) inp ;
  Buffer.contents buf

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

  | `Key (`Arrow `Left, [`Ctrl]) ->
    `Ok (fun (pre, post) ->
        let inp, middle = match List.rev pre with
          | ws::xs -> (xs, [ws])
          | [] -> ([], [])
        in
        let _, pre, prep =
          List.fold_left (fun (found, rp, rpp) char ->
              if found then
                (found, char :: rp, rpp)
              else if Uucp.White.is_white_space char then
                (true, char :: rp, rpp)
              else
                (false, rp, char :: rpp) )
            (false, [], [])
            inp
        in
        (pre, prep @ middle @ post))
  | `Key (`Arrow `Right, [`Ctrl]) ->
    `Ok (fun (pre, post) ->
        let inp, middle = match post with
          | ws::xs -> (xs, [ws])
          | [] -> ([], [])
        in
        let _, prep, post =
          List.fold_left (fun (found, rp, rpp) char ->
              if found then
                (found, rp, char :: rpp)
              else if Uucp.White.is_white_space char then
                (true, rp, char :: rpp)
              else
                (false, char :: rp, rpp) )
            (false, [], [])
            inp
        in
        (pre @ middle @ (List.rev prep), List.rev post))

  | k -> `Unhandled k
