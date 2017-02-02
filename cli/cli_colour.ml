open Notty
open Astring



module OrderedKind = struct
  type t = User.chatkind

  let compare (a : [< t]) (b : [< t]) = match a, b with
    | `Transit, `Transit -> 0
    | `Chat, `Chat -> 0
    | `GroupChat, `GroupChat -> 0
    | `Presence, `Presence -> 0
    | `Info, `Info -> 0
    | `Warning, `Warning -> 0
    | `Error, `Error -> 0
    | `Success, `Success -> 0
    | `Transit, _ -> 1
    | _, `Transit -> -1
    | `Chat, _ -> 1
    | _, `Chat -> -1
    | `GroupChat, _ -> 1
    | _, `GroupChat -> -1
    | `Presence, _ -> 1
    | _, `Presence -> -1
    | `Info, _ -> 1
    | _, `Info -> -1
    | `Warning, _ -> 1
    | _, `Warning -> -1
    | `Error, _ -> 1
    | _, `Error -> -1
end

module M = Map.Make(OrderedKind)

let c = ref M.empty

let init () =
  let m =
    M.add `Transit A.(gray 18)
      (M.add `Presence A.(gray 12)
         (M.add `Info A.(gray 18)
            (M.add `Warning A.yellow
               (M.add `Error A.red
                  (M.add `Success A.green M.empty)))))
  in
  c := m

let parse_value =
  let open A in
  function
  | "empty" -> None
  | x -> Some (match x with
  | "black" -> Some black
  | "red" -> Some red
  | "green" -> Some green
  | "yellow" -> Some yellow
  | "blue" -> Some blue
  | "magenta" -> Some magenta
  | "cyan" -> Some cyan
  | "white" -> Some white
  | "lightblack" -> Some lightblack
  | "lightred" -> Some lightred
  | "lightgreen" -> Some lightgreen
  | "lightyellow" -> Some lightyellow
  | "lightblue" -> Some lightblue
  | "lightmagenta" -> Some lightmagenta
  | "lightcyan" -> Some lightcyan
  | "lightwhite" -> Some lightwhite
  | x when String.is_prefix ~affix:"gray" x ->
    (match String.cut ~sep:" " x with
     | None -> None
     | Some (_, i) -> match String.to_int i with
       | Some x when x >= 0 && x <= 23 -> Some (gray x)
       | _ -> None)
  | x when String.is_prefix ~affix:"rgb" x ->
    (match String.cuts ~sep:" " x with
     | _::xs when List.length xs = 3 ->
       (match
          List.filter
            (function None -> false | Some _ -> true)
            (List.map String.to_int xs)
        with
        | Some r::Some g::Some b::[] when r >= 0 && r <= 5 && g >= 0 && g <= 5 && b >= 0 && b <= 5 ->
          Some (rgb ~r ~g ~b)
        | _ -> None)
     | _ -> None)
  | _ -> None)

let load_c (kind, value) =
  match parse_value value with
  | None -> c := (M.remove kind !c)
  | Some None -> invalid_arg ("couldn't parse your colour for " ^ User.chatkind_to_string kind ^ ": " ^ value)
  | Some (Some col) -> c := (M.add kind col !c)

let load = List.iter load_c

let kind k = try A.fg (M.find k !c) with Not_found -> A.empty

