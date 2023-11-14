module CharSet = Set.Make (Char)

let ( % ) f g x = f (g x)
let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let priority ch =
  String.index_opt alphabet ch
  |> Option.map @@ ( + ) 1
  |> Option.to_result ~none:(Printf.sprintf "Character not found in alphabet: %c" ch)
;;

let split_string str pos =
  let first = String.sub str 0 @@ pos in
  let last = String.sub str pos @@ (String.length str - pos) in
  first, last
;;

let sequence list =
  let rec loop list acc =
    match list with
    | [] -> Ok (List.rev acc)
    | Error e :: _ -> Error e
    | Ok x :: xs -> loop xs (x :: acc)
  in
  loop list []
;;

let sum = List.fold_left ( + ) 0

let is_string_empty = function
  | "" -> true
  | _ -> false
;;

let remove_empty_lines = List.filter @@ (not % is_string_empty % String.trim)

module PartOne = struct
  let solve lines =
    let results =
      List.mapi
        (fun index line ->
          (* TODO: create module to encapulate set *)
          let first, last = split_string line @@ (String.length line / 2) in
          let first' = CharSet.of_seq (String.to_seq first) in
          let last' = CharSet.of_seq (String.to_seq last) in
          let intersection = CharSet.inter first' last' in
          let ch =
            match CharSet.to_list intersection with
            | [ ch ] -> Ok ch
            | ch ->
              let str = [%show: char list] ch in
              Error
                (Printf.sprintf
                   "Line %d: intersection of strings doesn't have one element - %s"
                   index
                   str)
          in
          Result.bind ch priority)
        lines
    in
    match sequence results with
    | Ok priorities -> sum priorities
    | Error err -> invalid_arg err
  ;;
end

module PartTwo = struct
  let range i = List.init i succ
  let is_div_by n x = Int.equal 0 @@ Int.rem x n

  (* TODO: create function like "split on" that takes a lambda *)
  let group list =
    let rec aux xs ys curr =
      match xs with
      | [] -> List.rev ys
      | (x, pos) :: xs when is_div_by 3 pos ->
        let curr = x :: curr in
        aux xs (List.rev curr :: ys) []
      | (x, _) :: xs -> aux xs ys (x :: curr)
    in
    let idxs = range @@ List.length list in
    let xs = List.combine list idxs in
    aux xs [] []
  ;;

  let intersect group =
    let group = group |> List.map (CharSet.of_seq % String.to_seq) in
    group |> List.fold_left CharSet.inter (List.hd group) |> CharSet.to_list
  ;;

  let solve lines =
    let results =
      lines |> group |> List.map intersect |> List.flatten |> List.map priority
    in
    match sequence results with
    | Ok priorities -> sum priorities
    | Error err -> invalid_arg err
  ;;
end

let solve file_contents =
  let lines =
    file_contents
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> remove_empty_lines
  in
  Printf.printf "%d\n" @@ PartOne.solve lines;
  Printf.printf "%d\n" @@ PartTwo.solve lines
;;

let%expect_test _ =
  solve
    {|
    vJrwpWtwJgWrhcsFMMfFFhFp
    jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
    PmmdzqPrVvPwwTWBwg
    wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
    ttgJtRGJQctTZtZT
    CrZsJsPPZsGzwwsLwLmpwMDw
  |};
  [%expect "\n157\n70"]
;;
