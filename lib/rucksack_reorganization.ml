open Base
module CharSet = Set.M (Char)

let ( % ) f g x = f (g x)
let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let priority ch =
  String.index alphabet ch
  |> Option.map ~f:(( + ) 1)
  |> Result.of_option ~error:(Printf.sprintf "Character not found in alphabet: %c" ch)
;;

let split_string str pos =
  let first = String.prefix str pos in
  let last = String.suffix str (String.length str - pos) in
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

let sum = List.fold_left ~init:0 ~f:( + )

let is_string_empty = function
  | "" -> true
  | _ -> false
;;

let remove_empty_lines = List.filter ~f:(not % is_string_empty % String.strip)

module PartOne = struct
  let solve lines =
    let results =
      List.mapi
        ~f:(fun index line ->
          (* TODO: create module to encapulate set *)
          let first, last = split_string line @@ (String.length line / 2) in
          let first' = Set.of_list (module Char) (String.to_list first) in
          let last' = Set.of_list (module Char) (String.to_list last) in
          let intersection = Set.inter first' last' in
          let ch =
            match Set.to_list intersection with
            | [ ch ] -> Ok ch
            | ch ->
              let str = [%show: char list] ch in
              Error
                (Printf.sprintf
                   "Line %d: intersection of strings doesn't have one element - %s"
                   index
                   str)
          in
          Result.bind ch ~f:priority)
        lines
    in
    match sequence results with
    | Ok priorities -> sum priorities
    | Error err -> invalid_arg err
  ;;
end

module PartTwo = struct
  let range i = List.init i ~f:Int.succ
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
    let xs = List.zip_exn list idxs in
    aux xs [] []
  ;;

  let intersect group =
    let group = group |> List.map ~f:(Set.of_list (module Char) % String.to_list) in
    group |> List.fold_left ~init:(List.hd_exn group) ~f:Set.inter |> Set.to_list
  ;;

  let solve lines =
    let results = lines |> group |> List.bind ~f:intersect |> List.map ~f:priority in
    match sequence results with
    | Ok priorities -> sum priorities
    | Error err -> invalid_arg err
  ;;
end

let solve file_contents =
  let lines =
    file_contents
    |> String.split ~on:'\n'
    |> List.map ~f:String.strip
    |> remove_empty_lines
  in
  Stdio.printf "%d\n" @@ PartOne.solve lines;
  Stdio.printf "%d\n" @@ PartTwo.solve lines
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
