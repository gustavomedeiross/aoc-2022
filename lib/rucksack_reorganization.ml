module CharSet = Set.Make (Char)

let ( % ) f g x = f (g x)
let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let priority ch =
  String.index_opt alphabet ch
  |> Option.map @@ (+) 1
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

let solve file_contents =
  let lines =
    file_contents
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> remove_empty_lines
  in
  let results =
    List.mapi
      (fun index line ->
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
  | Ok priorities -> Printf.printf "%d\n" @@ sum priorities
  | Error err -> invalid_arg err
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
  [%expect "157"]
;;
