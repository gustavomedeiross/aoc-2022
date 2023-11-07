let ( % ) f g x = f (g x)
let sum = List.fold_left ( + ) 0

module Pick = struct
  type t =
    | Rock
    | Paper
    | Scissors

  let of_char = function
    (* First column *)
    | 'A' -> Ok Rock
    | 'B' -> Ok Paper
    | 'C' -> Ok Scissors
    (* Second column *)
    | 'X' -> Ok Rock
    | 'Y' -> Ok Paper
    | 'Z' -> Ok Scissors
    | ch -> Error (Printf.sprintf "Invalid pick, got %c\n" ch)
  ;;
end

module Round = struct
  type result =
    | Win
    | Lose
    | Draw

  let duel elf me =
    let open Pick in
    match elf, me with
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> Win
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> Lose
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> Draw
  ;;

  let points_from_pick = function
    | Pick.Rock -> 1
    | Pick.Paper -> 2
    | Pick.Scissors -> 3
  ;;

  let points_from_result = function
    | Win -> 6
    | Draw -> 3
    | Lose -> 0
  ;;

  (* TODO: improve name of this fn *)
  let points_from_round (elf, me) =
    let result = duel elf me in
    result |> points_from_result |> ( + ) @@ points_from_pick me
  ;;
end

module Parser = struct
  (* Line example: `A Y` *)
  let parse_line line =
    let ( let* ) = Result.bind in
    let line = String.trim line in
    let p1, p2 = Scanf.sscanf line "%c %c" (fun a b -> a, b) in
    let* p1 = Pick.of_char p1 in
    let* p2 = Pick.of_char p2 in
    Ok (p1, p2)
  ;;

  let is_string_empty = function
    | "" -> true
    | _ -> false
  ;;

  let remove_empty_lines = List.filter @@ (not % is_string_empty % String.trim)

  let sequence list =
    let rec loop list acc =
      match list with
      | [] -> Ok (List.rev acc)
      | Error e :: _ -> Error e
      | Ok x :: xs -> loop xs (x :: acc)
    in
    loop list []
  ;;

  let parse file_contents =
    file_contents
    |> String.split_on_char '\n'
    |> remove_empty_lines
    |> List.map parse_line
    |> sequence
    |> function
    | Ok x -> x
    | Error e -> invalid_arg e
  ;;
end

let solve file_contents =
  file_contents
  |> Parser.parse
  |> List.map Round.points_from_round
  |> sum
  |> Printf.printf "%d\n"
;;

let%expect_test _ =
  solve {|
    A Y
    B X
    C Z
    |};
  [%expect "15"]
;;
