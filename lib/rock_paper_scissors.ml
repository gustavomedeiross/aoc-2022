let ( % ) f g x = f (g x)
let sum = List.fold_left ( + ) 0

module Parser = struct
  module LcToken = struct
    (** left column *)
    type t =
      | A
      | B
      | C

    let of_char = function
      | 'A' -> Ok A
      | 'B' -> Ok B
      | 'C' -> Ok C
      | ch -> Error (Printf.sprintf "Invalid token on left column, got %c\n" ch)
    ;;
  end

  module RcToken = struct
    (** right column *)
    type t =
      | X
      | Y
      | Z

    let of_char = function
      | 'X' -> Ok X
      | 'Y' -> Ok Y
      | 'Z' -> Ok Z
      | ch -> Error (Printf.sprintf "Invalid token on right column, got %c\n" ch)
    ;;
  end

  (* Line example: `A Y` *)
  let parse_line line =
    let ( let* ) = Result.bind in
    let line = String.trim line in
    let l, r = Scanf.sscanf line "%c %c" (fun a b -> a, b) in
    let* l = LcToken.of_char l in
    let* r = RcToken.of_char r in
    Ok (l, r)
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

  (* TODO: return result and fail outside *)
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

module Pick = struct
  type t =
    | Rock
    | Paper
    | Scissors

  let of_lc_token token =
    let open Parser.LcToken in
    match token with
    | A -> Rock
    | B -> Paper
    | C -> Scissors
  ;;

  let of_rc_token token =
    let open Parser.RcToken in
    match token with
    | X -> Rock
    | Y -> Paper
    | Z -> Scissors
  ;;
end

module RoundOutcome = struct
  type t =
    | Win
    | Lose
    | Draw

  let of_rc_token token =
    let open Parser.RcToken in
    match token with
    | X -> Lose
    | Y -> Draw
    | Z -> Win
  ;;
end

module Game = struct
  open Pick
  open RoundOutcome

  let duel p1 p2 =
    match p1, p2 with
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> Win
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> Lose
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> Draw
  ;;

  let get_pick_to outcome pick =
    match outcome, pick with
    | Win, Rock -> Paper
    | Win, Paper -> Scissors
    | Win, Scissors -> Rock
    | Lose, Rock -> Scissors
    | Lose, Paper -> Rock
    | Lose, Scissors -> Paper
    | Draw, Rock -> Rock
    | Draw, Paper -> Paper
    | Draw, Scissors -> Scissors
  ;;

  let points_from_pick = function
    | Pick.Rock -> 1
    | Pick.Paper -> 2
    | Pick.Scissors -> 3
  ;;

  let points_from_outcome = function
    | Win -> 6
    | Draw -> 3
    | Lose -> 0
  ;;

  let calculate_round_points (elf, me) =
    let result = duel me elf in
    result |> points_from_outcome |> ( + ) @@ points_from_pick me
  ;;
end

let solve_part_one tokens =
  tokens
  |> List.map (fun (l, r) -> Pick.of_lc_token l, Pick.of_rc_token r)
  |> List.map Game.calculate_round_points
  |> sum
;;

let solve_part_two tokens =
  tokens
  |> List.map (fun (l, r) -> Pick.of_lc_token l, RoundOutcome.of_rc_token r)
  |> List.map (fun (elf, outcome) -> elf, Game.get_pick_to outcome elf)
  |> List.map Game.calculate_round_points
  |> sum
;;

let solve file_contents =
  let tokens = Parser.parse file_contents in
  Printf.printf "%d\n" @@ solve_part_one tokens;
  Printf.printf "%d\n" @@ solve_part_two tokens
;;

let%expect_test _ =
  solve {|
    A Y
    B X
    C Z
    |};
  [%expect "\n    15\n    12\n  "]
;;
