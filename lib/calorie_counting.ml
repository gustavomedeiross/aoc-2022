let split_inventories lines =
  let rec aux xs ys curr =
    match xs with
    | [] -> List.rev ys
    | "" :: xs -> aux xs ((List.rev curr) :: ys) []
    | x :: xs -> aux xs ys (x :: curr)
  in
  aux lines [] []

let sum = List.fold_left (+) 0

let max list = List.fold_left Int.max (List.hd list) list

let solve file_contents =
  file_contents
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> split_inventories
  |> List.map (List.map int_of_string)
  |> List.map sum
  |> max
  |> Printf.printf "%d\n"

let%expect_test _ =
  solve {|
    1000
    2000
    3000

    4000

    5000
    6000

    7000
    8000
    9000

    10000|};
  [%expect {|
    24000
    |}]

