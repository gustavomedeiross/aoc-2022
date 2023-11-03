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

let top_3_elves inventories =
  let calories =
    inventories
    |> List.map sum
    |> List.sort Int.compare
    |> List.rev
  in
  match calories with
  | f :: s :: t :: _ -> Some (f, s, t)
  | _ -> None

let solve file_contents =
  let inventories =
    file_contents
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> split_inventories
    |> List.map (List.map int_of_string)
  in

  (* Part 1 *)
  inventories
  |> List.map sum
  |> max
  |> Printf.printf "%d\n";

  (* Part 2 *)
  match top_3_elves inventories with
  | Some (f, s, t) -> Printf.printf "%d\n" (f + s + t)
  | None -> invalid_arg "Input should have at least 3 elf inventories"

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

    10000
    |};
  [%expect {|
    24000
    45000
    |}]
