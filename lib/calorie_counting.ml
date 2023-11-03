let split_inventories lines =
  let rec aux xs ys curr =
    match xs with
    | [] -> List.rev ys
    | "" :: xs -> aux xs (List.rev curr :: ys) []
    | x :: xs -> aux xs ys (x :: curr)
  in
  aux lines [] []
;;

let sum = List.fold_left ( + ) 0
let max list = List.fold_left Int.max (List.hd list) list

let take_first n list =
  let rec aux n xs ys =
    match n, xs with
    | 0, _ -> Some (List.rev ys)
    | _, [] -> None
    | n, hd :: tl -> aux (n - 1) tl (hd :: ys)
  in
  aux n list []
;;

let solve file_contents =
  let inventories =
    file_contents
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> split_inventories
    |> List.map (List.map int_of_string)
  in
  (* Part 1 *)
  inventories |> List.map sum |> max |> Printf.printf "%d\n";
  (* Part 2 *)
  let top_invs =
    inventories |> List.map sum |> List.sort Int.compare |> List.rev |> take_first 3
  in
  match top_invs with
  | Some top_invs -> Printf.printf "%d\n" @@ sum top_invs
  | None -> invalid_arg "Input should have at least 3 elf inventories"
;;

let%expect_test _ =
  solve
    {|
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
;;
