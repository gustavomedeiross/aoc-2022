open Aoc_2022

let read_file file = In_channel.with_open_bin file In_channel.input_all

module Cmd = struct
  type t =
    { challenge : string
    ; input_file : string
    }

  let parse () =
    match Array.to_list Sys.argv with
    | [ _; challenge; input_file ] -> { challenge; input_file }
    | _ ->
      Printf.printf "Usage: aoc_2022 <challenge> <input_file>\n";
      invalid_arg "Invalid number of arguments received"
  ;;
end

let select_challenge = function
  | "calorie_counting" -> Ok Calorie_counting.solve
  | challenge -> Error (Printf.sprintf "Unknown challenge: %s" challenge)
;;

let () =
  let { Cmd.challenge; input_file } = Cmd.parse () in
  let file_contents = read_file input_file in
  match select_challenge challenge with
  | Ok solve_fn -> solve_fn file_contents
  | Error msg -> invalid_arg msg
;;
