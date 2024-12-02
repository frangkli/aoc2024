open Core
open In_channel

let file = "data/day2.full"

let check_pairs ?(skips = 0) (lst : 'a list) (f : 'a -> 'a -> bool) : bool =
  let rec helper lst skips =
    match lst with
    | n :: m :: tl ->
      (f n m && helper (m :: tl) skips) || (skips > 0 && helper (n :: tl) (skips - 1))
    | _ -> true
  in
  (* additional case for when the first element is to be skipped *)
  helper lst skips || (skips > 0 && helper (List.tl_exn lst) (skips - 1))
;;

let increasing ?(skips = 0) (lst : int list) : bool =
  check_pairs ~skips lst (fun n m -> n < m && m - n > 0 && m - n < 4)
;;

let decreasing ?(skips = 0) (lst : int list) : bool =
  check_pairs ~skips lst (fun n m -> n > m && n - m > 0 && n - m < 4)
;;

let safe (lst : int list) : bool = increasing lst || decreasing lst

let tolerable_safe (lst : int list) : bool =
  increasing ~skips:1 lst || decreasing ~skips:1 lst
;;

let rec part1 ic =
  match input_line ic with
  | None -> 0
  | Some line ->
    let levels = String.split line ~on:' ' |> List.map ~f:Int.of_string in
    if safe levels then 1 + part1 ic else part1 ic
;;

let rec part2 ic =
  match input_line ic with
  | None -> 0
  | Some line ->
    let levels = String.split line ~on:' ' |> List.map ~f:Int.of_string in
    if tolerable_safe levels then 1 + part2 ic else part2 ic
;;

let () =
  let ic1 = create file in
  let ic2 = create file in
  part1 ic1 |> Int.to_string |> print_endline;
  part2 ic2 |> Int.to_string |> print_endline
;;
