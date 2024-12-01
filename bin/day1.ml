open Core
open In_channel

let file = "data/day1.full"

let rec diffs lst1 lst2 =
  match lst1, lst2 with
  | hd1 :: tl1, hd2 :: tl2 -> abs (hd1 - hd2) :: diffs tl1 tl2
  | _, _ -> []
;;

let counter lst =
  List.fold lst ~init:Int.Map.empty ~f:(fun acc x ->
    Map.update acc x ~f:(function
      | None -> 1
      | Some cnt -> cnt + 1))
;;

let part_1 ic =
  let rec helper ic =
    match input_line ic with
    | None -> [], []
    | Some line ->
      let numbers =
        String.split line ~on:' '
        |> List.filter ~f:(fun s -> not (String.is_empty s))
        |> List.map ~f:Int.of_string
      in
      let lst1, lst2 = helper ic in
      List.hd_exn numbers :: lst1, List.hd_exn (List.tl_exn numbers) :: lst2
  in
  let lst1, lst2 =
    match helper ic with
    | lst1, lst2 ->
      List.sort ~compare:Int.compare lst1, List.sort ~compare:Int.compare lst2
  in
  List.reduce (diffs lst1 lst2) ~f:( + ) |> Option.value ~default:0
;;

let part_2 ic =
  let rec helper ic =
    match input_line ic with
    | None -> [], []
    | Some line ->
      let numbers =
        String.split line ~on:' '
        |> List.filter ~f:(fun s -> not (String.is_empty s))
        |> List.map ~f:Int.of_string
      in
      let lst1, lst2 = helper ic in
      List.hd_exn numbers :: lst1, List.hd_exn (List.tl_exn numbers) :: lst2
  in
  let lst1, lst2 = helper ic in
  let cnt = counter lst2 in
  let res = List.map lst1 ~f:(fun x -> x * (Map.find cnt x |> Option.value ~default:0)) in
  List.reduce_exn res ~f:( + )
;;

let () =
  let ic1 = create file in
  let ic2 = create file in
  part_1 ic1 |> Int.to_string |> print_endline;
  part_2 ic2 |> Int.to_string |> print_endline
;;
