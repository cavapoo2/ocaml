open Base
let languages = "OCaml,Perl,C++,C";;

let dashed_languages () =
  let language_list = String.split languages ~on:',' in
  String.concat ~sep:"-" language_list

let area_of_ring inner_radius outer_radius =
  let pi = Float.pi in
  let area_of_circle r = pi *. r *. r in
  area_of_circle outer_radius -. area_of_circle inner_radius


let upcase_first_entry line =
  match String.split ~on:',' line with
  | [] -> assert false (* String.split returns at least one element *)
  | first :: rest -> String.concat ~sep:"," (String.uppercase first :: rest)

(*example of recursion for list with no elements, one element, and multiple elements*)

let rec find_first_repeat list =
  match list with
  | [] | [_] ->
    (* only zero or one elements, so no repeats *)
    None
  | x :: y :: tl ->
    if x = y then Some x else find_first_repeat (y::tl)

(*combing let and the and *)

let rec is_even x =
  if x = 0 then true else is_odd (x - 1)
and is_odd x =
  if x = 0 then false else is_even (x - 1)

(*redefining operators*)
(*call like this 
  (3,2) +! (-2,4);;
*)
let (+!) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2);;

(*notice the required extra spaces around the operator*)
let ( *** ) x y = (x **. y) **. y

open Stdio;;
let path = "/usr/bin:/usr/local/bin:/bin:/sbin:/usr/bin";;

(*using the sequence operator - just like linux pipe command*)
let split_and_sort on path = 
  String.split ~on:on path
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:print_endline

(*version without the sequence operator*)
let split_and_sort' on path =
  let split_path = String.split ~on:on path in
  let deduped_path = List.dedup_and_sort ~compare:String.compare split_path in
  List.iter ~f:print_endline deduped_path

(*using function keyword which has built in pattern matching*)
let some_or_zero = function
  | Some x -> x
  | None -> 0

(*equivalent not using function keyword*)
let some_or_zero' num_opt =
  match num_opt with
  | Some x -> x
  | None -> 0

(*2 argument function curried function 1st arument is default*)
let some_or_default default = function
  | Some x -> x
  | None -> default

(*partial application*)
let mapl lst =
  List.map ~f:(some_or_default 100) lst


(*optional arguments
  call like this 
  concat "1" "2" 
  concat ~sep:":" "1" "2"
*)
let concat ?sep x y = 
  let sep  = match sep with None -> "" | Some x -> x in
  x ^ sep ^ y

let uppercase_concat ?sep x y = concat ?sep (String.uppercase x) y

(*arguments with ~ or tilde are just labelled arguments so i could call test
  like this
  test 1 2
  or 
  test ~a:1 ~b:2 *)
let test ~a ~b = a + b
let test' a b = a + b

