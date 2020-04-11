let rec evens xs =
  match xs with
  [] -> []
  | x::xs -> if (x mod 2) = 0 then x :: evens xs else evens xs

let rec evenNumbered xs =
  match xs with
  [] -> []
  | [_] -> []
  | x::y::ys -> y :: evenNumbered ys

let rec count_true xs =
  match xs with
  [] -> 0
  | false::t -> count_true t
  | true::t -> 1 + count_true t 

let make_palidrome ls =
  let lr = List.rev ls in
  match lr with
  [] -> []
  | x::xs -> ls @ xs

let is_palindrome al =
  al = List.rev al

let rec drop_last l = 
  match l with 
 [] -> []
  |[_] -> []
  |x::ys -> x :: drop_last ys

let drop_last' l =
  let rec inner n l =
    match l with 
  []-> []
  |[_] -> List.rev n
 | x::xs -> inner (x ::n) xs in

  inner [] l;;

let rec exists e l =
  match l with
  [] -> false
 |x::xs -> if x = e then true else exists e xs

let rec make_set l =
  match l with 
  [] -> []
 | x::xs -> if exists x xs then make_set xs else x :: make_set xs

let rev l =
  let rec inner r ls =
    match ls with
    [] -> r
 | x::xs -> inner (x :: r) xs in

  inner [] l;;


