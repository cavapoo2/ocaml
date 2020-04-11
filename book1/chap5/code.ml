let rec merge x y =
  match x ,y with 
  [], l -> l
  | l, [] -> l
  | hx::tx,hy::ty ->
      if hx < hy
      then hx :: merge tx (hy::ty)
      else hy :: merge (hx::tx) ty

let rec take n ls =
  match ls with
  [] -> []
  | x::xs -> 
      if n > 0
      then x :: take (n-1) xs
      else []

let rec drop n ls =
  match ls with 
  [] -> []
  | x::xs ->
      if n > 0
      then drop (n-1) xs
      else x::xs

let rec msort l =
  match l with 
  [] -> []
  | [x] -> [x]
  | _ ->
      let left = take (List.length l/2) l in
      let right = drop (List.length l/2)  l in
      merge (msort left) (msort right)

let rec msort' l =
  match l with 
  [] -> []
  | [x] -> [x]
  | _ ->
      let len = List.length l/2 in
      let left = take len l in
      let right = drop len l in
      merge (msort left) (msort right)

let rec insert x l =
  match l with
  [] -> [x]
  | h::t ->
      if x <= h then x :: l
      else h :: insert x t

(*p is predicate *)
let rec insert' p x l =
  match l with
  [] -> [x]
  | h::t ->
      if p x h then x :: l
      else h :: insert' p x t


let rec isort l =
  match l with 
  [] -> []
  | h::t -> insert h (isort t)

let predh a b = a >= b

(* p is predicate*)
let rec isort' p l =
  match l with 
  [] -> []
  | h::t -> insert' p h (isort' p t)

let isort_greater l = isort' predh l

exception EmptyList of string

let headl ls = 
  match ls with
  [] -> raise(EmptyList "Empty List")
  | h::t -> h

let rec is_sorted l =
  match l with
  [] -> true
  |[x] -> true
  | h::t -> 
      if h <= headl t then is_sorted t
      else false

(* call like this 
 * is_sorted' (fun a b -> a >= b) [5;3;2;1];;*)
let rec is_sorted' pred l =
  match l with
  [] -> true
  |[x] -> true
  | h::t -> 
      if pred h (headl t) then is_sorted' pred t
      else false
