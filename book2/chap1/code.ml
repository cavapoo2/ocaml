(*fold left*)
let rec fold_left f a l =
  match l with
  [] -> a
  |h::t -> fold_left f (f a h) t

(*fold right*)
let rec fold_right f a l =
  match l with
  [] -> a
  |h::t -> f h (fold_right f a t)
(*using a fold to subtract from an initial value*)
let subtract v ls = fold_left (fun v x -> (v - x)) v ls

let subtract' v ls = List.fold_left (fun v x -> v -x) v ls
let subtract'' v ls = List.fold_left (-) v ls

(*length of list with fold*)
let length_of_list l =
  fold_left (fun v _ -> v+1) 0 l

let length_of_list' l =
  fold_right (fun _ v -> v+1) 0 l
(*last element of a list*) 
let last_element l = 
  fold_left (fun _ x -> x) 0 l  

let last_element' l = 
  fold_right (fun x v -> if v = 0 then v+x else v) 0 l  

let reverse_list ls =
  fold_left (fun v x -> x :: v) [] ls

let mem a ls = 
  fold_left (fun v x -> if a = x then (v || true) else (v || false)) false ls

  (*concat strings using concat operator*)
let join_strings ls = 
  fold_left (fun a x -> if (a = "") then a ^ x else a ^ " " ^ x) "" ls

type 'a tree = 
  Lf
  |Br of 'a * 'a tree * 'a tree

let rec fold_tree f e t = 
  match t with
  Lf -> e
  |Br(x,l,r) -> f x (fold_tree f e l) (fold_tree f e r)

let my_tree = Br(0,Br(3,Lf,Lf),Br(4,Br(17,Lf,Lf),Lf))

let max a b =
  if a > b then a else b

  (* tree depth*)
let tree_depth t =
   fold_tree (fun _ l r -> 1 + max l r) 0 t

(*Time function performance delta*)
let data = [1;2;3;4;45;56;2;4;8;13;33;6;11;55;78;4;7;2] 
let t1 = Unix.gettimeofday()
let _ = for i =0 to 100000 do ignore (mem 55 data) done
let t2 = Unix.gettimeofday()
let _ = for i =0 to 100000 do ignore (List.mem 55 data) done
let t3 = Unix.gettimeofday()
let _ = Printf.printf "Time for my mem =  %f " (t2 -. t1);
Printf.printf "Time for my List.mem =  %f " (t3 -. t2)

