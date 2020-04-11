type 'a tree = 
  Br of 'a * 'a tree * 'a tree
  | Lf

let my_tree = Br(1,Br(0,Lf,Lf),Br(2,Br(3,Lf,Lf),Br(4,Lf,Lf)))

let other_tree =
  Br(10,Br(4,Br(3,Lf,Lf),Br(5,Lf,Lf)),Br(16,Br(11,Lf,Lf),Br(17,Lf,Lf)))

let leftH = Br(3,Br(2,Lf,Lf),Lf)

let rightH = Br(3,Lf,Br(4,Lf,Lf))

let shapeOk = Br(4,Br(1,Lf,Lf),Br(6,Lf,Lf)) 

(*  
let dict_tree2 = 
  Br(12,"twelve"
*)
let rec contains l v =
 match l with
  h::t -> if h == v then true else contains t v
  |[] -> false

let rec list_of_tree tr =
  match tr with
  Br(x,y,z) -> list_of_tree y @ [x] @ list_of_tree z
  |Lf -> [] 

let rec member_of_tree tr v =
  match tr with
  Br(x,y,z) -> x = v || member_of_tree y v || member_of_tree z v
  |Lf -> false

let rec flip tr = 
  match tr with
  Br(x,y,z) -> Br(x,flip z, flip y)
  | Lf -> Lf

let rec shape t1 t2 = 
  match (t1,t2) with
 Br(x,y,z),Br(x',y',z') -> shape y y' && shape z z'
  |Lf,Lf -> true
  |_,_ -> false

let rec insert tr v =
  match tr with
  Br(x,y,z) -> if v > x then Br(x,y,insert z v) else Br(x,insert y v,z)
  |Lf -> Br(v,Lf,Lf)

(* pair extract *)
let get_1_2 (a,_) = a
let get_2_2 (_,a) = a  

let rec insertp tr v =
  match tr with
  Br(x,y,z) -> if (get_1_2 v) > (get_1_2 x) then Br(x,y,insertp z v)
  else if (get_1_2 v) < (get_1_2 x) then Br(x,insertp y v,z) else tr
  |Lf -> Br(v,Lf,Lf)


let head ls =
  match ls with
  h::t -> h
  |[] -> []

let rest ls =
  match ls with
  [] -> []
  |h::t -> t

let tree_of_list ls =
let rec inner tr l =
  match l with
  [] -> tr
  |h::t -> inner (insert tr h) t
in inner (Lf) ls 

(*call like this
 *
 tree_of_list' (fun tr p -> insertp tr p) [(4,"four");(2,"two");(5,"five")];;
*)

let tree_of_list' insertf ls =
let rec inner tr l =
  match l with
  [] -> tr
  |h::t -> inner (insertf tr h) t
in inner (Lf) ls 

let dict_tree2 =  tree_of_list' (fun tr p -> insertp tr p) [(4,"four");(2,"two");(5,"five")];;

(* dict tree*)
let dict_tree1 =
  Br((7,"seven"),Br((2,"two"),Br((1,"one"),Lf,Lf),Br((3,"three"),Lf,Lf)),Br((14,"fourteen"),Lf,Lf))

let combine tr1 tr2 = 
  let rec inner tr l  =
    match l with
    h::t -> inner (insertp tr h) t
  |[] -> tr

  in inner tr1 (list_of_tree tr2)

type 'a mtree = 
  Br of 'a * 'a mtree list 
(* awful data structure*)
let myMtree = Br(7,[Br(10,[]);Br(12,[]);Br(13,[Br(16,[])])])

let rec sum l = 
  match l with
  [] -> 0
  |h::t -> h + sum(t)

let rec size trm =
  match trm with
  Br(x,l) -> 1 + sum(List.map size l) 

let rec total trm =
  match trm with
  Br(x,l) -> x + sum(List.map total l) 

let rec mapMtr f trm = 
  match trm with
  Br(x,l) -> Br(f x,List.map (mapMtr f) l)

