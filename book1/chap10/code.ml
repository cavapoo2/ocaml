type rect =
  Square of int
|Rect of int * int

let area r = 
  match r with
  Square x -> x * x
  |Rect (x,y) -> x*y


let rot r =
  match r with 
  Square _ -> r
  | Rect (w,h) ->
      if w > h then Rect (h,w)
      else r

let rotl = List.map rot 

let get_width rect =
  match rect with
  Square s -> s
  |Rect(w,_) -> w

let comp x y =
  if get_width x < get_width y then -1
  else if get_width x > get_width y then 1
  else 0


let sorted l =
  let r =  List.map rot l
  in List.sort (fun x y -> comp x y) r 

type 'a sequence = Nil | Cons of 'a * 'a sequence

let rec take n s = 
  if n = 0 then Nil else
    match s with 
    Nil -> raise (Invalid_argument "take")
  |Cons(a,b) -> Cons(a, take (n-1) b)

let rec drop n s = 
  if n = 0 then s else
    match s with 
  Nil -> raise (Invalid_argument "drop")
  | Cons(_,b) -> drop (n-1) b   

let rec map f s = 
  match s with
  Nil -> Nil
  |Cons(a,b) -> Cons(f a,map f b)

let compose f g = fun x -> f (g x)

let rec power x n =
  match n with
  0 -> 1
  |_ -> x * power x (n-1)

let power' x n =
  let rec inner c r =
    match c with
    0 -> r
  |_ -> inner (c-1) (r*x)

  in inner n 1 

type expr =
  Num of int
  |Add of expr * expr
  |Subtract of expr * expr
  |Multiply of expr * expr
  |Divide of expr * expr
  |Power of expr * expr

(*call like this
 * evaluate (Add(Num 3, Num 5));;*)

let rec evaluate e =
  match e with
  Num x -> x
  |Add (x,y) -> evaluate x + evaluate y
  |Subtract (x,y) -> evaluate x - evaluate y
  |Multiply(x,y) -> evaluate x * evaluate y
  | Divide(x,y) -> evaluate x / evaluate y
  |Power(x,y) -> power' (evaluate x) (evaluate y)

let evaluate' e = 
  try Some (evaluate e) with
  Division_by_zero -> None  
