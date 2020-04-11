let rec member x l =
  match l with 
  [] -> false 
  |h::t -> x = h || member x t

let member' x y = if x = y then true else false

let rec map f l = 
  match l with
  [] -> []
  |h::t ->
      f h :: map f t

let rec mapl f ll =
  match ll with
  [] -> []
  |h::t -> (map f h) :: mapl f t 

let mapl' f ll = map (map f) ll  
let member_all x ll =
  let contains = map (member x) ll in
  not (member false contains)

let divl x = map (fun v -> v/x)

let mapll f lll = map (map (map f)) lll 

let rec length l =
  match l with
  [] -> 0
  |h::t -> 1 + length t

let rec chop x l =
  let keep = (length l) -x in
  let rec inner c l' = 
    match l' with
    [] -> []
  |h::t ->
      if keep < 0 then l'
      else if c > 0 then h:: inner (c-1) t
      else []
  in inner keep l

  
let rec truncate x = (map (chop x)) 

let head d l = 
  match l with
  [] -> d
  |h::t -> h

let first_elements d = (map (head d))
