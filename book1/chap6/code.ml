(*this function will replace '!' with ','*)
let rep_ex l =

  let rec inner nl ls =
    match ls with
  [] -> nl
  |h::t ->
      if h = '!' then inner (nl @ ['.']) t 
      else inner (nl @ [h]) t 

  in inner [] l 

  let rec map f l =
    match l with
  [] -> []
  |h::t -> f h :: map f t

  let replace l = map (fun x -> if x = '!' then '.' else x) l

  let rec rep_ex' l =
    match l with
  [] -> []
  |'!'::t -> '.' :: rep_ex' t
  | h::t -> h :: rep_ex' t

  let replace' l = 
    let inner x =
      match x with
    '!' -> '.'
  | x -> x

  in map inner l
  (*vals are ints that will be clipped to range 1 to 10*)
  let clip vals =
    let inner x =
      if x < 1 then 1
    else if x > 10 then 10
      else x

    in map inner vals

  let clip' l = 
    map (fun x -> 
      if x > 10 then 10 
  else if x < 1 then 1 
      else x)
    l
(*apply given a funcion f, and num of times to apply, and initial default*)
let apply f num d =
  let start = num in
  let rec inner f n res =
    if n = start then inner f (n-1) (f d)
    else if n <> 0 then inner f (n-1) (f res)
    else res
  in inner f num 0

 let rec apply' f n d =
   if n =0 then d
   else f ( apply' f (n-1) d)

let rec filter cmp l =
  match l with
  [] -> []
  |h::t ->
      if cmp h then h :: filter cmp t
      else filter cmp t

let rec for_all cmp l =
  match l with
  [] -> true
  | h::t ->
      if (cmp h = false) then false
      else for_all cmp t
 
 let rec mapl f ll = 
   match ll with 
   [] -> []
  | h::t -> (map f h) :: mapl f t

