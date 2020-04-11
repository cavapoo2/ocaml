let not' x =
  match x with 
  true -> false
  | false -> true

let rec sum n = 
  match n with 
  0 -> 0
  | _ -> n + sum (n-1)

let rec pow x n =
  match n with
  0 -> 1
  | _ -> x * pow x (n-1)

 let islower c =
   match c with
   'a'..'z' -> true
  | _ -> false
