let smallest l =
  let rec inner lr s found =
    match lr with
  [] -> 
    if found then s else raise Not_found
  |h::t -> 
      if h < s && h >= 0 then inner t h true
      else inner t s found
  in inner l max_int false 

let smallest_or_zero l =
  try (smallest l) with
   Not_found -> 0

exception Complex

let largest_integer_of_sqrt i =
 if i < 0 then raise Complex
 else int_of_float (sqrt (float_of_int i))   

let largest_integer_of_sqrt' i =
  try largest_integer_of_sqrt i with
  Complex -> 0
