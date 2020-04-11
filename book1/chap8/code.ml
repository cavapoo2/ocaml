let num_of_keys d = 
  let rec inner dl nk =
    match dl with
    [] -> nk
    | (k,_)::t -> inner t (nk+1) 

  in inner d 0

let replace k v d = 
  let rec inner k' v' od =
    match od with 
    [] -> raise Not_found
    |(kc,vc)::t -> 
        if kc = k' then (k',v') ::t 
        else (kc,vc) :: inner k' v' t

  in inner k v d

let rec make_dict l1 l2 = 
  match (l1,l2) with 
  [],[] -> []
  |[],_ ->  raise (Invalid_argument "make_dict")  
  |_,[] -> raise (Invalid_argument "make_dict")
  |h1::t1,h2::t2 -> (h1,h2) :: make_dict t1 t2

let make_lists d =
  let rec inner d' k v =
    match d' with 
    [] -> (k,v) 
  |(k',v')::t -> inner t (k@[k']) (v@[v']) 

  in inner d [] []

let rec make_lists' d =
  match d with
  [] -> ([],[])
  |(k,v)::rest -> 
      let (ks,vs) = make_lists' rest in
      (k::ks,v::vs)

let rec lookup k l = 
  match l with 
  [] -> raise Not_found
  |(k',v)::t ->
      if k = k' then true else lookup k t 

let key_exists k l =
  try lookup k l
  with Not_found -> false

let make_dict_from_list l = 
  let rec inner nl ol =
  match ol with
  [] -> nl 
  |(k,v)::t -> 
      if (key_exists k nl) then inner nl t
      else inner (nl@[(k,v)]) t

  in inner [] l

let rec member v l = 
  match l with
  [] -> false
  | h::t ->
      if v = h then true
      else member v t

let rec make_dict_from_list_inner' seen l =
  match l with 
  [] -> []
  |(k,v)::t ->
      if (member k seen) then make_dict_from_list_inner' seen t
      else (k,v) :: make_dict_from_list_inner' (k::seen) t

let make_dict_from_list' l =
  make_dict_from_list_inner' [] l
(* much simpler way than this !!*)
let rec union_dict_inner seen d1 d2 =
  match (d1,d2) with 
  [],[] -> []
  |(k,v)::t,[] -> 
      if member k seen then union_dict_inner seen t d2
      else (k,v) :: union_dict_inner (k::seen) t d2
  |[],(k,v)::t ->
      if member k seen then union_dict_inner seen d1 t
      else (k,v) :: union_dict_inner (k::seen) d1 t
  |(k1,v1)::t1,(k2,v2)::t2 ->
      let k1seen = member k1 seen in 
      let  k2seen = member k2 seen in
      if (not k1seen) && (not k2seen) then
        begin
          let x = (k1,v1) ::[(k2,v2)] in
          let y = [k1;k2] in
          x @ union_dict_inner (y@seen) t1 t2
        end
      else if (not k1seen) && k2seen then
        (k1,v1) :: union_dict_inner (k1::seen) t1 t2
      else if k1seen && (not k2seen) then
        (k2,v2)::union_dict_inner (k2::seen) t1 t2
      else
        union_dict_inner seen t1 t2

let union_dict d1 d2 =
  union_dict_inner [] d1 d2

let rec add k v d = 
  match d with
  [] -> [(k,v)]
  |(k',v')::t ->
      (*replace key with new value if found*)
      if k' = k then 
        (k',v) :: t
      else
        (k',v') :: add k v t 

let rec union_dict' d1 d2 =
  match d1 with 
  [] -> d2 
  |(k,v)::t ->
      add k v (union_dict' t d2)
