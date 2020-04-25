(*not sure why its needs to be reversed like this *)
let list_concat l = 
  let rec inner ls rs = 
    match ls with
    h::t -> inner t (List.rev h@rs) 
    | [] -> List.rev rs

  in inner l []

let rec contains a ll = 
  match ll with
  h::t -> if (List.mem a h) = true
  then contains a t
          else false
    |[] -> true

let count_char c s = 
  let len = String.length s in
  let count = ref 0 in
  for i = 0 to (len-1) do
    if s.[i] = c then count := !count +1
    else count := !count
  done;
  count

  (*another way*)

let count_char' c s = 
  let rec inner c' s' i r =
    if i < 0  then r
    else 
      begin
        if s'.[i] = c' then inner c' s' (i-1) (r+1)
        else inner c' s' (i-1) r
      end 
  in inner c s (String.length s -1) 0

let replace_if c t r = 
  if c = t 
  then r else c

let replace_in_string s r w = 
  String.map (fun c -> 
    replace_if c r w) s

let replace_in_string' s r w = 
  String.map (fun c -> 
    if c = r then w else c) s

    (*concat list of strings*)
let concat_string sep ls = 
  String.concat sep ls

  (*use buffer lib to concat strings*)
let concat_string' sep ls = 
  let buf = Buffer.create 16 in
  let rec inner rl = 
    match rl with
    [h] -> 
      Buffer.add_string buf h; 
    |h::t ->
      Buffer.add_string buf h;
      Buffer.add_string buf sep;
      inner t 
    |[] -> () 
  in inner ls ;
  Buffer.contents buf

(*better way concating buffer *)
let concat_string'' ls = 
  let buf = Buffer.create 100 in
  List.iter (fun s -> Buffer.add_string buf s) ls;
  Buffer.contents buf
(*count occrances of substring *)
let count_occurances sub str = 
    let len = (String.length str) in
    let lensub = String.length sub in
    let count = ref 0 in
    for i =0 to (len - lensub) do
      let s = String.sub str i lensub in
      if (String.compare s sub) = 0 then count := !count +1 else count :=
        !count

    done;
    !count
    








