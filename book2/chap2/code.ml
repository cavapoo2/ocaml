type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

let rec llist n = 
  Cons(n, fun () -> llist (n*2))

let rec llist' n = 
  Cons(n, fun () -> llist' (n+1))

let rec llist_rep' r l = 
  match r with
 [] -> raise (Invalid_argument "repeating empty list")
| [x] -> Cons(x, fun () -> llist_rep' l l)
| h::t -> Cons(h,fun () -> llist_rep' t l)

let llist_rep l = llist_rep' l l


  (*lazy list 
   * call as : ltake (llist 1) 10 *)
let rec ltake (Cons(h,tf)) n = 
  match n with
  0 -> []
|_ -> h :: ltake(tf ()) (n-1)

(*fibonnaci lazy list*)
let rec fib' p2 p1 = 
  Cons(p2+p1, fun () -> fib' p1 (p1+p2)) 

let fib = fib' 0 1

(*unleave lazy list *)

let rec unleave (Cons(h,tf)) =
  let Cons(h',tf') = tf() in
  let t = tf'() in
  (Cons(h,fun () -> fst (unleave t) ),Cons(h',fun () -> snd (unleave t)))

  (* ltake2 (unleave (list' 0)) 20 *)
let rec ltake2 (Cons(h,tf),Cons(h',tf')) n = 
  match n with
  0 -> []
|_ -> (h,h') :: ltake2(tf (),tf' ()) (n-1)

let dec_to_bin x =
  let rec inner v s = 
    if v > 0 then
      let q = v /2 in
      let r = v mod 2 in
      inner q ((string_of_int r) ::  s)
    else
      s
      in if x = 0 then ["0"] else
        inner x [] 

        (* for n > 0 *)
let num_to_string n = 
  let x = 65 + n in
  Char.escaped (Char.chr x)

let char_to_num c = 
  (Char.code c) - 65

(*unusual number system excel columns, for
 * example in base 10 it goes
 * 0,1,2,3,4,5,6,7,8,9,10,11,12...
 * whereas in excel it goes
 * A,B,C,D,E,F...AA,AB
 * meaning in base 10,the zero only starts (leads)
 * once from 9 to 10 the 1 leads, it does not go 00,01,01
 * hence code below accounts for that irregularity*)

  (* dec to base 26 convertor *)
let int_to_b26 n = 
  let rec inner v s = 
    if v < 26 then
      (num_to_string (v-1)) :: s
      else
        let r = (v) mod 26 in
        let q = (v) / 26 in
        inner q ((num_to_string (r))::s)
        in if n < 0 then []
        else if n < 26 then [num_to_string n]
        else inner n []

(* convert a excel code to int*)
let b26_to_int s =  
  let len = String.length s in
  if len = 1 then
    char_to_num s.[0]
  else
    begin
  let b = ref 26 in
  let sum = ref 0 in
  let u = char_to_num s.[len-1] in  
  sum := !sum + u;
  for i =len-2 downto 0 do
    let v = char_to_num s.[i] +1 in
    sum := !sum + (!b* v);
    b := !b * 26  
  done;
  !sum
    end

let rec lmap f (Cons(x,tf)) = 
  Cons( f x, fun () -> lmap f (tf()))

let excel_cols' = lmap int_to_b26 (llist' 0)
(*how do i get lazy out to show more values, 
 * seems to be a limit with how much is shown*)
let excel_cols n = ltake excel_cols' n


