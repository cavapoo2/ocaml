(* result 6 of course*)
let x = ref 1 in let y = ref 2 in x := !x + !x; y := !x + !y; !x + !y;;

let z = ref 5 in [z;z];;
(*call this like forloop print_int 1 10 *)
let rec forloop f n m =
  if n <= m then
    begin
      f n;
      forloop f (n+1) m
    end
;;
(*int array *)
[|1;2;3|];;
(*bool array *)
[|true;false;true|];;
(*int array array *)
[|[|1|]|];;
(* int array array *)
[|
  [|4;5;6|];
  [|7;8;9|]
|];;

Array.make 3 (Array.make 3 0);;
(*int list array*)
[|[1;2;3];[4;5;6]|];;

(*get index 2 which is 3*)
[|1;2;3|].(2);;

(* *)
[|1;2;3|].(2) <- 4;;

(*writes 4 to index 2 *)
let j = [|1;2;3|];;
j.(2) <-4;;
j;;

(* sum array *)
let sum arr = 
  let len = Array.length arr in
  let r = ref 0 in
  for i = 0 to (len-1) do
      r := !r + arr.(i);
  done;
    !r

let reverse arr = 
  let len = (Array.length arr) in
  for i = 0 to (len/2) do
    begin
      let j = (len-1) - i in
      let t = arr.(j) in
      arr.(j) <- arr.(i);
      arr.(i) <- t;
    end
  done;
  arr

let table n = 
  let m = Array.make n (Array.make n 0) in
  for i =0 to n-1 do
    let r = Array.make n 0 in
    for j =0 to n-1 do
      r.(j) <- (i+1)*(j+1);
    done;
    m.(i) <- r;
  done;
  m

let upper_case c =
  let i = int_of_char c in
  if (i >= 97) && (i <= 122) then
    let v = (i - 32) in
    let u = char_of_int v in
    u
  else
    c
let lower_case c =
  let i = int_of_char c in
  if (i >= 65) && (i <= 90) then
    let v = (i + 32) in
    let u = char_of_int v in
    u
  else
    c
    

