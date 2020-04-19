let round x = 
  let c = ceil(x) in
  let f = floor(x) in
  if (x -. f) < (c -. x) then f else c

(* call like midpoint (1.,10.) (-7.,6.) *)
let midpoint (x, y) (x', y') =
  ((x +. x') /. 2., (y'+. y) /. 2.) 

(*splits the float to integer and fractional parts*)
let rec splitfloat x =
  if x < 0. then
    begin
    let a,b = splitfloat (-.x) in
    (-.a,b)
    end
  else
    begin
  let i = floor(x) in
  let f = (x -. i) in
  (i,f)
    end

(*star, if 0 then prints start in column 1, if 1 then prints starr in column
 * 50*)
let star x =
  if x <= 1. then
    let c = int_of_float(round(x *. 50.)) in 
    for i =0 to 50 do
      if i = c then
        print_string "*"
      else
        print_string " "
    done;
  else
    print_string "Number need to be between 0 and 1"

(* f is function to plot, r is range, s is step size*)
let plot f r1 r2 s =
  let steps = int_of_float((r2 -. r1) /. s) in
  for i = 0 to steps do
    let v = f ( float_of_int(i) *. s) in
    star v;
    print_string "\n";
  done


