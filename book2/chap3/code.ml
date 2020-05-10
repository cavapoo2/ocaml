type 'n info = {x: float; mutable name : 'n;}

let q = {x = 1.1; name = "fred"} 

let _ = q.name <- "john"

let weekday num = 
  let d=
    [|"Sunday";"Monday";"Tuesday";"Wednesday";"Thursday";"Friday";"Saturday"|]
  in
  d.(num mod 7)

let month num = 
  let m =
    [|"January";"February";"March";"April";"May";"June";"July";"August";"September";"October";"November";"December"|]
  in
    m.(num mod 12)

(*this is not a function, just a variable with scope in this file
 *)
let date_info  =
let t =  Unix.localtime(Unix.gettimeofday()) in
let min = t.tm_min in 
let day = (weekday t.tm_wday) in
let dm = t.tm_mday in
let ms = (month t.tm_mon) in 
let yr = (1900 + t.tm_year) in
let hr =  (t.tm_hour mod 12 ) in
(min,day,dm,ms,yr,hr)

let min (m,_,_,_,_,_) = m 
let day (_,m,_,_,_,_) = m
let dm (_,_,m,_,_,_) = m
let ms (_,_,_,m,_,_) = m
let yr (_,_,_,_,m,_) = m
let hr (_,_,_,_,_,m) = m

(*2:45 on Wednesday 8 January 2014*)
(* seems like MUST include the () after function name ,
 * signifies a function that takes no arguemnts*)
let show_date_info () =
Printf.printf "It is %d:%d on %s %d %s %d\n" (hr date_info) (min date_info) (day date_info) (dm
date_info) (ms date_info) (yr date_info);
0

let show_date_info' () = 
  print_string "It is ";
  print_int (hr date_info);
  print_string ":";
  print_int (min date_info);
  print_string " on ";
  print_string (day date_info);
  print_string " ";
  print_int (dm date_info);
  print_string " ";
  print_string (ms date_info); print_string " ";
  print_int (yr date_info);
  print_newline()


let string_of_month m =
  match m with
    0 -> "January"
  | 1 -> "February"
  | 2 -> "March"
  | 3 -> "April"
  | 4 -> "May"
  | 5 -> "June"
  | 6 -> "July"
  | 7 -> "August"
  | 8 -> "September"
  | 9 -> "October"
  | 10 -> "November"
  | 11 -> "December"
  | _ -> raise (Invalid_argument "string_of_month")

let string_of_day d =
  match d with
    0 -> "Sunday"
  | 1 -> "Monday"
  | 2 -> "Tuesday"
  | 3 -> "Wednesday"
  | 4 -> "Thursday"
  | 5 -> "Friday"
  | 6 -> "Saturday"
  | _ -> raise (Invalid_argument "string_of_day")

(*in uptop call like this :
  * string_of_time ();;*)
let string_of_time () =
  let
    {Unix.tm_min;
     Unix.tm_hour;
     Unix.tm_mday;
     Unix.tm_mon;
     Unix.tm_year;
     Unix.tm_wday}
  =
    Unix.localtime (Unix.time ())
  in
      "It is " 
    ^ string_of_int tm_hour
    ^ ":"
    ^ string_of_int tm_min
    ^ " on "
    ^ string_of_day tm_wday
    ^ " "
    ^ string_of_int tm_mday
    ^ " "
    ^ string_of_month tm_mon
    ^ " "
    ^ string_of_int (tm_year + 1900)

(*show some stats of garbage collector*)
let garbage_state' outch = 
  let g = Gc.quick_stat() in
  output_string outch "Garbage Data\n:";
  output_string outch (string_of_int g.heap_words);
  output_string outch "\nEnd"

let garbage_state outfile = 
  try 
    let o = open_out outfile in
    garbage_state' o;
    close_out o;
  with
  Sys_error e ->
    print_string "Could not open files"
