(*better to use for loop *)
let rec print_list l =
  match l with
  [] -> []
  |h::t -> print_int h; print_char ';';print_list t 

let print_list' l =
  print_string "[";
  List.iter (fun i -> print_int i; print_string ";") l;
  print_string "]" 

let rec print_list'' l =
  match l with
  [] -> ()
  |[x] -> print_int x
  |h::t -> print_int h;print_char ';';print_list'' t

  (*call like:
    * wrap_brackets (fun x -> print_list'' x) *)
let wrap_brackets f l =
  ignore (print_char '['); ignore (f l); ignore (print_char ']')

exception Failure
  (*warning here - code should not depend on the value of this 
   * constuctors arguemnts!
   * to run this do:
     * read3_ints();;*)
let rec read3_ints()= 
  try
    print_string "Type in 3 integers, pressing Enter afer each";
    print_newline();
    let i1 = read_int() in
    let i2 = read_int() in
    let i3 = read_int() in
    (i1,i2,i3)
  with
  Failure  ->
    print_string "This is not a valid integer try again";
    print_newline ();
    read3_ints()

let rec read_dict_number n = 
  if n = 0 then [] else
    try
      print_string "Now enter a number ";
      print_newline();
      let i = read_int() in 
      print_string "Now enter a string";
      print_newline();
      let name = read_line() in 
      (i,name) :: read_dict_number (n-1)
  with
    Failure  ->
      print_string "the number is not an int";
      print_newline();
      print_string "please enter the number and name again";
      print_newline();
      read_dict_number n

exception BadNumber

let rec read_dict () = 
  print_string "Enter the number of entries for dictionary";
  print_newline();
  try
    let n = read_int() in
    if n < 0 then raise BadNumber else read_dict_number n
    with
  Failure  ->
    print_string "the number entered is not an int";
    print_newline();
    read_dict()
  |BadNumber ->
      print_string "number entered is negative, try again";
      print_newline();
      read_dict()

let rec print_list l =
  match l with
  [] -> ()
  |h::t -> print_int h; print_string "\t" ;print_list t

let rec get_list from t =
  if from > t then [] else
    if from < t then from :: get_list (from +1) t
  else [t]

let rec row from step t = 
  if from > t then [] else
    if from < t then from :: row (from+step) step t
    else [t]
    (* garbage prefere for loops!*) 
let rec table f n c =
  if c =0 then [] else
    let r = row f f (f*n) in
    print_list r;
    print_newline();
    table (f+1) n (c-1)


let table' n = 
  for row = 1 to n do
    print_newline();
    for col = 1 to n do
      print_int (col*row); print_string "\t";
    done
    done

let rec make_row n = 
  match n with 
  0 -> []
  |_ -> make_row (n-1) @[n]
  (*call like:
    * table'' stdout 5 *)
let table'' ch n =
  List.iter (fun x ->
    List.iter (fun i ->
      output_string ch (string_of_int i);
      output_string ch "\t")
    (List.map(fun a -> a*x)(make_row n));
    output_string ch "\n")
  (make_row n)


let table''' n =
  List.iter (fun x ->
    List.iter (fun i ->
      print_string (string_of_int i);
      print_string "\t")
    (List.map(fun a -> a*x)(make_row n));
  print_string "\n")
  (make_row n)


let rec count_lines ch c =
  try
    let _ = input_line ch in
    count_lines ch (c+1)
  with
  End_of_file -> print_string "Number of lines are "; print_int c

let line_count filename =
  let ch = open_in filename in
  let lines = count_lines ch 0 in
  close_in ch;
  lines

let rec copy ich och =
  try
    let s = input_line ich in
    output_string och s;
    output_string och "\n";
    copy ich och
  with
  End_of_file -> ()

exception CopyFailedHAHA
let copy_file file1 file2 = 
  try
    let i = open_in file1 in
    let o = open_out file2 in
    (*do the copying*)
    copy i o;
    close_in i;
    close_out o
  with
  _ -> raise CopyFailedHAHA 

