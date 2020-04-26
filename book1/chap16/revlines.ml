(*reverse lines in a text files sending result to other file *)

let reverse s = 
  let len = String.length s in
  for i=0 to (len/2) do
    let t = s.[len-1-i] in
    s.[len-1-i] <- s.[i];
    s.[i] <- t;
  done;
  s

let rev_lines' inch outch = 

  try
    while true do 
      let line =  input_line inch in
      (*process line*)
      let r = reverse line in
      (*write to out channel*)
      output_string outch r;
      output_string outch "\n";
  done; 
  with
  End_of_file -> () 


let rev_lines infile outfile =
try
  let i = open_in infile in
  let o = open_out outfile in
  rev_lines' i o;
  close_in i;
  close_out o;
  with
  Sys_error e ->
    print_string "Could not open files"
