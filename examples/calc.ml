open Calcparser
let read_file file =
  let f = open_in file in
  let rec loop acc =
      try
        loop (acc ^ input_line f ^ "\n")
      with End_of_file -> acc
  in loop ""
      
let _ = 
  print_endline "Mlpeg parser example, simple calculator.";
  while true do
    print_string "Type in expression: ";
    match (parse (read_line()^" ")) with
	Mlpeg.Success ( p , v) -> Printf.printf "The result it: %s\n" v
      | _ -> print_endline "Parsing failed!"
  done
