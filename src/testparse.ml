open Packratparser
let read_file file =
  let f = open_in file in
  let rec loop acc =
      try
        loop (acc ^ input_line f ^ "\n")
      with End_of_file -> acc
  in loop ""
      

let _ = 
  while true do
    match (parse (read_line()^" ")) with
	Mlpeg.Success ( _ , v) -> Printf.printf "=%s\n" v
      | _ -> print_endline "Failed!"
  done
