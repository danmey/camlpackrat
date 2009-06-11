let read_file file =
  let f = open_in file in
  let rec loop acc =
      try
        loop (acc ^ "\n" ^ input_line f)
      with End_of_file -> acc
  in loop ""
      
let _ =
 for i = 1 to Array.length Sys.argv - 1 do
   Printf.printf "%s" (Mlcodegen.gen_code (read_file Sys.argv.(i))) 
  done

