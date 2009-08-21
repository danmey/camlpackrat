open Arg 

let options = []
let usage_text = "mlpeg <options> <files*>"

let parse_file f = 
  let read_file file =
    let f = open_in file in
    let rec loop acc =
      try
        loop (acc ^ "\n" ^ input_line f)
      with End_of_file -> acc
    in loop ""
  in
      Printf.printf "%s" (Mlcodegen.gen_code (read_file f)) 

(* String of character *)
let string_of_char ch = Printf.sprintf "%c" ch

let _ = 
  if Array.length Sys.argv > 1 then
    parse options parse_file usage_text
  else usage options usage_text

