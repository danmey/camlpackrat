type 'a result = Success of int * 'a | Fail

let make_stream_generator f str = 
  let i = ref 0 in
  (fun () ->
     if !i < String.length str then
       let v = (String.get str !i) in
         i:=!i+1;
         Some(f v)
     else None)
;;

let parse f string arule =
  let gen = make_stream_generator f string in
    arule (Mstream.of_generator gen)

let implode lst = 
  let str = String.create (List.length lst) in
  let rec loop i = function [] -> str | x::xs -> String.set str i x; loop (i+1) xs in
    loop 0 lst
  
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

let _ = 
  if Array.length Sys.argv > 1 then
    parse options parse_file usage_text
  else usage options usage_text
