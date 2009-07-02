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
  
(* String of character *)
let string_of_char ch = Printf.sprintf "%c" ch
