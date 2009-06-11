
(*
let match_token bind stream = 
  let pos = Stream.pos stream in
  if string.[0] = (Stream.next stream).base then
    if string.[1] = (Stream.next stream).base then
      if string.[2] = (Stream.next stream).base then
          Stream.set pos stream (Remembered (bind (implode string)))
       else Stream.set pos stream Failed; ParseFail
     else Stream.set pos stream Failed; ParseFail
  else Stream.set pos stream Failed; ParseFail
*)

let id() = ""    
let rec tabs n = if n > 0 then Printf.sprintf "\t" ^ tabs (n-1) else ""

let gen_if lh rh body el indent =
    tabs indent ^ "if " ^ lh indent ^ " = " ^ rh indent ^ " then\n" 
  ^ body (indent+1) ^ tabs indent 
  ^ "else " ^ el (indent+1) ^ "\n"
(*
  "match_token bind stream =\n" ^
    "Btstream.push stream\n" ^
*)
let implode lst = 
  let str = String.create (List.length lst) in
  let rec loop i = function [] -> str | x::xs -> String.set str i x; loop (i+1) xs in
    loop 0 lst

let explode str =
  let rec loop i acc =
    if i < 0 then acc else
      loop (i-1) ((String.get str i)::acc) in
    loop ((String.length str)-1) [] 

let indented_string str n = tabs n ^ str
let just_string str _  = str

let rec gen_token_match indent = function
  | x::xs -> gen_if 
      (just_string (Printf.sprintf "'%c'" x))
	(just_string "(next stream).base")
	(fun indent -> (gen_token_match indent xs)) 
	(just_string "()") indent
  | [] -> ""


