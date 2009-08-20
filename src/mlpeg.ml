type 'a result = Success of int * 'a | Fail | Lr of bool * int

let make_stream_generator f str = 
  let i = ref 0 in
    (fun () ->
       if !i < String.length str then
	 let v = (String.get str !i) in
           i:=!i+1;
           Some(f v)
       else None)
;;

let rec grow_lr stream getter setter r __memo =
  let flag = ref true in
    while !flag do
      Mstream.push stream;
      let p = Mstream.spos stream in
	try 
	  let ans = r stream in
	    Mstream.pop stream;
	    match ans with
	      | Fail ->  flag := false 
	      | Success (p2,a) -> setter __memo (Some ans)
	with Mstream.End_of_stream -> Mstream.pop stream; flag := false
    done;
    let Some a = getter __memo in a
and
    apply_rule stream getter setter r =
  let __memo = (Mstream.top stream) in
    match getter __memo with
      | None -> 
	  begin
	    Mstream.push stream;
	    setter __memo (Some (Lr (false, (Mstream.spos stream))));
	    let ans = r stream in
	      Mstream.pop;
	      match getter __memo with
		| Some(Lr (_,p)) -> setter __memo (Some ans); grow_lr stream getter setter r __memo
		| Some(a) -> a
	  end
      | Some a -> 
	  match a with
	    | Lr (_,p) -> (Mstream.advance stream p); setter __memo (Some (Lr (true, Mstream.spos stream))); 
		Fail
	    | Success (p,r) -> Mstream.advance stream p; a
	    | Fail -> Fail

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

(* String of character *)
let string_of_char ch = Printf.sprintf "%c" ch

let _ = 
  if Array.length Sys.argv > 1 then
    parse options parse_file usage_text
  else usage options usage_text

