(* String of character *)
let string_of_char ch = Printf.sprintf "%c" ch

let implode lst = 
  let str = String.create (List.length lst) in
  let rec loop i = function [] -> str | x::xs -> String.set str i x; loop (i+1) xs in
    loop 0 lst

let explode str =
  let rec loop i acc =
    if i < 0 then acc else
      loop (i-1) ((String.get str i)::acc) in
    loop ((String.length str)-1) [] 

(* Quote string *)
let do_slashes str = 
  let lst = explode str in
  let rec loop acc = function
    | [] -> acc
    | '\\'::r -> loop (acc@['\\';'\\']) r
    | t::r -> loop (acc@[t]) r
  in
    implode (loop [] lst)

let quote str = "\"" ^ do_slashes str ^ "\""
let squote str = "'" ^ do_slashes str ^ "'"
