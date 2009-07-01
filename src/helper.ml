type ast = Choice of ast * ast | Cat of ast list | Literal of string | Rule of string | Many of ast | Transform of string * ast
and prule = {rule_id:string; rule_type:string; rule_body:ast;}    

let implode lst = 
  let str = String.create (List.length lst) in
  let rec loop i = function [] -> str | x::xs -> String.set str i x; loop (i+1) xs in
    loop 0 lst

let explode str =
  let rec loop i acc =
    if i < 0 then acc else
      loop (i-1) ((String.get str i)::acc) in
    loop ((String.length str)-1) [] 
