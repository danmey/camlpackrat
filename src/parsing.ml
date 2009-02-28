module Parsing = struct
exception Noparse 
let some f = function
  | [] -> raise Noparse 
  | h::t ->  if f h then (h,t) else raise Noparse


let (++) p1 p2 s = 
  let result1, rest1 = p1 s in
  let result2, rest2 = p2 rest1 in
    (result1,result2),rest2

let rec many p s =
  try
    let result, next = p s in
    let results,rest = (many p next) in
      result::results,rest
  with Noparse -> [],s


let (>>) p f s =
  let result,rest = p s in
    f result,rest

let (||) p1 p2 s =
  try p1 s
  with Noparse -> p2 s

type lex_token = Name of string | Number of string | Operator of string

let explode str =
  let rec loop i acc =
    if i < 0 then acc else
      loop (i-1) ((String.get str i)::acc) in
    loop ((String.length str)-1) [] 

let implode lst = 
  let str = String.create (List.length lst) in
  let rec loop i = function [] -> str | x::xs -> String.set str i x; loop (i+1) xs in
    loop 0 lst
    

let digit = some (fun a -> a >= '0' && a <= '9')
let alpha = some (fun a -> ((a >= 'A' && a <= 'Z') or (a >= 'a' && a <= 'z') or a == '-'or a == '+'))
let number = (digit ++ many digit) >> function a,b -> a::b
let is_white = (fun x -> x == ' ' or x == '\n' or x == '\t' or x == '\r')
let white = some is_white 
let whites = many white
	  

let t t = some (fun x -> t == x)
end
