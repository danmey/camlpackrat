type 'a memo_type = Remembered of 'a * int * int | NotYet | Failed
type 'a memo = { string:string; memo:'a memo_type array array }
type 'a stream = { current:int; memo_table:'a memo }
exception Noparse

let init_memo str = {string=str; memo=Array.make_matrix 1 (String.length str) NotYet}
let make_stream str = {current=0; memo_table=init_memo str}

let get_result memo current rule_id f =
  let num_rules = Array.length memo.memo in
    try
      let a_memo = if rule_id >= num_rules then
	let num_difference = rule_id - (num_rules-1) in
	let new_matrix = Array.make_matrix  num_difference (String.length memo.string) NotYet in
	let new_memo_arrays = Array.append memo.memo new_matrix in
	  {memo with memo=new_memo_arrays}
      else memo in
      let result = 
	match a_memo.memo.(rule_id).(current) with
	  | NotYet ->   
	      begin 
		let r = f {current=current; memo_table=a_memo} in
		  a_memo.memo.(rule_id).(current) <- (Remembered((fst r), current, (snd r).current));
		  r
	      end
	  | (Remembered(a,startp,endp)) -> a, {current=endp; memo_table=a_memo}
	  | _ -> raise Noparse
      in
	result
    with Invalid_argument a -> raise Noparse
      

let pmatch ch str = 
  try 
    let the_char = String.get str.memo_table.string str.current in
    if the_char == ch then
      [ch], {str with current=str.current+1;}
    else raise Noparse
  with Invalid_argument a -> raise Noparse

let choice p1 p2 str =
  try p1 str with
    Noparse -> p2  str

let rec many p s =
  try
    let result, next = p s in
    let results,rest = many p next in
      (result@results),rest
  with Noparse -> [],s

let rec seq p1 p2 s =
    let result, next = p1 s in
    let result2,rest = p2 next in
      (result@result2, rest)

(*
let many p rule_id str = get_result str.memo_table str.current rule_id (manyi p)
let seq p1 p2 rule_id str = get_result str.memo_table str.current rule_id (seqi p1 p2)
*)

let explode str =
  let rec loop i acc =
    if i < 0 then acc else
      loop (i-1) ((String.get str i)::acc) in
    loop ((String.length str)-1) [] 

(* Slow thing *)
let pmatch_str string =
  let char_lst = explode string in
  match char_lst with
    | [] -> raise Noparse
    | a::[] -> pmatch a 
    | lst -> begin
	let rec loop = function
	  | [] -> raise Noparse
	  | x::[] -> pmatch x
	  | x::xs -> seq (pmatch x) (loop xs)
	in (loop lst) 
      end

let new_grammar()  = 
   let n = ref 0 in 
     (fun () -> n := !n+1; let a = !n in a);;

let parse_grammar grammar s =
  let stream = make_stream s in
    fst (grammar stream)

let bind p f str = 
  let result, rest = p str in
    [f result], rest

let def_rule p rule_id str  = get_result str.memo_table str.current rule_id p

type ast = Let | Blank

let example =
  let grammar = new_grammar() in
  let spaces = def_rule (bind (many (choice (pmatch ' ') (choice (pmatch '\t') (pmatch '\n')))) (fun x -> []))  (grammar()) in 
  let keyword = def_rule (bind (pmatch_str "let") (fun _ -> [Let])) (grammar()) in
  let keywords = def_rule (many (seq spaces keyword)) (grammar()) in 
    keywords
 
