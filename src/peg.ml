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
		try
		  let r = f {current=current; memo_table=a_memo} in
		  let startp, endp = current, (snd r).current in
		    for i = startp to endp-1 do
		      a_memo.memo.(rule_id).(i) <- Remembered ((fst r), startp, endp)
		    done; 
		    r
		with Noparse -> 
		      a_memo.memo.(rule_id).(current) <- Failed; raise Noparse
	      end
	  | (Remembered(a,startp,endp)) -> a, {current=endp; memo_table=a_memo}
	  | _ -> raise Noparse
      in
	result
    with Invalid_argument a -> raise Noparse
      
let iterations = ref 0 ;;

let pmatch ch str =
    try 
      iterations := !iterations+1;
    let the_char = String.get str.memo_table.string (str.current) in
      if ch == the_char then
	[the_char], {str with current=str.current+1;}
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
  let new_result = f result in
      new_result, rest

let def_rule p rule_id str  = get_result str.memo_table str.current rule_id p

let empty str = [],str

let many1 p = seq p (many p)
let opt p = choice p empty

let followed p str =
  let _ = p str in
    [], str

let eof str = 
  if str.current >= String.length str.memo_table.string then
    [], str
  else raise Noparse

exception Not_followed

let not_followed p str =
  try
    p str;
    raise Not_followed
    with 
      | Noparse -> [], str
      | Not_followed -> raise Noparse
	

type ast = Let | Blank

let (>>=) = bind
let (</>) = choice
let (<++>) = seq
let t = pmatch_str 
let (<?>) p1 p2 = seq p1 (opt p2)
let (<*>) p1 p2 = seq (many p1) p2
let (<&>) p1 p2 = seq p1 (followed p2)
let (<!>) p1 p2 = seq p1 (not_followed p2)

let example =
  let end_rule = new_grammar() in
  let spaces = def_rule ((many ((t " ") </> (t "\t") </> (t "\n"))) >>= (fun x -> []))  (end_rule()) in 
  let keyword = def_rule ((t "let") <?> (t ";") >>= (fun _ -> [Let])) (end_rule()) in
  let keywords = def_rule (spaces <++> keyword <!> (t "begin") <*> empty <&> eof) (end_rule()) in 
    keywords 

let end_rule = new_grammar() 
let first = def_rule ((many1 (t "*")) <++> (many1 (t "a")) >>= (fun x -> [1])) (end_rule())
let second = def_rule ((many1 first) >>= (fun x -> [1])) (end_rule())
let third = def_rule (((many1 second) ) >>= (fun x -> [1])) (end_rule())



(*    

let exponential_hit = (Parsing.many (Parsing.many (Parsing.many (Parsing.t '*')))) (explode "*************************************************************************************************************************************************************a******************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************");;
*)
