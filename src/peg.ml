type 'a memo_type = Remembered of 'a * int * int | NotYet | Failed
type 'a memo = { string:string; }
type 'a stream = { current:int; memo_table:'a memo } 

exception Noparse of string

let memo_table2 = ref (Array.make_matrix 100 2 (Remembered (['a';'b'], 0, 0)))
let init_memo str = 
  memo_table2 := Array.make_matrix 500 (2*(String.length str)) NotYet;
  {string=str;}
let make_stream str = let _ = init_memo str in {current=0; memo_table={string=str}}


let stream_to_string str = String.sub 
  str.memo_table.string str.current ((String.length str.memo_table.string) - str.current)
let raise_parse_error str = raise (Noparse (stream_to_string str))

let get_result memo current rule_id f =


(*      let a_memo = if rule_id >= num_rules then
	let num_difference = rule_id - (num_rules-1) in
	let new_matrix = Array.make_matrix  num_difference (String.length memo.string) NotYet in
	let new_memo_arrays = Array.append memo.memo new_matrix in
(*	let _ = Printf.printf "%d %d\n" (Array.length new_memo_arrays) (Array.length new_memo_arrays.(1)) in *)
	{memo with memo=new_memo_arrays}
	
	else memo in
*)  
  let result = 
    match !memo_table2.(rule_id).(current) with
      | NotYet ->  
	  begin
		try
		  let r = f {current=current; memo_table=memo} in
		  let startp, endp = current, (snd r).current in
		    for i = startp to endp-1 do
		      !memo_table2.(rule_id).(i) <- Remembered ((fst r), startp, endp);
		    done;  
		    r
		with Noparse a -> 
		      !memo_table2.(rule_id).(current) <- Failed;
		  raise (Noparse a)
	      end
	  | (Remembered(a,startp,endp)) -> 		      
	      Printf.printf "Found! %d\n" rule_id;
	      a, {current=endp; memo_table=memo}
	  | _ -> raise_parse_error {current=current; memo_table=memo}
      in
	Printf.printf "%d %d\n" current rule_id;
	result
let iterations = ref 0 ;;

let pmatch_pred f str =
    try 
      iterations := !iterations+1;
    let the_char = String.get str.memo_table.string (str.current) in
      if f the_char then
	[the_char], {str with current=str.current+1;}
      else (raise_parse_error str)
  with Invalid_argument a -> (raise_parse_error str)
 
let pmatch ch = pmatch_pred (fun x -> x == ch)

let any str = pmatch_pred (fun _ -> true) str
 
let choice p1 p2 str =
  try p1 str with
    Noparse _ -> p2  str

let rec many p s =
  try
    let result, next = p s in
    let results,rest = many p next in
      (result@results),rest
  with Noparse _ -> [],s

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
let pmatch_str op string =
  let char_lst = explode string in
  match char_lst with
    | [] -> raise (Invalid_argument "")
    | a::[] -> pmatch a 
    | lst -> begin
	let rec loop = function
	  | [] -> raise (Invalid_argument "")
	  | x::[] -> pmatch x
	  | x::xs -> op (pmatch x) (loop xs)
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

let def_rule p rule_id   = (fun str -> (get_result str.memo_table str.current rule_id p))
(*let def_rule p rule_id   =  p*)

let empty str = [],str

let many1 p = seq p (many p)
let opt p = choice p empty

let followed p str =
  let _ = p str in
    [], str

let eof str = 
  if str.current >= String.length str.memo_table.string then
    [], str 
 else (raise_parse_error str)

exception Not_followed

let not_followed p str =
  try
    p str;
    raise Not_followed
    with 
      | Noparse _ -> [], str
      | Not_followed -> (raise_parse_error str)
	


let (>>=) = bind
let (</>) = choice
let (<++>) = seq
let t str = pmatch_str seq str
let r str = pmatch_str choice str
let (<?>) p1 p2 = seq p1 (opt p2)
let (<*>) p1 p2 = seq (many p1) p2
let (<&>) p1 p2 = seq p1 (followed p2)
let (<!>) p1 p2 = seq p1 (not_followed p2)

let implode lst = 
  let str = String.create (List.length lst) in
  let rec loop i = function [] -> str | x::xs -> String.set str i x; loop (i+1) xs in
    loop 0 lst

(*
  let example =
  let end_rule = new_grammar() in
  let spaces = def_rule ((many ((t " ") </> (t "\t") </> (t "\n"))) >>= (fun x -> []))  (end_rule()) in 
  let keyword = def_rule ((t "let") <?> (t ";") >>= (fun _ -> [Let])) (end_rule()) in
  let keywords = def_rule (spaces <++> keyword <!> (t "begin") <*> empty <&> eof) (end_rule()) in 
    keywords 
*)
