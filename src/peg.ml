(* type 'a memo_type = Remembered of 'a * int * int | NotYet | Failed *)
(* type 'a memo = { string:string; memo:'a memo_type array array } *)
(* type 'a stream = { current:int; memo_table:'a memo } *)
(* exception Noparse *)

(* let init_memo str = {string=str; memo=Array.create 1 (Array.create (String.length str) NotYet)} *)

(* let get_result memo current rule_id f = *)
(*   let num_rules = Array.length memo.memo in *)
(*   let a_memo = if rule_id >= num_rules then *)
(*     let num_difference = rule_id - (num_rules-1) in *)
(*     let new_arrays = Array.create num_difference (Array.create (String.length memo.string) NotYet) in *)
(*     let new_memo_arrays = Array.append memo.memo new_arrays in *)
(*       {memo with memo=new_memo_arrays} *)
(*   else memo in *)
(*   let result =  *)
(*     let a_rule = Array.get a_memo.memo rule_id in *)
(*       match Array.get a_rule current with *)
(* 	| NotYet -> f rule_id {current=current; memo_table=a_memo}  *)
(* 	| (Remembered(a,startp,endp)) -> a, {current=endp; memo_table=a_memo} *)
(* 	| _ -> raise Noparse *)
(*   in *)
(*     result *)
      

(* let pmatch  ch rule_id str =  *)
(*   let the_char = String.get str.memo_table.string str.current in *)
(*     if the_char == ch then *)
(*       [ch], {str with current=str.current+1;} *)
(*     else raise Noparse *)

(* let choicei p1 p2 rule_id str = *)
(*   try p1 rule_id str with *)
(*     Noparse -> p2 rule_id str *)

(* let choice p1 p2 rule_id str = get_result str.memo_table str.current rule_id (choicei p1 p2) *)

type 'a memo_type = Remembered of 'a * int * int | NotYet | Failed
type 'a memo = { string:string; memo:'a memo_type array array }
type 'a stream = { current:int; memo_table:'a memo }
exception Noparse

let init_memo str = {string=str; memo=Array.create 1 (Array.create (String.length str) NotYet)}
let make_stream str = {current=0; memo_table=init_memo str}
let get_result memo current rule_id f =
   
let num_rules = Array.length memo.memo in
  try
    let a_memo = if rule_id >= num_rules then
      let num_difference = rule_id - (num_rules-1) in
      let new_arrays = Array.create num_difference (Array.create (String.length memo.string) NotYet) in
      let new_memo_arrays = Array.append memo.memo new_arrays in
	{memo with memo=new_memo_arrays}
    else memo in
    let result = 
      let a_rule = Array.get a_memo.memo rule_id in
	match Array.get a_rule current with
	  | NotYet ->   let r = f rule_id {current=current; memo_table=a_memo} in
	    let an_array = (Array.get a_memo.memo rule_id) in
	      Array.set an_array current (Remembered(fst r, current, (fst r).current));
	      Array.set a_memo.memo rule_id an_array;
	      r
	  | (Remembered(a,startp,endp)) -> a, {current=endp; memo_table=a_memo}
	  | _ -> raise Noparse
  in
      result
 with Invalid_argument a -> raise Noparse
      

let pmatch ch rule_id str = 
  try 
    let the_char = String.get str.memo_table.string str.current in
    if the_char == ch then
      [ch], {str with current=str.current+1;}
    else raise Noparse
  with Invalid_argument a -> raise Noparse

let choicei p1 p2 rule_id str =
  try p1 rule_id str with
    Noparse -> p2 rule_id str

let choice p1 p2 rule_id str = get_result str.memo_table str.current rule_id (choicei p1 p2)

let rec manyi p rule_id s =
  try
    let result, next = p rule_id s in
    let results,rest = (manyi p rule_id next ) in
      result@results,rest
  with Noparse -> [],s

let many p rule_id str = get_result str.memo_table str.current rule_id (manyi p)

