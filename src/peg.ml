module Peg = struct
  type 'a memo_result = {poss:int; pose:int; result:'a}
  type 'a memo_slot = NotYet | Remembered of 'a memo_result | Failed
  type 'a stream = ('a  array array * int)
  exception Noparse 

    
  let make_stream str = 
    let s = Array.make 10 (Array.make (String.length str) NotYet) in
    let rec loop i =
      if i < 0 then s,0 else begin
	let chr = (String.get str i) in
	  s.(0).(i) <- (Remembered {poss=i;pose=i+1;result=chr}); 
	  loop (i-1)  
      end
    in loop ((String.length str)-1) 
  
  let get_memo stream combinator rule_id = 
    let ar, position = stream in
    match ar.(rule_id).(position) with
      | Remembered a -> a
      | NotYet -> let result = combinator stream in 
	  Array.set ar.(position) rule_id (Remembered(result)); result
      | Failed -> raise Noparse ;;

  let end_of_stream = function str,pos -> Array.length str.(0) <= pos 

  let get_result str rule_id = 
    let s,pos = str in
      if not (end_of_stream str) then match s.(rule_id).(pos) with
	| Remembered r -> r
	| _ -> Printf.printf "bala\n";raise Noparse 
      else raise Noparse 

  let advance = function str, pos -> (str, pos+1)
  let k f b = if f b != b then raise Noparse else b

  let some f str _ =  let r = (get_result str 0) in if f r.result then r, (advance str) else raise Noparse

  let rec many p s id =
      let rec loop str =
	try
	  let result, next = p str id in
	  let results,rest = (loop next) in
	    result.result::results,rest
	with Noparse -> [],str in
      let r = loop s in
	{poss=snd s;pose=snd (snd r);result=fst r},s
	  

  let new_rules()  = 
    let n = ref 0 in 
      (fun () -> n := !n+1; n);;
  


  let bind p f s id =
    let result,rest = p s id in
      {result with result=f result.result},rest

	
  type ast = One | Zeros of int
  ;;

  let parse_rule a_rule transform str id = 
    bind a_rule transform str id
      ;;
  
(*  
  let parser string =
    let str = make_stream string in
    let nr = new_rules() in
    let first = parse_rule (many (some (fun x -> x == '0'))) (fun l -> Zeros (List.length l)) str nr in
      fst first
*)
let _ = 'a'     
end
