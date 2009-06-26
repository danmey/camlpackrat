open Helper

type parser_lang = Push | Pop | Drop | RetSuccess of int | RetFail | Assign of int * string | MatchRule of string * int * parser_lang * parser_lang | CustomCode of string | Block of parser_lang list | Check of char * parser_lang * parser_lang | TopLevel of string * parser_lang | Struct of (string*string) list | InitStruct of (string*string) list | Entry of string

let rule_success transform label =
  Block [Drop; Assign (0, transform); RetSuccess 0]

let rule_fail = Block [Pop; RetFail]

let match_token token succ fail =
  Block [Check (token, succ, fail)]

let quote str = "\"" ^ str ^ "\""
let string_of_char ch = Printf.sprintf "%c" ch

let match_string str succ fail index =
  let lst = explode str in 
    Block [Assign (index, quote str);
   	   (List.fold_right 
	      (fun el acc -> 
		   match_token el acc rule_fail) lst 
	      succ)]

let match_rule a_rule succ fail label = MatchRule (a_rule, label, succ, fail)

let implode lst = 
  let str = String.create (List.length lst) in
  let rec loop i = function [] -> str | x::xs -> String.set str i x; loop (i+1) xs in
    loop 0 lst

let explode str =
  let rec loop i acc =
    if i < 0 then acc else
      loop (i-1) ((String.get str i)::acc) in
    loop ((String.length str)-1) [] 

let toplevel_rule rule_id body = (TopLevel (rule_id,Block[Push; body]))


let no_of_terms lst =
  let rec loop = function
    | Cat lst -> List.fold_left (fun acc el -> acc + (loop el)) 0 lst
    | _ -> 1
  in
    List.fold_left (fun acc el -> acc + loop el) 0 lst
      
let rec drop_if f = function
    [] -> []
  | h::t -> if not (f h) then h::t else drop_if f t
	
let take_if f lst = 
  let rec loop acc = function
    [] -> acc
  | h::t -> if f h then loop (acc@[h]) t else acc in
    loop [] lst

let replace_placholders s = 
  let str = explode s in
  let before = take_if (fun x -> x != '$') str in
  let after = drop_if (fun x -> x != '$') str in
  let a_number = take_if (fun x -> x = '$' || x >= '0' && x < '9') after in
  let after_number = drop_if (fun x -> x = '$' || x >= '0' && x < '9') after in
    match a_number with
	[] -> implode str
      | h::t -> implode (before @ (explode "r_res_") @ t @ after_number)

let rec rule_body lst ml_block =
  fst (List.fold_right 
    (fun el -> function acc,i ->
       match el with
	 | Literal str -> (match_string str acc rule_fail i) ,(i-1)
	 | Rule str -> match_rule str acc rule_fail i,(i-1)
	 | Cat l -> (rule_body l ml_block),i
    ) lst ((rule_success (if ml_block = "" then "r_res_1" else (replace_placholders ml_block)) 1),no_of_terms lst))

let one_rule r = 
  toplevel_rule r.rule_id (rule_body [r.rule_body] r.ml_block)

let declarations rules = 
  let struc = Struct (List.map (fun x -> x.rule_id, x.rule_type) rules) in
  let init_struc = InitStruct (List.map (fun x -> x.rule_id,x.rule_type) rules) in
    [struc; init_struc]

let rec string_of_parser_lang plang =
  let rec ident str i = if i > 0 then ident ("   " ^ str) (i-1) else str in
  let var_bind n = "r_res_" ^ string_of_int n in
  let rec loop t = 
    function
      | Push -> [t, "let _ = Mstream.push stream in"]
      | Pop -> [t, "let _ = Mstream.pop stream in"]
      | Drop -> [t, "let _ = Mstream.drop stream in"]
      | RetSuccess n -> [t,"Mlpdef.Success ((Mstream.spos stream), " ^ var_bind n ^ ")"]
      | RetFail -> [t, "Mlpdef.Fail"]
      | Assign (n,s) -> [t, "let " ^ var_bind n ^ " = " ^ s ^ " in"]
      | MatchRule (r,n,s,f) -> [(t,"match " ^ r ^ " stream with")]@[((t+1),"| Mlpdef.Fail -> ")]@loop t f@[(t+1, "| Mlpdef.Success(_, " ^ var_bind n ^ ") -> ")]@loop t s
      | Block lst -> [t,"begin"]@(List.map (function (t2,x) -> (t2+1), x) (List.concat (List.map (loop t) lst)))@[t,"end"]
      | Check (ch, s, f) -> [t, "if (Mstream.next stream).r_base = '" ^ string_of_char ch ^ "' then"]@loop t s@[t,"else"]@loop t f
      | TopLevel (n, b) -> [t, "let " ^ n ^ " stream = "]@(loop t b)
      | Struct lst -> [(t,"type memo = {r_base:char;")]@(List.fold_left (fun acc -> function id,typ -> acc@[t+1, id ^ " :" ^ typ ^ " option;"]) [] lst)@[t,"}"]
      | InitStruct lst -> [(t,"let memo_table_init ch = {")]@[t+1, "r_base=ch;"]@
	  (List.fold_left (fun acc -> function id,typ -> acc@[t+1, id ^ "= None;"]) [] lst)@[t,"}"] 
      | Entry n -> [t, "let parse str = (Mlpdef.parse memo_table_init str " ^ n ^")"]
  in
  let lst = loop 0 plang in
    String.concat "\n"  (List.fold_left (fun acc -> function t,str -> acc @ [ident str t]) [] lst)

let gen_code str = 
    let lexbuf = Lexing.from_string str in
    let t = Peglexer.token in
    let parsed = Pegparser.prule t lexbuf in
    let rules = List.map one_rule parsed in
      String.concat "\n\n" (List.map string_of_parser_lang (declarations parsed @ rules @ [Entry "a_main"]))

