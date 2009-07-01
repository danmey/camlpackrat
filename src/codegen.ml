open Helper

type parser_lang = 
    Push 
  | Pop 
  | Drop 
  | RetFail 
  | Check of char * parser_lang * parser_lang 
  | CheckCache of string * parser_lang
  | CustomCode of string 
  | Block of parser_lang list 
  | TopLevel of string * parser_lang 
  | Struct of (string*string) list 
  | InitStruct of (string*string) list 
  | Entry of string 
  | MatchRule of string * int * parser_lang * parser_lang 
  | Loop of int * parser_lang * parser_lang
  | EscapeLoop of int | Const of string
  | Assign of int * parser_lang * parser_lang 
  | RetSuccess of int 
  | AppendResult of int
 
let rule_success =
  Block [Drop; RetSuccess 0]

let rule_fail = Block [Pop; RetFail]

let match_token token succ fail =
  Check (token, succ, fail)

let quote str = "\"" ^ str ^ "\""

let string_of_char ch = Printf.sprintf "%c" ch

let match_string str succ fail =
  let lst = explode str in 
    Assign (0, Const(quote str), (List.fold_right 
				    (fun el acc -> 
				       match_token el acc fail) lst 
				    succ))
      
let toplevel_rule rule_id body = TopLevel (rule_id,Block[Push; body])

let rec drop_if f = function
    [] -> []
  | h::t -> if not (f h) then h::t else drop_if f t
	
let take_if f lst = 
  let rec loop acc = function
    [] -> acc
  | h::t -> if f h then loop (acc@[h]) t else acc in
    loop [] lst

let rec replace_placholders s = 
  let str = explode s in
  let before = take_if (fun x -> x != '$') str in
  let after = drop_if (fun x -> x != '$') str in
    if after != [] then
      let a_number = take_if (fun x -> x = '$' || x >= '0' && x < '9') after in
      let after_number = drop_if (fun x -> x = '$' || x >= '0' && x < '9') after in
	match a_number with
	    [] -> implode str
	  | h::t -> replace_placholders (implode (before @ (explode "r_res_") @ t @ after_number))
    else 
      s

let make_int_gen () = 
  let i = ref 0 in
    fun () -> i := !i+1; !i

let int_gen () = make_int_gen()

let rule_body ast = 
  let rec rule_body' succ fail = function
    | Literal s -> match_string s succ fail
    | Group (f,s) -> rule_body' (rule_body' succ fail s) fail f 
    | Rule str -> MatchRule (str, 0, succ, fail)
    | Many s -> Block[Push; Loop (0, (rule_body' (Block[Drop;Push;AppendResult 0]) (Block[Pop;EscapeLoop 0]) s), succ)]
    | Transform (code,s) -> rule_body' (Assign (0, CustomCode (replace_placholders code), succ)) fail s  
    | Choice (l, r) -> Block [Push; rule_body' succ (Block[Pop;rule_body' succ fail r]) l] in
  let resolve_variables ast = 
    let rec resolve_variables' n  = function 
      | Assign(_, v, b) -> Assign(n, resolve_variables' (n+1) v, resolve_variables' (n+1) b)
      | MatchRule(str,_,s,f) -> MatchRule(str, n, resolve_variables' (n+1) s, resolve_variables' (n+1) f)       
      | Loop (_,s,f) -> Loop(n, resolve_variables' (n+1) s, resolve_variables' (n+1) f)       
      | EscapeLoop _ -> EscapeLoop (n-1)
      | RetSuccess _ -> RetSuccess (n-1)
      | AppendResult _ -> AppendResult (n-1)
      | Check (c, a, b) -> Check (c, resolve_variables' n a,resolve_variables' n b)
      | Block l -> Block (List.map (resolve_variables' n) l)
      | els -> els
    in resolve_variables' 0 ast in
    resolve_variables (rule_body' rule_success rule_fail ast)
  

let declarations rules = 
  let struc = Struct (List.map (fun x -> x.rule_id, x.rule_type) rules) in
  let init_struc = InitStruct (List.map (fun x -> x.rule_id,x.rule_type) rules) in
    [struc; init_struc]

let rec string_of_parser_lang plang =
  let rec ident str i = if i > 0 then ident ("  " ^ str) (i-1) else str in
  let var_bind n = "r_res_" ^ string_of_int n in
  let rec loop t = 
    function
      | Push -> [t, "let _ = Mstream.push stream in"]
      | Pop -> [t, "let _ = Mstream.pop stream in"]
      | Drop -> [t, "let _ = Mstream.drop stream in"]
      | RetSuccess n -> [t,"Mlpdef.Success ((Mstream.spos stream), " ^ var_bind n ^ ")"]
      | RetFail -> [t, "Mlpdef.Fail"]
      | Assign (n,v, b) -> [t, "let " ^ var_bind n ^ " = "]@loop t v@[t," in";t,"begin"]@loop t b@[t,"end"]
      | Const s -> [t, s]
      | MatchRule (r,n,s,f) -> [(t,"match " ^ r ^ " stream with")]@[((t+1),"| Mlpdef.Fail -> ")]@loop t f@[(t+1, "| Mlpdef.Success(_, " ^ var_bind n ^ ") -> ")]@loop t s
      | Block lst -> [t,"begin"]@(List.map (function (t2,x) -> (t2+1), x) (List.concat (List.map (loop t) lst)))@[t,"end"]
      | Check (ch, s, f) -> [t, "if (Mstream.next stream).r_base = '" ^ string_of_char ch ^ "' then"]@loop t s@[t,"else"]@loop t f
      | TopLevel (n, b) -> [t, "let " ^ n ^ " stream = "]@(loop (t+1) b)
      | Struct lst -> [(t,"type memo = {r_base:char;")]@(List.fold_left (fun acc -> function id,typ -> acc@[t+1, id ^ " :" ^ typ ^ " option;"]) [] lst)@[t,"}"]
      | InitStruct lst -> [(t,"let memo_table_init ch = {")]@[t+1, "r_base=ch;"]@
	  (List.fold_left (fun acc -> function id,typ -> acc@[t+1, id ^ "= None;"]) [] lst)@[t,"}"] 
      | Entry n -> [t, "let parse str = (Mlpdef.parse memo_table_init str " ^ n ^")"]
      | Loop (i, l, a) -> [t, "let res = ref [] in"; 
		   t, "let flag = ref true in";
		   t, "while !flag do"]@
	  (loop t l)@[t,"done;";t,"let " ^ var_bind i ^ " = !res in"]@loop t a
      | EscapeLoop _ -> [t,"flag := false"]
      | AppendResult i -> [t,"res:=!res @ [" ^ var_bind i ^ "]"]
      | CustomCode s -> [t,s]
  in
  let lst = loop 0 plang in
    String.concat "\n"  (List.fold_left (fun acc -> function t,str -> acc @ [ident str t]) [] lst)


let one_rule r = 
  toplevel_rule r.rule_id (rule_body r.rule_body)

let gen_code str = 
    let lexbuf = Lexing.from_string str in
    let t = Peglexer.token in
    let parsed = Pegparser.stmnt t lexbuf in
    let rules =  (List.map one_rule parsed) in
      String.concat "\n\n" (List.map string_of_parser_lang (declarations parsed @ rules @ [Entry "a_main"]))

