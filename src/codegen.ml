open Helper
open Mlpdef

(* 
   Our parser embededed language 
   All the label values are ints and are processed
   in second pass
*)
type parser_lang = 
    Push 
  | Pop 
  | Drop 
  | Fetch
  | RetFail 
  | Check of char * parser_lang * parser_lang 
  | CheckRange of string * char * char * parser_lang * parser_lang
  | CheckCache of string * parser_lang
  | CustomCode of string 
  | Block of parser_lang list 
  | TopLevel of string * parser_lang 
  | Struct of (string*string) list 
  | InitStruct of (string*string) list 
  | Entry of string 
  | MatchRule of string * string * parser_lang * parser_lang 
  | Loop of string * parser_lang * parser_lang
  | EscapeLoop of string | Const of string
  | Assign of string * parser_lang * parser_lang 
  | RetSuccess of string
  | AppendResult of string
  | ResetVars of parser_lang

let std_var = "___nm"
 
(* The rule succeeded drop position stack, and return success *)
let rule_success var =
  Block [Drop; RetSuccess var]

(* Rule failed pop out position stack and return failure *)
let rule_fail = Block [Pop; RetFail]

let mk_generator () = 
  let x = ref 0 in
    fun() -> x := !x+1; !x

let int_gen = mk_generator()


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

(* 
   Construct code for matching string
   succ and fail are valid actions 
*)
let match_string str succ fail =
  let lst = explode str in 
    Assign (std_var, Const(quote str), (List.fold_right 
				    (fun el acc -> 
				       Check (el, acc, fail)) lst 
				    succ))
  
(* Construct top level rule *)    
let toplevel_rule rule_id body = TopLevel (String.lowercase rule_id,Block[Push; body])

let rec drop_if f = function
    [] -> []
  | h::t -> if not (f h) then h::t else drop_if f t
	
let take_if f lst = 
  let rec loop acc = function
    [] -> acc
  | h::t -> if f h then loop (acc@[h]) t else acc in
    loop [] lst

(* Replace place holders with free variables bound to chunks processed *)
(*
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
*)

(* Transform ast to parser language *)
let rule_body ast = 
  let rec char_class var succ fail = function
    | Range (ch1, ch2) -> Assign (var, Fetch, (CheckRange (var, ch1,ch2, succ, fail)))
    | Classes cls ->    List.fold_right (fun el acc -> 
					   char_class var succ acc el) cls fail
    | OneCharacter ch -> Assign(var, Fetch, Block [Pop; Push; Check (ch, succ, fail)])
    | Negate cls -> List.fold_right (fun el acc -> 
				       char_class var fail acc el) cls succ
  in
  let rec rule_body' var succ fail = function
    | Literal s -> match_string s succ fail
    | Group (f,s) -> rule_body' var (rule_body' std_var succ fail s) fail f 
    | Rule str -> MatchRule (String.lowercase str, var , succ, fail)
    | Many s -> Block[Push; Loop (var, (rule_body' std_var (Block[Drop;Push;AppendResult std_var]) (Block[Pop;EscapeLoop var]) s), succ)]
    | Class s -> char_class var succ fail s
    | Transform (code,s) -> rule_body' var (Assign (std_var, CustomCode (code), succ)) fail s  
    | Choice (l, r) -> ResetVars (Block [Push; rule_body' var succ (ResetVars (Block[Pop;rule_body' std_var succ fail r])) l])    | Not s -> Block[Push;rule_body' var (Block[Pop; fail]) (Block[Pop; succ]) s]
    | And s -> Block[Push;rule_body' var (Block[Pop; succ]) (Block[Pop; fail]) s]
    | Any s -> Assign(var, Fetch, rule_body' var succ fail s)
    | Nothing -> Block[]
    | AssignVar (n, bl) -> rule_body' n succ fail bl
  in
(*
  let resolve_variables ast = 
    let rec resolve_variables' n  = function 
      | Assign(_, Fetch, CheckRange(_, ch1,ch2, s, f ) ) -> Assign(n, Fetch, CheckRange(var, ch1,ch2, resolve_variables' (n+1) s, resolve_variables' (n+1) f ) ) 
      | Assign(_, v, b) -> Assign(n, resolve_variables' (n+1) v, resolve_variables' (n+1) b)
      | MatchRule(str,_,s,f) -> MatchRule(str, n, resolve_variables' (n+1) s, resolve_variables' (n+1) f)       
      | Loop (_,s,f) -> Loop(n, resolve_variables' (n+1) s, resolve_variables' (n+1) f)       
      | EscapeLoop _ -> EscapeLoop (n-1)
      | RetSuccess _ -> RetSuccess (n-1)
      | AppendResult _ -> AppendResult (n-1)
      | Check (c, a, b) -> Check (c, resolve_variables' n a,resolve_variables' n b)
      | Block l -> Block (List.map (resolve_variables' n) l)
      | ResetVars l -> resolve_variables' 0 l
      | els -> els

    in resolve_variables' 0 ast in
    resolve_variables (rule_body' var rule_success rule_fail ast) 
*)
    rule_body' std_var (rule_success std_var) rule_fail ast
  

let declarations rules = 
  let struc = Struct (List.map (fun x -> String.lowercase x.rule_id, x.rule_type) rules) in
  let init_struc = InitStruct (List.map (fun x -> String.lowercase x.rule_id,x.rule_type) rules) in
    [struc; init_struc]

(* Transform parser language code to ml *)
let rec string_of_parser_lang plang =
  let rec ident str i = if i > 0 then ident ("  " ^ str) (i-1) else str in
  let rec loop t = 
    function
      | Push -> [t, "let _ = Mstream.push stream in"]
      | Pop -> [t, "let _ = Mstream.pop stream in"]
      | Drop -> [t, "let _ = Mstream.drop stream in"]
      | RetSuccess n -> [t,"Mlpdef.Success ((Mstream.spos stream), " ^ n ^ ")"]
      | RetFail -> [t, "Mlpdef.Fail"]
      | Assign (n,v, b) -> [t, "let " ^ n ^ " = "]@loop t v@[t," in";t,"begin"]@loop t b@[t,"end"]
      | Const s -> [t, s]
      | MatchRule (r,n,s,f) -> [(t,"match " ^ r ^ " stream with")]@[((t+1),"| Mlpdef.Fail -> ")]@loop t f@[(t+1, "| Mlpdef.Success(_, " ^ n ^ ") -> let " ^ std_var ^ " = " ^ n ^ " in ")]@loop t s
      | Block lst -> [t,"begin"]@(List.map (function (t2,x) -> (t2+1), x) (List.concat (List.map (loop t) lst)))@[t,"end"]
      | Check (ch, s, f) -> [t, "if (Mstream.next stream).r_base = " ^ squote (string_of_char ch) ^ " then"]@loop t s@[t,"else"]@loop t f
      | TopLevel (n, b) -> [t, "and " ^ n ^ " stream = "]@(loop (t+1) b)
      | Struct lst -> [(t,"type memo = {r_base:char;")]@(List.fold_left (fun acc -> function id,typ -> acc@[t+1, id ^ " :" ^ typ ^ " option;"]) [] lst)@[t,"}"]
      | InitStruct lst -> [(t,"let rec memo_table_init ch = {")]@[t+1, "r_base=ch;"]@
	  (List.fold_left (fun acc -> function id,typ -> acc@[t+1, id ^ "= None;"]) [] lst)@[t,"}"] 
      | Entry n -> [t, "let parse str = (Mlpdef.parse memo_table_init str " ^ n ^")"]
      | Loop (n, l, a) -> [t, "let res = ref [] in"; 
		   t, "let flag = ref true in";
		   t, "while !flag do"]@
	  (loop t l)@[t,"done;";t,"let " ^ n ^ " = !res in"]@loop t a
      | EscapeLoop _ -> [t,"flag := false"]
      | AppendResult n -> [t,"res:=!res @ [" ^ n ^ "]"]
      | CustomCode s -> [t,s]
      | Fetch -> [t, "(Mlpdef.string_of_char (Mstream.next stream).r_base)"]
      | CheckRange (n, ch1, ch2, s, f) -> 
	  [t, "if " ^ n ^ ".[0]" ^ " >= " ^ squote (string_of_char ch1) ^ " && " ^ n ^ ".[0]" ^ " <= " ^ squote (string_of_char ch2) ^ " then"]
	    @ loop (t+1) s @ [t+1," else "] @ loop (t+1) f
      | ResetVars ls -> loop t ls
  in
  let lst = loop 0 plang in
    String.concat "\n"  (List.fold_left (fun acc -> function t,str -> acc @ [ident str t]) [] lst)


let one_rule r = 
  toplevel_rule r.rule_id (rule_body r.rule_body)

let gen_code str = 
    let lexbuf = Lexing.from_string str in
    let t = Peglexer.token in
    let (code,parsed) = Pegparser.parse t lexbuf in
    let rules =  (List.map one_rule parsed) in
      String.concat "\n\n" (code::(List.map string_of_parser_lang (declarations parsed @ rules @ [Entry "a_main"])))

