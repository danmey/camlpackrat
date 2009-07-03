open Helper
open Mlpegt

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
  | Cache of string * parser_lang
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

let std_var = "loc"
 
(* The rule succeeded drop position stack, and return success *)
let rule_success name var =
  Block [Drop; Cache(name, RetSuccess var)]

(* Rule failed pop out position stack and return failure *)
let rule_fail name = Block [Pop; Cache(name, RetFail)]

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
let toplevel_rule rule_id body = 
  let id = String.lowercase rule_id in
    TopLevel (id,CheckCache(id, Block[Push; body]))

let rec drop_if f = function
    [] -> []
  | h::t -> if not (f h) then h::t else drop_if f t

(* Transform ast to parser language *)
let rule_body name ast = 
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
  let rec reduce_pass = function
    | Assign(var1, CustomCode(var2), body) -> 
	if var1 = var2 then 
	  reduce_pass body 
	else
	  Assign(var1, CustomCode(var2), reduce_pass body)
    | Check (ch, b1, b2) -> Check (ch, reduce_pass b1, reduce_pass b2) 
    | CheckRange(str,ch1,ch2,b1,b2) -> CheckRange(str,ch1,ch2,reduce_pass b1,reduce_pass b2)
    | Block lst ->  Block (List.map reduce_pass lst)
    | TopLevel (str, b) -> TopLevel (str, reduce_pass b) 
    | MatchRule (str1, str2, b1, b2) -> MatchRule (str1, str2, reduce_pass b1, reduce_pass b2) 
    | Loop (str, b1, b2) -> Loop (str, reduce_pass b1, reduce_pass b2)
    | Assign (str,b1, b2) -> Assign (str,reduce_pass b1, reduce_pass b2)
    | ResetVars b -> ResetVars b 
    | a -> a 
  in
    (rule_body' std_var (rule_success name std_var) (rule_fail name) ast)
  
let declarations rules = 
  let struc = Struct (List.map (fun x -> String.lowercase x.rule_id, x.rule_type) rules) in
  let init_struc = InitStruct (List.map (fun x -> String.lowercase x.rule_id,x.rule_type) rules) in
    [struc; init_struc]
