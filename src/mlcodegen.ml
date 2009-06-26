open Helper


(* Our simplified AST of a target language *)
type toplevel = 
  | Fun of atype * string * (atype * string) list * form
  | StructDecl of atype * (atype * string) list
and form = 
  | IfElse of expression * form * form
  | Block of form list
  | Simple of expression
and 
  expression = 
  | TypeCons of string * expression list
  | Struct of atype * string * (string * expression) list
  | Infix of string * expression * expression
  | App of string * expression list
  | Atom of string
  | String of string
  | Char of char
  | Match of expression * (expression * form) list
and atype = 
  | SimpleType of string
  | CompoundType of atype list





(* zamienic to na liste tokenow i dorobic pretty printing *)    
let rec emit_toplevel = function
  | Fun (_, name, params, body) -> 
      let (_, param_names) = List.split params in
        "let " ^ name ^ " " ^ String.concat " " param_names ^ " = " ^ emit_form body
  | StructDecl (name, members) -> "type "^ emit_type name ^ " = {" ^ String.concat ";\n" (List.fold_left (fun a -> function (typ,name) -> a@[name^":"^emit_type typ]) [] members) ^ "}"

and emit_form = function
  | IfElse (predicate, body, els) -> 
      "if " ^ emit_expression predicate ^ " then\n" ^ emit_form body ^ " else " ^ emit_form els
  | Block statements -> "begin\n" ^ (String.concat ";\n" (List.fold_left (fun a b -> a @ [(emit_form b)]) [] statements)) ^ "\n" ^ "end\n"
  | Simple expression -> emit_expression expression

and emit_expression = function
  | Infix (operator, lhs, rhs) -> emit_expression lhs ^ " " ^ operator ^ " " ^ emit_expression rhs
  | App (func, sub_expressions) -> "(" ^ func ^ List.fold_left (fun a b -> a ^ " " ^ emit_expression b) "" sub_expressions ^ ")"
  | Atom str -> str
  | String str -> "\"" ^ str ^ "\""
  | Char char -> "\'" ^ Helper.implode [char] ^ "\'"
  | Struct (_, _, members) -> "{" ^ String.concat ";\n" (List.fold_left (fun a b -> a@[(fst b)^"="^(emit_expression (snd b))]) [] members) ^ "}"
  | TypeCons(name, sub_expressions) -> 
      let args = if sub_expressions = [] then "" else "(" ^ (String.concat "," (List.fold_left (fun a b -> a @ [emit_expression b]) [] sub_expressions)) ^ ")" in
        "(" ^ name ^ args  ^ ")"
  | Match(expression, cases) -> "match " ^ emit_expression expression ^ " with "^ String.concat "\n|" (List.fold_left (fun a -> function cas, cod -> a @ [emit_expression cas ^ "->" ^ emit_form cod]) [] cases)
and emit_type = function
  | SimpleType name -> name
  | CompoundType lst -> String.concat " " (List.fold_left (fun a b -> a@[emit_type b]) [] lst)



let parser_fail = Block([Simple(App("Mstream.pop",[Atom("stream")]));Simple(Atom("Mlpdef.Fail"))])
let parser_success = Block([Simple(App("Mstream.drop",[Atom("stream")]));Simple(TypeCons("Mlpdef.Success",[App("Mstream.spos",[Atom("stream")]);String("ala")]))])
let stream_next id = Infix(".", App("Mstream.next",[Atom("stream")]),Atom(id))
let stream_push = App("Mstream.push", [Atom "stream"])
let stream_pop = App("Mstream.pop", [Atom "stream"])
let chain_op op  = function h::t -> List.fold_left (fun a b -> Infix(op,a, b)) h t
let chain_and = chain_op "&&"
let chain_or = chain_op "||"

let parser_check_symbol symbol rule_id  = Infix("=",Char(symbol),stream_next rule_id)

let parser_match_str str rule_id =
  let lst = (Helper.explode str) in
    chain_and (stream_push::(List.map (fun x -> parser_check_symbol x "__base") lst))

let parser_choice left right = chain_or [left;chain_and [stream_pop;right]]
let parser_check lst = IfElse(lst, parser_success, parser_fail)

let parser_function rule_id body = 
  Fun(SimpleType "", rule_id, [(SimpleType "","stream")], 
      Block([Simple(App("Mstream.push", [Atom "stream"])); body])) 

let parser_check_rule created_rule_id rule_id success fail = 
      Fun(SimpleType "", created_rule_id, [
	    (SimpleType "","stream")], Block([Simple(App("Mstream.push", [Atom "stream"]));
					      Simple(Match(App(rule_id,[Atom("stream")]),
							   [
									TypeCons("Mlpdef.Success",[Atom("pos");Atom("result")]),success;
							     TypeCons("Mlpdef.Fail",[]),fail
							   ]))]))
	
let init_rules_function n = 
  let inits = let rec loop i =  if i < n then (Printf.sprintf "__r%.3d" i, App("None",[]))::loop (i+1) else [] in loop 0 in
    Fun(SimpleType "", "memo_table_init", [(SimpleType "","ch")], Simple(Struct(SimpleType "", "", ("__base", Atom("ch"))::inits)))
      
let declare_rule_struct n = 
  let inits = let rec loop i =  if i < n then (CompoundType([SimpleType("char");SimpleType("option")]),Printf.sprintf "__r%.3d" i)::loop (i+1) else [] in loop 0 in
    StructDecl(SimpleType "rules", (SimpleType("char"),"__base")::inits)

let parse_function rule_id = 
  Fun(SimpleType "", "parse", [(SimpleType "", "str")], Simple(App("Mlpdef.parse", [Atom("memo_table_init"); Atom("str");Atom(rule_id)])))
        
let rec ast_from_parsed_ast ast = 
  let rec loop = function
    | [] -> []
    | r::rs -> 
	let l = r.rule_body in
	let rec expression = function
	  | Cat(lst) -> let str = String.concat "" (List.fold_left (fun a b -> a @ [(match b with Literal x -> x)]) [] lst) 
	    in parser_match_str str r.rule_id
	  | Choice(left, right) -> parser_choice (expression left) (expression right)
	in (parser_function r.rule_id (parser_check (expression l)))::(loop rs)
  in
  let nrules = (List.length ast) in
    declare_rule_struct nrules  :: init_rules_function nrules :: loop ast @ 
      [parse_function (List.hd (List.rev ast)).rule_id]
      
    

let gen_code str = 
    let lexbuf = Lexing.from_string str in
    let t = Peglexer.token in
    let parsed = Pegparser.prule t lexbuf in
      String.concat "\n\n\n\n" (List.map emit_toplevel (ast_from_parsed_ast [parsed])) 
      
