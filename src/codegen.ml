open Helper
let explode str =
  let rec loop i acc =
    if i < 0 then acc else
      loop (i-1) ((String.get str i)::acc) in
    loop ((String.length str)-1) [] 

let match_symbol symbol = 
  fun succ fail ->
    "begin\n" ^ "if (Mstream.next stream).__base = " ^ "'" ^ symbol ^ "'" ^ " then\n"
    ^ succ ^ "else\n" ^ fail ^ "end\n"
      
let string_of_char ch = Printf.sprintf "%c" ch
  

let rule_success = fun transform ->
  "begin\n(Mstream.drop stream);\n" ^
    "let res = " ^ transform ^ " in\n" ^
    "(Mlpdef.Success((Mstream.spos stream),res))\nend\n"

let rule_fail = 
  "begin\n(Mstream.pop stream);\n"^
    "Mlpdef.Fail\nend\n"

let match_string str =
  let lst = explode str in
    fun index succ fail ->
      (List.fold_right 
	 (fun el acc -> 
	    let el_str = string_of_char el in
	      match_symbol el_str acc rule_fail) lst 
	 succ)

let simple_match_string str = match_string str "res" (rule_success "res") rule_fail


let match_concat_strings lst =
  let all = String.concat "" lst in
  List.fold_right 
    (fun el acc ->
       match_string el "res" acc rule_fail) lst (rule_success ("\"" ^ all ^ "\""))
	
let toplevel_rule rule_id =
  fun body ->
    "let " ^ rule_id ^ " stream =\n" ^ "Mstream.push stream;\n" ^ body

let rec emit_rule_body = function
  | Cat(lst) -> match_concat_strings (List.map emit_rule_body lst)
  | Literal a ->  a

let emit_rule r = 
  toplevel_rule r.rule_id (emit_rule_body r.rule_body)


let emit_prelude = "type memo = { __base:char; }\n" ^ "let memo_table_init ch = {__base=ch; }\n"
let emit_parse = "let parse str = (Mlpdef.parse memo_table_init str rule_no1)"


let gen_code str = 
    let lexbuf = Lexing.from_string str in
    let t = Peglexer.token in
    let parsed = Pegparser.prule t lexbuf in
      emit_prelude ^ emit_rule parsed ^ emit_parse
