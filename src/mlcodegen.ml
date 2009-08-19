open Helper
open Parserlang
open Mlpegt

(* Transform parser language code to ml *)
let rec string_of_parser_lang plang =
  let rec ident str i = if i > 0 then ident ("  " ^ str) (i-1) else str in
  let rec loop t = 
    function
      | Push -> [t, "Mstream.push stream;"]
      | Pop -> [t, "Mstream.pop stream;"]
      | Drop -> [t, "Mstream.drop stream;"]
      | RetSuccess n -> [t,"Mlpeg.Success ((Mstream.spos stream), " ^ n ^ ")"]
      | RetFail -> [t, "Mlpeg.Fail"]
      | Assign (n,v, b) -> [t, "let " ^ n ^ " = "]@loop t v@[t," in";t,"begin"]@loop t b@[t,"end"]
      | Const s -> [t, s]
      | MatchRule (r,n,s,f) -> [(t,"match Mlpeg.apply_rule stream " ^ "__get_" ^ r ^ " __set_" ^ r ^ " " ^ r ^ " with")]@[((t+1),"| Mlpeg.Fail -> ")]@loop t f@[(t+1, "| Mlpeg.Success(_, " ^ n ^ ") -> " ^ if not (std_var = n) then "let " ^ std_var ^ " = " ^ n ^ " in " else "")]@loop t s
      | Block lst -> [t,"begin"]@(List.map (function (t2,x) -> (t2+1), x) (List.concat (List.map (loop t) lst)))@[t,"end"]
      | Check (ch, s, f) -> [t, "if (Mstream.next stream).r_base = " ^ squote (string_of_char ch) ^ " then"]@loop t s@[t,"else"]@loop t f
      | TopLevel (n, b) -> [t, "and " ^ n ^ " stream = "]@(loop (t+1) b)
      | Struct lst -> [(t,"type memo = {r_base:char;")]@(List.fold_left (fun acc -> function id,typ -> acc@[t+1, "mutable " ^ id ^ " :" ^ typ ^ " Mlpeg.result option;"]) [] lst)@[t,"}"]
      | InitStruct lst -> [(t,"let rec memo_table_init ch = {")]@[t+1, "r_base=ch;"]@
	  (List.fold_left (fun acc -> function id,typ -> acc@[t+1, id ^ "= None;"]) [] lst)@[t,"}"] 
      | Entry n -> [t, "let parse str = (Mlpeg.parse memo_table_init str " ^ n ^")"]
      | Loop (n, l, a) -> [t, "let res = ref [] in"; 
		   t, "let flag = ref true in";
		   t, "while !flag do"]@
	  (loop t l)@[t,"done;";t,"let " ^ n ^ " = !res in"]@loop t a
      | EscapeLoop _ -> [t,"flag := false"]
      | AppendResult n -> [t,"res:=!res @ [" ^ n ^ "]"]
      | CustomCode s -> [t,s]
      | Fetch -> [t, "(Mlpeg.string_of_char (Mstream.next stream).r_base)"]
      | CheckRange (n, ch1, ch2, s, f) -> 
	  [t, "if " ^ n ^ ".[0]" ^ " >= " ^ squote (string_of_char ch1) ^ " && " ^ n ^ ".[0]" ^ " <= " ^ squote (string_of_char ch2) ^ " then"]
	    @ loop (t+1) s @ [t+1," else "] @ loop (t+1) f
      | ResetVars ls -> loop t ls
      | CheckCache (str, body) -> 
	    [t, "let __memo = (Mstream.top stream) in"]
	  @ [t, "match __memo." ^ str ^ " with"]
	  @ [t, "| Some a -> begin match a with | Mlpeg.Success(p,r) -> Mstream.advance stream p; a | a -> a end"]
	  @ [t, "| None ->"]
	  @ loop t body
      | Cache (str, body) -> 
	    [t,"__memo. " ^ str ^ "<- Some ( "]
	  @ loop t body @ [t,");"] @ loop t body
      | SelectStruct lst -> List.fold_left (fun acc x -> acc@[t,"let __set_" ^ x ^ " memo v = memo." ^ x ^ " <- v ";t,"let __get_" ^ x ^ " memo = memo." ^ x]) [] lst
  in
  let lst = loop 0 plang in
    String.concat "\n"  (List.fold_left (fun acc -> function t,str -> acc @ [ident str t]) [] lst)


let one_rule r = 
  toplevel_rule r.rule_id (rule_body (String.lowercase r.rule_id) r.rule_body)

let gen_code str = 
    let lexbuf = Lexing.from_string str in
    let t = Peglexer.token in
    let (code,parsed) = Pegparser.parse t lexbuf in
    let rules =  (List.map one_rule parsed) in
      String.concat "\n\n" (code::(List.map string_of_parser_lang (declarations parsed @ rules @ [Entry "a_main"])))

