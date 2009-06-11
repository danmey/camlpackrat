open Peg


let gen_rules_memo_declaration rule_types_assoc = 
  begin
    Printf.printf "type memo = {\n";
    List.iter (function name,type_ -> Printf.printf "\t%s:%s;\n" name type_) rule_types_assoc;
    Printf.printf "}\n"
  end

type operation = Choice of term * term | Cat of term list
and term = Literal of string | Rule of string | Operation of operation
and prule = {rule_id:string; rule_type:string; rule_body:term }    


(*val peg_grammar : char list Peg.stream -> char list * char list Peg.stream *)
(* rule : char list := "ala"; *)

    
let implode lst = 
  let str = String.create (List.length lst) in
  let rec loop i = function [] -> str | x::xs -> String.set str i x; loop (i+1) xs in
    loop 0 lst

let parse str =
  let end_of_line = (t "\r\n" </> t "\n" </> t "\r") >>= (fun _ -> []) in
  let comment = (t "#" <++> (many ((not_followed end_of_line) <++> any)) <++> end_of_line) >>= (fun _ -> []) in 
  let space = ((t " ") </> (t "\t") </> end_of_line) >>= (fun _ -> [])  in
  let spacing = (many (space </> comment)) in
  let just_token str = (t str) >>= (fun _ -> []) in
  let just_token_sp str = spacing <++> just_token str in
  let char_ = 
    ((t "\\") <++> (pmatch_pred 
		      (fun x -> (x == 'n') || (x == 'r')
			 || (x == 't') || (x =='\'') || (x == '\"')
			 || (x == '\\') || (x == '[') || (x == ']')))) 
    </> (t "\\" <++> 
	   (pmatch_pred (fun x -> (x >= '0' && x <= '2'))) <++> 
	   (pmatch_pred (fun x -> (x >= '0' && x <= '7'))) <++>
	   (pmatch_pred (fun x -> (x >= '0' && x <= '7'))))
    </> (t "\\" <++> 
	   (pmatch_pred (fun x -> (x >= '0' && x <= '7'))) <++>
	   (opt (pmatch_pred (fun x -> (x >= '0' && x <= '7')))))
    </> ((not_followed (t "\\")) <++> any) in
  let dquote = just_token "\"" in
  let quote = just_token "\'" in
  let literal = 
    ((quote <++> (many ((not_followed quote) <++> char_)) <++> quote <++> spacing)  
    </> ((dquote <++> (many ((not_followed (dquote)) <++> char_)) <++> dquote <++> spacing))) 
    >>= (fun x -> [(implode x)]) in
  let ident_start = (pmatch_pred (fun x -> (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || x == '_')) in
  let ident_cont =  (ident_start </> (pmatch_pred (fun x -> x >= '0' && x <= '9'))) in
  let identifier =  (ident_start <++> (many ident_cont) <++> spacing) >>= (fun x -> [implode x]) in
  let colon = just_token_sp ":" in
  let semi_colon = just_token_sp ";" in
  let subst = just_token_sp ":=" in
  let rule_body = ((spacing <++> literal) >>= (function x::[] -> Literal x)) </> ((spacing <++> identifier) >>= (function x::[] -> Rule x)) in
  let peg_rule = ((spacing <++> identifier <++> colon <++> spacing <++> (((many1 ((not_followed subst) <++> char_))) >>= (fun x -> [implode x])) 
                  <++> subst) >>= (function name::typ::[] -> [{rule_id=name;rule_type=typ;rule_body=Literal ""}])
                  <++> (rule_body >>= (function x -> [{rule_id="";rule_type="";rule_body=x}]))) >>= (function x::y::[] -> [{rule_id=x.rule_id;rule_type=x.rule_type;rule_body=y.rule_body}]) <++> semi_colon in
    (fst (((many1 peg_rule) <++> (followed eof)) (make_stream str)))
