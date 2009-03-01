open Peg
  let peg_grammar =
    let end_rule = new_grammar() in
    let rec grammar str = (def_rule (spacing <++> (many1 definition) <++> (followed eof)) (end_rule())) str
    and	definition str = (def_rule (identifier <++> left_arrow <++> expression ) (end_rule())) str
    and expression str  = (def_rule (sequence <++> (many (slash <++> sequence))) (end_rule())) str 
    and sequence str  = (def_rule (many prefix) (end_rule())) str 
    and	prefix str = (def_rule ((opt (pand </> pnot))<++>  suffix) (end_rule())) str
    and suffix str = (def_rule (primary <++> (opt (question </> star </> plus))) (end_rule())) str
    and primary str = 
      (def_rule ((identifier <++> (not_followed left_arrow)) 
		 </> (open_ <++> expression <++> close_) 
		 </> (literal </> class_ </> dot)) (end_rule())) str
    and	pnot str = (def_rule (t "!") (end_rule()))  str
    and	spacing str = (def_rule (many (space </> comment)) (end_rule())) str
    and	identifier str = (def_rule (ident_start <++> (many ident_cont) <++> spacing) (end_rule())) str
    and left_arrow str = (def_rule ((t "<-") <++> spacing) (end_rule())) str
    and slash str = (def_rule ((t "/") <++> spacing) (end_rule())) str 
    and pand str =  (def_rule ((t "&") <++> spacing) (end_rule())) str
    and space str = (def_rule ((t " ") </> (t "\t") </> end_of_line) (end_rule())) str
    and comment str = (def_rule (t "#" <++> (many ((not_followed end_of_line) <++> any)) <++> end_of_line) (end_rule())) str
    and ident_start str = (def_rule (pmatch_pred (fun x -> (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || x == '_')) (end_rule())) str
    and ident_cont str = (def_rule (ident_start </> (pmatch_pred (fun x -> x >= '0' && x <= '9'))) (end_rule()))  str
    and question str = ((def_rule ((t "?") <++> spacing)) (end_rule())) str
    and star str = ((def_rule ((t "*") <++> spacing)) (end_rule())) str
    and plus str = ((def_rule ((t "+") <++> spacing)) (end_rule())) str
    and end_of_line str = (def_rule ((t "\r\n") </> (t "\n") </> (t "\r")) (end_rule())) str
    and open_ str = ((def_rule ((t "(")  <++> spacing)) (end_rule()))  str
    and close_ str =((def_rule ((t ")") <++> spacing)) (end_rule()))  str
    and literal str  = 
      (def_rule 
	 ((t "\'" <++> (many ((not_followed (t "\'")) <++> char_)) <++> (t "\'") <++> spacing) 
	  </> ((t "\"" <++> (many ((not_followed (t "\"")) <++> char_)) <++> (t "\'") <++> spacing))) (end_rule())) str
    and class_ str = (def_rule ((t "[") <++> (many ((not_followed (t "]")) <++> range)) <++> (t "]") <++> spacing) (end_rule())) str
    and dot str = ((def_rule ((t ".") <++> spacing)) (end_rule())) str
    and char_ str = (def_rule (
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
      </> (not_followed (t "\\")) <++> any) (end_rule()))  str
    and range str = (def_rule ((char_ <++> (t "-") <++> char_) </> char_) (end_rule())) str
				    
    in
      grammar

