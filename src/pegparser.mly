%{
  open Helper
  let parse_class str = 
    let parse_range = function
      | [] -> None,[]
      | a::'-'::b::r -> Some (Range (a, b)), r
      | r -> None, r
    in
    let parse_char = function
      | [] -> None,[]
      | a::r -> Some (OneCharacter a),r
    in
    let rec parse ls =
	match ls with
	| [] -> []
	| '^'::r -> Negate (parse r)::[]
	| r ->
	    begin
	      match parse_range r with
		| None,r -> begin
		    match parse_char r with
		      | None,_ -> []
		      | Some a,r -> a::(parse r)
		  end
		| Some a,r -> a::(parse r)
	    end
    in
      match parse (explode str) with
	  | [] -> Classes []
	  | x::[] -> x
	  | ls -> Classes ls
%}

%token Eof
%token <string> Ident 
%token <string> Literal
%token <string> Code
%token <string> Action
%token <string> Class
%token Slash Star Lb Rb
%token Colon Arrow Semicolon Quote Question Plus
%type <string * Helper.prule list> parse
%start parse
%%

term:
	  Ident			{ Rule($1) }
	  | Class              	{ Class(parse_class($1))}
	  | Literal 	       	{ Literal($1) }
	  | Lb expression Rb   	{ $2 }

term_list:
      term				{ $1 }
      | term term_list             	{ Group($1,$2) }

many:
          term_list			{ $1 }
	  | term_list Star           	{ Many($1) } 
	  | term_list Star many      	{ Group(Many($1),$3) } 
	  | term_list Plus           	{ Group($1,Many($1)) } 
	  | term_list Plus many      	{ Group(Group($1,Many($1)),$3) } 

expression:	
      	many				{ $1 } 
      | many Slash expression         	{ Choice($1,$3) }

trans:
          expression				{ $1 }
	  | expression Action               	{ Transform($2,$1) }
	  | expression Action Slash trans   	{ Choice(Transform($2,$1),$4) }
atype:
	Ident				{ [$1] }
      	| Ident atype                   { [String.concat " " ($1::$2)] }

parse:
	  Eof 						{ ("",[]) }
	  | Code 					{ ("",[]) }
	  | Ident Colon atype Arrow trans Semicolon parse      	{ (fst $7,{rule_id=$1; rule_type=String.concat " " $3; rule_body=$5}::(snd $7)) }
	  | Code Ident Colon atype Arrow trans Semicolon parse      	{ ($1^(fst $8),{rule_id=$2; rule_type=String.concat " " $4; rule_body=$6}::(snd $8)) }

%%
