%{
	open Helper
%}

%token Eof
%token <string> Ident 
%token <string> Literal
%token <string> Code
%token Slash Star Lb Rb
%token Colon Arrow Semicolon Quote Question Plus

%type <Helper.prule list> stmnt
%start stmnt
%%

term:
	  Ident		     { Rule($1) }
	| Literal 	     { Literal($1) }
 	| Lb expression Rb   { $2 }

term_list:
          term                       { $1 }
	| term term_list             { Group($1,$2) }

many:
          term_list                { $1 }
        | term_list Star           { Many($1) } 
        | term_list Star many      { Group(Many($1),$3) } 
        | term_list Plus           { Group($1,Many($1)) } 
        | term_list Plus many      { Group(Group($1,Many($1)),$3) } 

expression:	
          many                          { $1 } 
        | many Slash expression         { Choice($1,$3) }

trans:
          expression                    { $1 }
        | expression Code               { Transform($2,$1) }
	| expression Code Slash trans   { Choice(Transform($2,$1),$4) }
atype:
          Ident                         { [$1] }
        | Ident atype                   { [String.concat " " ($1::$2)] }

stmnt:
	  Eof { [] }
	| Ident Colon atype Arrow trans Semicolon stmnt     { {rule_id=$1; rule_type=String.concat " " $3; rule_body=$5}::$7 }

%%
