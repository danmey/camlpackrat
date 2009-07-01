%{
	open Helper
%}

%token Eof
%token <string> Ident 
%token <string> Literal
%token <string> Code
%token Slash Star Lb Rb
%token Colon Arrow Semicolon Quote Question Plus

%type <Helper.prule list> prule
%start prule
%%

prule:
	Eof { [] }
|	Ident Colon Ident Arrow expression Semicolon prule     { {rule_id=$1; rule_type=$3; rule_body=$5}::$7 }

literal:
	  Ident			{ [Rule($1)] }
	| Ident literal   	{ Rule($1)::$2 }
	| Literal             	{ [Literal($1)] }
	| Literal literal   	{ Literal($1)::$2 }
	| Lb expression Rb      { [$2] }


expression:	
        | literal                              { (Cat($1)) }
        | expression Code                      { Transform($2,$1) }
	| literal Star                         { Many(Cat($1)) }
	| literal Star expression              { Cat((Many(Cat($1)))::$3::[]) }
	| literal Plus                         { Cat($1@[Many(Cat($1))]) }
	| literal Plus expression              { Cat($1@[Cat((Many(Cat($1)))::$3::[])]) }
	| literal Slash expression             { Choice(Cat($1),$3) }
	| expression Code Slash expression     { Choice(Transform($2, $1),$4) }
%%
