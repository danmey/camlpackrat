%{
	open Helper
%}

%token EOF
%token <string> Ident 
%token <string> Literal
%token Slash
%token Colon Arrow Semicolon Quote 

%type <Helper.prule> prule
%start prule
%%

prule:
	Ident Colon Ident Arrow expression Semicolon { {rule_id=$1; rule_type=$3; rule_body=$5} }

literal:
	Literal             { [Literal($1)] }
	| Literal literal   { Literal($1)::$2 }

	
expression:	
	literal                             { Cat($1) } 
	| literal Slash expression	    { Choice(Cat($1),$3) }
  

%%
