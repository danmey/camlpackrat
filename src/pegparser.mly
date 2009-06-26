%{
	open Helper
%}

%token Eof
%token <string> Ident 
%token <string> Literal
%token <string> Code
%token Slash Star
%token Colon Arrow Semicolon Quote 

%type <Helper.prule list> prule
%start prule
%%

prule:
	
	Ident Colon Ident Arrow expression Code Semicolon Eof { [{rule_id=$1; rule_type=$3; rule_body=$5;ml_block=$6;}] }
|	Ident Colon Ident Arrow expression Code Semicolon prule { {rule_id=$1; rule_type=$3; rule_body=$5;ml_block=$6;}::$8 }
|	Ident Colon Ident Arrow expression Semicolon Eof { [{rule_id=$1; rule_type=$3; rule_body=$5;ml_block=""}] }
|	Ident Colon Ident Arrow expression Semicolon prule { {rule_id=$1; rule_type=$3; rule_body=$5;ml_block=""}::$7 }

literal:
	  Ident			{ [Rule($1)] }
	| Ident literal   	{ Rule($1)::$2 }
	| Literal             	{ [Literal($1)] }
	| Literal literal   	{ Literal($1)::$2 }
	| Literal Star	        { [Many(Literal($1))] }
	| Ident   Star	        { [Many(Literal($1))] }


expression:	
          literal                           { Cat($1) } 
	| literal Slash expression	    { Choice(Cat($1),$3) }
 
%%
