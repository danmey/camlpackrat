{
  open Pegparser
}

let newline      =  '\n' | '\r'
let comment      =  '#'[^'\n']*'\n'
let whitespace   =  "\t" | " " | newline
let letter       = ['a'-'z''A'-'Z']
let digit        = ['0'-'9']
let misc         = ['_'] (* '+''-''_''^''*''&''$''%'':''.''/''\\''<''>''['']''~''='] *)
let symbol       = letter|misc
let string       = ('"'[^'"']*'"')|('|'[^'|']*'|')
let code         = ('{'[^'}']*'}')
let ident        = symbol+(symbol | digit | '-')*

rule token = parse
    whitespace          { token lexbuf }
  | newline             { token lexbuf }
  | comment             { token lexbuf }
  | ident as name       { Ident(name) }
  | "<-"                { Arrow }
  | ":"                 { Colon }
  | ";"                 { Semicolon }
  | "\""                { Quote }
  | "/"                 { Slash }
  | "*"			{ Star }
  | "("                 { Lb }
  | ")"                 { Rb }
  | "?"			{ Question }
  | "+"			{ Plus }

  | code as str         { Code (String.sub str 1 ((String.length str)-2)) }
  | string as str       { Literal(String.sub str 1 ((String.length str)-2)) }
  | eof	                { Eof }
(*  | "|"                 { Alternative } *)
      
