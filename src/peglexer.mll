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
let chclass      = ('['[^']']*']')
let action         = ('{'[^'}']*'}')
let code         = ("%{"_*"%}")
let ident        = symbol+(symbol | digit | '-')*

rule token = parse
    whitespace          { token lexbuf }
  | newline             { token lexbuf }
  | comment             { token lexbuf }
  | code as str         { Code(String.sub str 2 ((String.length str)-4)) }
  | chclass as str 	{ Class (String.sub str 1 ((String.length str)-2)) }
  | action as str       { Action (String.sub str 1 ((String.length str)-2)) }
  | string as str       { Literal(String.sub str 1 ((String.length str)-2)) }
  | ident as name       { Ident(name) }
  | "<-"                { Arrow }
  | ":"                 { Colon }
  | ";"                 { Semicolon }
  | "\""                { Quote }
  | "/"                 { Slash }
  | "*"			{ Star }
  | "("                 { Lb }
  | ")"                 { Rb }
  | "."			{ Dot }
  | "?"			{ Question }
  | "+"			{ Plus }
  | "!"			{ Shreek }
  | "&"			{ Ampersand }
  | eof	                { Eof }
      
