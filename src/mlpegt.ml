type ch_class = 
    Range of char * char
  | OneCharacter of char 
  | Negate of ch_class list
  | Classes of ch_class list

type ast = 
    Choice of ast * ast 
  | Group of ast * ast 
  | Literal of string 
  | Rule of string
  | Many of ast 
  | Transform of string * ast
  | Class of ch_class
  | And of ast
  | Not of ast
  | Any of ast
  | AssignVar of string * ast
  | Nothing
      
and arule = {rule_id:string; rule_type:string; rule_body:ast;}    
