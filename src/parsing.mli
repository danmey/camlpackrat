module Parsing :
  sig
    exception Noparse
    val some : ('a -> bool) -> 'a list -> 'a * 'a list
    val ( ++ ) : ('a -> 'b * 'c) -> ('c -> 'd * 'e) -> 'a -> ('b * 'd) * 'e
    val many : ('a -> 'b * 'a) -> 'a -> 'b list * 'a
    val ( >> ) : ('a -> 'b * 'c) -> ('b -> 'd) -> 'a -> 'd * 'c
    val ( || ) : ('a -> 'b) -> ('a -> 'b) -> 'a -> 'b
    type lex_token = Name of string | Number of string | Operator of string
    val explode : string -> char list
    val implode : char list -> string
    val digit : char list -> char * char list
    val alpha : char list -> char * char list
    val number : char list -> char list * char list
    val is_white : char -> bool
    val white : char list -> char * char list
    val whites : char list -> char list * char list
    val t : 'a -> 'a list -> 'a * 'a list
  end
