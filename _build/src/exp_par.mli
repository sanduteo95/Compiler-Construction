
(* The type of tokens. *)

type token = 
  | WHILE
  | TYPE
  | TIMES
  | TEXT of (string)
  | SEMI_COLLON
  | RIGHT_ROUND_BRACKET
  | RIGHT_CURLY_BRACKET
  | RETURN
  | READ
  | PRINT
  | PLUS
  | OR
  | NOTEQ
  | NEGATE
  | MODULUS
  | MINUS
  | LET
  | LESS
  | LEQ
  | LEFT_ROUND_BRACKET
  | LEFT_CURLY_BRACKET
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | GREATER
  | GEQ
  | FUNCTION
  | EQ
  | EOF
  | ELSE
  | DIVIDE
  | COMMA
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val parse: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.program)
