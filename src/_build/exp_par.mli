
(* The type of tokens. *)

type token = 
  | WHILE
  | TIMES
  | SEMI_COLLON
  | RIGHT_ROUND_BRACKET
  | RIGHT_CURLY_BRACKET
  | READ
  | PRINT
  | PLUS
  | OR
  | NOTEQ
  | NOT
  | NEW
  | MINUS
  | LEQ
  | LEFT_ROUND_BRACKET
  | LEFT_CURLY_BRACKET
  | INT of (int)
  | IF
  | ID of (string)
  | GEQ
  | EQ
  | EOF
  | ELSE
  | DIVIDE
  | DEREF
  | COMMA
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.fundef)
