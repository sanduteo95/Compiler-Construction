
(* The type of tokens. *)

type token = 
  | WHILE
  | TYPE
  | TO
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
  | NULL
  | NOTEQ
  | NEGATE
  | MODULUS
  | MINUS
  | LET
  | LESS
  | LEQ
  | LEFT_ROUND_BRACKET
  | LEFT_CURLY_BRACKET
  | LAMBDA
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | GREATER
  | GEQ
  | FUNCTION
  | FUN
  | FOR
  | FLOAT of (float)
  | EQ
  | EOF
  | ELSE
  | DIVIDE
  | CONTINUE
  | COMMA
  | BREAK
  | BOOL of (bool)
  | ASSIGN
  | AND
  | ADDRESS_OF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val parse: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.program)
