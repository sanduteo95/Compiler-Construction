{
open Exp_par
open Lexing
exception SyntaxError of string

(** Makes sure the number of lines in the lexbuf is correct. *)
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}


let type = "var"
let int = ['0'-'9'] ['0'-'9']*
let float = ['0'-'9'] ['0'-'9']* "." ['0'-'9'] ['0'-'9']*
let text = "\"" ['a'-'z' 'A'-'Z' '1'-'9' '.' '!' '?' ':' ',' '.' '-' ' ']+ "\""
let mybool = "true" | "false"

let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = "/*" ['a'-'z' 'A'-'Z' '.' '!' ',' '.' '-' '!' '=' '(' ')' ':' ';' ' ' '\n' '\t' '\r']+ "*/"

rule read =
	parse
	| white  { read lexbuf }
	| newline  { next_line lexbuf; read lexbuf }
	| int  { INT (int_of_string (Lexing.lexeme lexbuf)) }
	| float  { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
	| '&'  { ADDRESS_OF }
	| ','  { COMMA }
	| ';'  { SEMI_COLLON }
	| '{'  { LEFT_CURLY_BRACKET }
	| '}'  { RIGHT_CURLY_BRACKET }
	| '('  { LEFT_ROUND_BRACKET }
	| ')'  { RIGHT_ROUND_BRACKET}
	| '+'  { PLUS }
	| '-'  { MINUS }
	| '*'  { TIMES }
	| '/'  { DIVIDE }
	| '\\'  { FUN }
	| '%'  { MODULUS }
	| "<="  { LEQ }
	| "<"  { LESS }
	| "=>"  { GEQ }
	| ">"  { GREATER }
	| "==" { EQ }
	| "!="  { NOTEQ }
	| "&&"  { AND }
	| "||"  { OR }
	| '!'  { NEGATE }
	| '='  { ASSIGN }
	| type  { TYPE }
	| "return"  { RETURN }
	| "if"  { IF }
	| "else"  {ELSE}
	| "while"  { WHILE }
	| "for"  { FOR }
	| ":"  { TO }
	| "let"  { LET }
	| "in"  { IN }
	| "read"  { READ }
	| "print"  { PRINT }
	| "function"  { FUNCTION }
	| "->"  { LAMBDA }
	| "NULL"  { NULL }
	| mybool  { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
	| id { ID (Lexing.lexeme lexbuf) }
	| text { TEXT (Lexing.lexeme lexbuf) }
	| comment  { read lexbuf }
 	| _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  	| eof { EOF }