{
open Exp_par
exception SyntaxError of string 
}

let int = ['0'-'9'] ['0'-'9']* 
let id = "_" ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let type = "int"
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =  
	parse
	| white  { read lexbuf }  
	| newline  { read lexbuf }  
	| int  { INT (int_of_string (Lexing.lexeme lexbuf)) }
	| id { ID (Lexing.lexeme lexbuf)}
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
	| "<="  { LEQ }
	| ">="  { GEQ }
	| "==" { EQ }
	| "!="  { NOTEQ }
	| "&&"  { AND }
	| "||"  { OR }
	| '!'  { NOT }
	| '='  { ASSIGN }
	| '''  { DEREF }
	| type  { TYPE }
	| "if"  { IF }
	| "else"  {ELSE}
	| "while"  { WHILE }
	| "let"  { LET }
	| "in"  { IN }
	| "read_int"  { READ }
	| "print_int"  { PRINT }
	| _  { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
	| eof  { EOF }