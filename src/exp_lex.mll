{
open Exp_par
exception SyntaxError of string 
}

let int = ['0'-'9'] ['0'-'9']* 
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let type = "var"
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =  
	parse
	| white  { read lexbuf }  
	| newline  { read lexbuf }  
	| int  { INT (int_of_string (Lexing.lexeme lexbuf)) }
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
	| '%'  { MODULUS }
	| "<="  { LEQ }
	| "<"  { LESS }
	| ">="  { GEQ }
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
	| "let"  { LET }
	| "in"  { IN }
	| "read"  { READ }
	| "print"  { PRINT }
	| "function"  { FUNCTION }
	| id { ID (Lexing.lexeme lexbuf)}
	| _  { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
	| eof  { EOF }