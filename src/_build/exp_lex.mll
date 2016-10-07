{
	open Exp_par
	open Syntax
	exception SyntaxError of string 
}

let int = ['0'-'9'] ['0'-'9']* 
let name = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =  parse
	| white  { read lexbuf }  
	| newline  { read lexbuf }  
	| int  { INT (int_of_string (Lexing.lexeme lexbuf)) }
	| name  { NAME (Lexing.lexeme lexbuf)}
	| ','  { COMMA }
	| ';'  { SEMI_COLLON }
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
	| "int"  { NEW }
	| '{'  { LEFT_CURLY_BRACKET }
	| '}'  { RIGHT_CURLY_BRACKET }
	| '('  { LEFT_ROUND_BRACKET }
	| ')'  { RIGHT_ROUND_BRACKET}
	| "if"  { IF }
	| "else"  {ELSE}
	| "while"  { WHILE }
	| "temporary"  { LET }
	| "iread"  { READ }
	| "iprint"  { PRINT }
	| _  { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
	| eof  { EOF }