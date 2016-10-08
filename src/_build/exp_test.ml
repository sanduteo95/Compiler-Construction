open Exp_lex 
open Lexing 
open Printf
open Syntax 

let print_position lexbuf =   
	let pos = lexbuf.lex_curr_p in   
	eprintf "Pos %d:%d:%d\n" pos.pos_lnum pos.pos_bol pos.pos_cnum

let parse_with_error lexbuf =   
	try Exp_par.top Exp_lex.read lexbuf with
  	| SyntaxError msg -> prerr_string (msg ^ ": ");                       
  						 print_position lexbuf;                        
  						 exit (-1)   
  	| Exp_par.Error -> prerr_string "Parse error: ";                    
  					   print_position lexbuf;                       
  					   exit (-1)

let rec read_to_empty buf =    
	let s = read_line () in
    if s = "" then buf     
	else (Buffer.add_string buf s; 
          Buffer.add_string buf "\n";                
      	  read_to_empty buf)

let print_param  = function 
	| None -> printf ""
	| Param(s) -> printf "%s" s

let rec print_parameters = function
	| [] -> printf ""
	| [p] -> print_param p
	| p::ps -> print_param p; printf ", "; print_parameters ps

let print_operator = function
	| Plus -> printf "Plus"
	| Minus -> printf "Minus"
	| Times -> printf "Times"
	| Divide -> printf "Divide"
  	| Leq -> printf "Leq"
  	| Geq -> printf "Geq"
  	| Eq -> printf "Eq"
  	| Noteq -> printf "Noteq"
  	| And -> printf "And"
  	| Or -> printf "Or"
  	| Not -> printf "Not"

let rec print_content = function
	| Nothing -> printf ""
  	| Seq(e1, e2) -> printf " Seq("; print_content e1; printf ", "; print_content e2; printf ")"
  	| While(e1, e2) -> printf " While("; print_content e1; printf ", "; print_content e2; printf ")"
  	| If(e1, e2, e3) -> printf " If("; print_content e1; printf ", "; print_content e2; printf ", "; print_content e3; printf ")"
  	| Asg(e1, e2) -> printf " Asg("; print_content e1; printf ", "; print_content e2; printf ")"
  	| Deref(e) -> printf " Deref("; print_content e; printf ")"
  	| Operator(op, e1, e2) -> printf " Opertor("; print_operator op; printf ", "; print_content e1; printf ", "; print_content e2; printf ")"
  	| Application(e1, e2) -> printf " Application("; print_content e1; printf ", "; print_content e2; printf ")"
  	| Const(i) -> printf "Const("; printf "%d)" i
  	| Readint -> printf "read_int()"
  	| Printint(e) -> printf " Printint("; print_content e; printf ")"
  	| Identifier(s) -> printf " Identifiter( %s)" s
  	| Let(s, e1, e2) -> printf " Let("; printf "%s, " s; print_content e1; printf ", "; print_content e2; printf ")"
  	| New(s, e1, e2) -> printf " New("; printf "%s, " s; print_content e1; printf ", "; print_content e2; printf ")"

let print_fundef (name, parameters, content) = 
	printf "function name: %s\n" name;
	printf "parameters: ["; print_parameters parameters; printf "]\n";
	printf "function content: {"; print_content content; printf "}\n"

let _ =  
	read_to_empty (Buffer.create 1)
 	|> Buffer.contents  
 	|> Lexing.from_string  
 	|> parse_with_error  
 	|> print_fundef
 
