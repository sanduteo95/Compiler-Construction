open Exp_lex 
open Lexing 
open Printf
open Syntax 

let print_position lexbuf =   
	let pos = lexbuf.lex_curr_p in   
	eprintf "Pos %d:%d:%d\n" pos.pos_lnum pos.pos_bol pos.pos_cnum

let parse_with_error lexbuf =   
	try Exp_par.parse Exp_lex.read lexbuf with
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
	| Modulus -> printf "Modulus"
	| Less -> printf "Less"
  	| Leq -> printf "Leq"
  	| Greater -> printf "Greater"
  	| Geq -> printf "Geq"
  	| Eq -> printf "Eq"
  	| Noteq -> printf "Noteq"
  	| And -> printf "And"
  	| Or -> printf "Or"

let rec print_arg = function
	| Const(i) -> printf "Const %d" i
	| Deref(Identifier(s)) -> printf "Deref (Identifier \"%s\")" s
	| Operator(op, e1, e2) -> printf "Operator("; print_operator op; printf ", "; print_arg e1; printf ", "; print_arg e2; printf ")"
	| _ -> eprintf "\nThe function cannot take this type of argument!"

let rec print_arg_list = function
	| [] -> printf ""
	| [arg] -> print_arg arg
	| arg::args -> print_arg arg; printf ", "; print_arg_list args

let rec print_content content tab = match content with
	| Nothing -> printf " Nothing"
  	| Seq(e1, e2) -> printf "%s" tab; printf "Seq(\n"; print_content e1 (tab^"\t"); printf ",\n "; print_content e2 (tab^"\t"); printf ")"
  	| While(e1, e2) -> printf "%s" tab; printf "While("; print_content e1 ""; printf ",\n "; print_content e2 (tab^"\t"); printf ")"
  	| If(e1, e2, e3) -> printf "%s" tab; printf "If("; print_content e1 ""; printf ",\n"; print_content e2 (tab^"\t"); printf ",\n "; print_content e3 (tab^"\t")
  	| Asg(e1, e2) -> printf "%s" tab; printf "Asg("; print_content e1 (tab^"\t"); printf ", "; print_content e2 ""
  	| Deref(e) -> printf "%s" tab; printf "Deref("; print_content e ""; printf ")"
  	| Negate(e1) -> printf "Negate("; print_content e1 (tab^"\t"); printf ")"
  	| Operator(op, e1, e2) -> printf "%s" tab; printf "Operator("; print_operator op; printf ", "; print_content e1 ""; printf ", "; print_content e2 ""; printf ")"
  	| Application(e, es) -> printf "%s" tab; printf "Application("; print_content e ""; printf ", ["; print_arg_list es; printf "])"; printf ")"
  	| Text(s) -> printf "Text(%s)" s
  	| Const(i) -> printf "Const %d" i
  	| Readint -> printf "%s" tab; printf "Readint()"
  	| Printint(e) -> printf "%s" tab; printf "Printint("; print_content e ""; printf ")"
  	| Identifier(s) -> printf "Identifier \"%s\"" s
  	| Let(s, e1, e2) -> printf "%s" tab; printf "Let(%s, " s; print_content e1 ""; printf "\n "; print_content e2 (tab^"\t"); printf ")"
  	| New(s, e1, e2) -> printf "%s" tab; printf "New(%s, " s; print_content e1 ""; printf ",\n"; print_content e2 (tab^"\t"); printf ")"

let print_function (name, parameters, content) = 
	printf "(\"%s\"," name;
	printf " ["; print_parameters parameters; printf "], ";
	printf "\n"; print_content content "\t"

let rec print_functions = function 
	| [] -> printf "]\n"
	| [f] -> print_function f; printf "\n]\n"
	| f::fs -> print_function f;  printf ")\n, "; print_functions fs

let print_program p = 
	printf "["; print_functions p

let _ =  
	read_to_empty (Buffer.create 1)
 	|> Buffer.contents  
 	|> Lexing.from_string  
 	|> parse_with_error  
 	|> print_program
 
