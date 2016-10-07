open Lexing 
open Printf

let print_position lexbuf =   
	let pos = lexbuf.lex_curr_p in   
	eprintf "Pos %d:%d:%d\n" pos.pos_lnum pos.pos_bol pos.pos_cnum

let parse_with_error lexbuf =   
	try Exp_par.top Exp_lex.read lexbuf with
  	| SyntaxError_exn msg -> prerr_string (msg ^ ": ");                       
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

let _ =  
	read_to_empty (Buffer.create 1)
 	|> Buffer.contents  
 	|> Lexing.from_string  
 	|> parse_with_error  
 	|> print_endline