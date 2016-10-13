open Exp_lex 
open Lexing 
open Printf
open Formatter

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

let rec read_to_empty ic buf =    
  try
    let line = input_line ic in
    if line = "" then buf     
    else (Buffer.add_string buf line; 
          Buffer.add_string buf "\n";
          read_to_empty ic buf);
  with e -> close_in_noerr ic; raise e  

let read_file = 
  let ic = open_in Sys.argv.(1) in read_to_empty ic (Buffer.create 1)
 	|> Buffer.contents  
 	|> Lexing.from_string  
 	|> parse_with_error  
 	|> print_program;
  close_in ic
 