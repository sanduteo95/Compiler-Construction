open Exp_lex 
open Lexing 
open Printf
open Formatter
let filename = Sys.argv.(1)

(** Helper function to print file name, line and character position, as well as problem character. *)
let print_error lexbuf =   
	let pos = lexbuf.lex_curr_p in   
	eprintf "File \"%s\", line %d, character %d: %s\n" filename pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) (lexeme lexbuf)

(** Calls the lexer and parser and prints errors. *)
let parse_with_error lexbuf =   
	try Exp_par.parse Exp_lex.read lexbuf with
  	| SyntaxError msg -> prerr_string ("\t" ^ msg ^ "\n");                       
  						 exit (-1)   
  	| Exp_par.Error -> prerr_string ("\t Parse error: ");                    
  					   print_error lexbuf;                       
  					   exit (-1)

(** Function reads from the input channel and adds to a buffer. *)
let rec read_to_empty ic buf =    
  try
    let line = input_line ic in
    if line = "" then buf     
    else (Buffer.add_string buf line; 
          Buffer.add_string buf "\n";
          read_to_empty ic buf);
  with e -> close_in_noerr ic; raise e  

(** Function reads the file and prints the resulting parse tree.  *)
let read_file_and_print_tree =   
  let ic = open_in filename in read_to_empty ic (Buffer.create 1)
 	|> Buffer.contents  
 	|> Lexing.from_string  
 	|> parse_with_error  
 	|> print_program;
  close_in ic


(** Function prints that the test was successfuly. *)
let print_success _ = printf " \t Test passed! \n"

(** Function reads the file and prints whether there was an error or not. (and the error) *)
(*let read_file_and_print_success =   
  let ic = open_in filename in read_to_empty ic (Buffer.create 1)
  |> Buffer.contents  
  |> Lexing.from_string  
  |> parse_with_error
  |> print_success;
  close_in ic*)

