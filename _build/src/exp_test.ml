open Exp_lex 
open Lexing 
open Syntax
open Printf
open Formatter
open Exp_store

let filename = Sys.argv.(1)

(** Function reads from the input channel and adds to a buffer. *)
let rec read_to_empty ic buf =    
  try
    let line = input_line ic in
    if line = "" then buf     
    else (Buffer.add_string buf line; 
          Buffer.add_string buf "\n";
          read_to_empty ic buf);
  with e -> close_in_noerr ic; raise e  
(** PART 1  *)

(** Helper function to print file name, line and character position, as well as problem character. *)
let print_error lexbuf =   
	let pos = lexbuf.lex_curr_p in   
	eprintf "File \"%s\", line %d, character %d: %s\n" filename pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) (lexeme lexbuf)

(** Calls the lexer and parser and prints errors. *)
let parse_with_error lexbuf =   
	try Exp_par.parse Exp_lex.read lexbuf with
  	| SyntaxError msg -> prerr_string (msg ^ "\n");                       
  						 exit (-1)   
  	| Exp_par.Error -> prerr_string ("Parse error: ");                    
  					   print_error lexbuf;                       
  					   exit (-1)

(** Function reads the file and prints the resulting parse tree.  *)
(* let read_file_and_print_tree =   
  let ic = open_in filename in read_to_empty ic (Buffer.create 1)
 	|> Buffer.contents  
 	|> Lexing.from_string  
 	|> parse_with_error  
 	|> print_program;
  close_in ic

(** Function prints that the test was successful. *)
let print_success _ = printf " \t Test passed! \n"

(** Function reads the file and prints whether there was an error or not. (and the error) *)
let read_file_and_print_success =   
  let ic = open_in filename in read_to_empty ic (Buffer.create 1)
  |> Buffer.contents  
  |> Lexing.from_string  
  |> parse_with_error
  |> print_success;
  close_in ic
*)

(** PART 2 *)

(** Function tests the result of the evaluation.  *)
let test_result result = 
  let string_of_result = 
    (match result with
    | Unit -> "()"
    | String(s) -> String.sub s 1 (String.length s - 2)
    | Integer(i) -> string_of_int i
    | Boolean(b) -> b
    | Pointer(_) -> "a pointer to another variable")
  in 
  let filename = Sys.argv.(2) in 
  let actual_result = input_line (open_in filename) in
  if (String.equal string_of_result actual_result) then printf "The function evaluates to the expected value: %s. \n" string_of_result
  else eprintf "The function was supposed to evaluate to %s, but it evaluated to %s. \n" actual_result string_of_result

(** Function looks at the content of the parsed function and then sees if it evaluates correctly. *)
let eval_function (_, _, content) = 
  eval_with_error content
  |> test_result

(** Function checks what type of program it is and rejects ones that we haven't implemented yet. *)
let eval_program program = match program with
  | [("", [], Nothing)] -> prerr_string ("Error: There are no functions to evaluate.\n"); exit (-1)
  | [func] -> eval_function func
  | _ -> prerr_string ("Error: Right now, we can only evaluate programs with one function definition. \n"); exit (-1)

(** Function reads the file and prints the result of evaluating the expression. *)
let read_file_and_print_result =  
  let ic = open_in filename in 
  read_to_empty ic (Buffer.create 1)
  |> Buffer.contents  
  |> Lexing.from_string  
  |> parse_with_error  
  |> eval_program;
  close_in ic