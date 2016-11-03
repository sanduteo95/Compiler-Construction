open Lexing
open Syntax
open Printf
open Exp_lex
open Exp_par
open Exp_eval
open Exp_errors
open Exp_opt
(* open Formatter *)

(** The argument for optimisation, which is optional. *)
let o = Sys.argv.(1)

(** The two arguments for the file names, which are not optional. *)
let arg1 = ref 1
let arg2 = ref 2
(** The name of the file. *)
let filename = ref ""
(** The start and end time of the evaluator. *)
let start_time = ref 0.0
let end_time = ref 0.0

(** Function reads from the input channel and adds to a buffer. *)
let rec read_to_empty ic buf =
    try
        let line = input_line ic in
        if line = "" then buf
        else (Buffer.add_string buf line;
            Buffer.add_string buf "\n";
            read_to_empty ic buf);
    with e -> close_in_noerr ic; raise e

(** Helper function to print file name, line and character position, as well as problem character. *)
let print_error lexbuf =
    let pos = lexbuf.lex_curr_p in
    eprintf "File \"%s\", line %d, character %d: %s\n" !filename pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) (lexeme lexbuf)

(** Calls the lexer and parser and prints errors. *)
let parse_with_error lexbuf =
    try parse read lexbuf with
        | SyntaxError msg -> prerr_string (msg ^ "\n"); exit (-1)
        | Error -> prerr_string ("Parse error: "); print_error lexbuf; exit (-1)

(** Function checks if there were any errors thrown.  *)
let eval_with_error program =
    start_time := Unix.gettimeofday ();
    try eval program with
        | TypeError (msg, name) -> prerr_string ("Error: " ^ msg ^ name ^ ".\n"); exit (-1)
        | FunctionError msg -> prerr_string ("Error: " ^msg ^ "\n"); exit (-1)
        | DivisionError msg -> prerr_string ("Error: " ^msg ^ "\n"); exit (-1)
        | NotImplementedError msg -> prerr_string ("Error: " ^msg ^ "\n"); exit (-1)
        | VariableDeclaration (msg, var) -> prerr_string ("Error: " ^msg ^ var ^ ".\n"); exit (-1)

(** Function tests the result of the evaluation.  *)
let test_result (steps, result) =
    end_time := Unix.gettimeofday ();
    let string_of_result =
        (match result with
            | Null -> "a null value"
            | Unit -> "()"
            | String(s) -> String.sub s 1 (String.length s - 2)
            | Integer(i) -> string_of_int i
            | Float(f) -> string_of_float f
            | Boolean(b) -> string_of_bool b
            | Pointer(_) -> "a pointer to another variable"
            | Tuple(_) -> "a tuple"
            | Fundef(ps, _) -> "a function with " ^ string_of_int (List.length ps) ^ " parameters" )
    in
    (* printf "%s" string_of_result *)
    filename := Sys.argv.(!arg2);
    let ic = open_in !filename in
    let actual_result = input_line ic in
    printf "%d %b %f" steps (String.equal string_of_result actual_result) (!end_time -. !start_time);
    close_in ic

(** The function decides if the problem needs optimisation. *)
let optimise expression =
    if(String.equal o "-o") then opt expression else expression

(** Function reads the file and prints the resulting parse tree.  *)
let read_file_and_print_tree =
    if (String.equal o "-o") then arg1 := !arg1+1 else arg1 := !arg1;
    arg2 := !arg1+1;
    filename := Sys.argv.(!arg1);
    let ic = open_in !filename in
    read_to_empty ic (Buffer.create 1)
    |> Buffer.contents
    |> Lexing.from_string
    |> parse_with_error
    |> optimise
    (* |> print_program; *)
    |> eval_with_error
    |> test_result;
    close_in ic
