open Lexing
open Syntax
open Printf
open Exp_lex
open Exp_par
open Exp_eval
open Exp_errors
open Exp_opt
open Formatter

(** The argument for optimisation, which is optional. *)
let flag = Sys.argv.(1)
(** The name of the file. *)
let filename = Sys.argv.(2)
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
    eprintf "File \"%s\", line %d, character %d: %s\n" filename pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) (lexeme lexbuf)

(** Calls the lexer and parser and prints errors. *)
let parse_with_error lexbuf =
    try parse read lexbuf with
        | SyntaxError msg -> prerr_string (msg ^ "\n"); exit (-1)
        | Error -> prerr_string ("Parse error: "); print_error lexbuf; exit (-1)

(** Function checks if there were any errors thrown.  *)
let eval_with_error program =
    start_time := Unix.gettimeofday ();
    try eval program with
        | VariableDeclaration (msg, name) | TypeError (msg, name) -> prerr_string ("Error: " ^ msg ^ name ^ ".\n"); exit (-1)
        | FunctionError msg | DivisionError msg | NotImplementedError msg -> prerr_string ("Error: " ^msg ^ "\n"); exit (-1)

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
    let ic = open_in (Sys.argv.(3)) in
    let actual_result = input_line ic in
    let test = if (String.equal string_of_result actual_result) then "passed" else "failed" in
    printf "%d %s %f" steps test (!end_time -. !start_time);
    close_in ic

(** The function chooses what to do. *)
let run expression = match flag with
        | "-o" -> opt expression |> eval_with_error |> test_result
        | "-p" -> print_program expression
        | "-e" -> eval_with_error expression |> test_result
        | _ -> failwith "Not yet."
        (* | "-i" -> interpret *)

(** Function reads the file and prints the resulting parse tree.  *)
let read_file_and_print_tree =
    let ic = open_in filename in
    read_to_empty ic (Buffer.create 1)
    |> Buffer.contents
    |> Lexing.from_string
    |> parse_with_error
    |> run;
    close_in ic
