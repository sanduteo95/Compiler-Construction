(** The formatter for the parse tree. *)
open Syntax
open Printf

(** Prints each parameter. If there are no parameters, prints an empty string. *)
let print_param  = function 
	| None -> printf ""
	| Param(s) -> printf "%s" s

(** Prints the parameters that the function takes, as a list. *)
let rec print_parameters = function
	| [] -> printf ""
	| [p] -> print_param p
	| p::ps -> print_param p; printf ", "; print_parameters ps

(** Prints each operator type. *)
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

(** Prints each argument, which can only be of three types. *)
let rec print_arg = function
	| Const(i) -> printf "Const %d" i
	| Deref(Identifier(s)) -> printf "Deref (Identifier \"%s\")" s
	| Operator(op, e1, e2) -> printf "Operator("; print_operator op; printf ", "; print_arg e1; printf ", "; print_arg e2; printf ")"
	| _ -> eprintf "\nThe function cannot take this type of argument!\n"; exit(-1)

(** Prints the list of arguments. *)
let rec print_arg_list = function
	| [] -> printf ""
	| [arg] -> print_arg arg
	| arg::args -> print_arg arg; printf ", "; print_arg_list args

(** Prints a nicely formatted string for each syntx type, in the form of a tree. *)
let rec print_content content tab = match content with
	| Nothing -> printf " Nothing"
  	| Seq(e1, e2) -> printf "%s" tab; printf "Seq(\n"; print_content e1 (tab^"\t"); printf ",\n "; print_content e2 (tab^"\t"); printf ")"
  	| While(e1, e2) -> printf "%s" tab; printf "While("; print_content e1 ""; printf ",\n "; print_content e2 (tab^"\t"); printf ")"
  	| If(e1, e2, e3) -> printf "%s" tab; printf "If("; print_content e1 ""; printf ",\n"; print_content e2 (tab^"\t"); printf ",\n "; print_content e3 (tab^"\t")
  	| Asg(e1, e2) -> printf "%s" tab; printf "Asg("; print_content e1 (tab^"\t"); printf ", "; print_content e2 ""
  	| Deref(e) -> printf "%s" tab; printf "Deref("; print_content e ""; printf ")"
  	| Negate(e1) -> printf "Negate("; print_content e1 ""; printf ")"
  	| Operator(op, e1, e2) -> printf "%s" tab; printf "Operator("; print_operator op; printf ", "; print_content e1 ""; printf ", "; print_content e2 ""; printf ")"
  	| Application(e, es) -> printf "%s" tab; printf "Application("; print_content e ""; printf ", ["; print_arg_list es; printf "])"; printf ")"
  	| Text(s) -> printf "Text(%s)" s
  	| Const(i) -> printf "Const %d" i
  	| Read -> printf "%s" tab; printf "Read()"
  	| Print(e) -> printf "%s" tab; printf "Print("; print_content e ""; printf ")"
  	| Identifier(s) -> printf "Identifier \"%s\"" s
  	| Let(s, e1, e2) -> printf "%s" tab; printf "Let \"%s\", " s; print_content e1 ""; printf "\n "; print_content e2 (tab^"\t"); printf ")"
  	| New(s, e1, e2) -> printf "%s" tab; printf "New \"%s\", " s; print_content e1 ""; printf ",\n"; print_content e2 (tab^"\t"); printf ")"

(** Prints the name of the function, parameters it takes and its content in the syntax format. *)
let print_function (name, parameters, content) = 
	printf "(\"%s\"," name;
	printf " ["; print_parameters parameters; printf "], ";
	printf "\n"; print_content content "\t"

(**  Prints each of the functions and their definitions. *)
let rec print_functions = function 
	| [] -> printf "]\n"
	| [f] -> print_function f; printf ")]\n"
	| f::fs -> print_function f;  printf "),\n "; print_functions fs

(** Prints the program as a list of function definitions. *)
let print_program p = match p with
	| [("", [], Nothing)] -> printf "[ Nothing ] \n"
	| _ -> printf "["; print_functions p
