(** The formatter for the parse tree. *)
open Syntax
open Printf

let t = "    "

(** Prints each operator type. *)
let string_of_operator = function
	| Plus -> "Plus"
	| Minus -> "Minus"
	| Times -> "Times"
	| Divide -> "Divide"
	| Modulus -> "Modulus"
	| Less -> "Less"
  	| Leq -> "Leq"
  	| Greater -> "Greater"
  	| Geq -> "Geq"
  	| Eq -> "Eq"
  	| Noteq -> "Noteq"
  	| And -> "And"
  	| Or -> "Or"

let print_operator op = printf "%s" (string_of_operator op)

(** Prints the parameters that the function takes, as a list. *)
let rec print_parameters = function
	| [] -> printf ""
	| [p] -> printf "%s" p
	| p::ps -> printf "%s, " p; print_parameters ps

(** Prints the list of arguments. *)
let rec print_arg_list = function
	| [] -> printf ""
	| [arg] -> print_content arg ""
	| arg::args -> print_content arg ""; printf ", "; print_arg_list args
and
print_content content tab =
	printf "%s" tab;
	(match content with
	| Nothing -> printf "Nothing"
  	| Seq(e1, e2) -> printf "Seq(\n"; print_content e1 (tab^t); printf ",\n"; print_content e2 (tab^t); printf ")"
	| For(s, e1, e2, e3) -> printf "For("; print_content e1 ""; printf ", "; print_content e2 ""; printf ",\n"; print_content e3 (tab^t)
	| While(e1, e2) -> printf "While(\n"; print_content e1 (tab^t); printf ",\n"; print_content e2 (tab^t); printf ")"
  	| If(e1, e2, e3) -> printf "If(\n"; print_content e1 (tab^t); printf ",\n"; print_content e2 (tab^t); printf ",\n"; print_content e3 (tab^t)
  	| Asg(e1, e2) -> printf "Asg(\n"; print_content e1 (tab^t); printf ", \n"; print_content e2 (tab^t); printf ")"
  	| Deref(e) -> printf "Deref("; print_content e ""; printf ")"
  	| Negate(e) -> printf "Negate("; print_content e ""; printf ")"
  	| Operator(op, e1, e2) -> printf "Operator("; print_operator op; printf ", "; print_content e1 ""; printf ", "; print_content e2 ""; printf ")"
  	| Application(e, es) -> printf "Application(\n"; print_content e (tab^t); printf ",\n%s[" (tab^t); print_arg_list es; printf "])"; printf ")"
  	| Lambda (ps, e) ->  printf "Lambda(["; print_parameters ps; printf "], \n"; print_content e (tab^t); printf ")"
  	| MyString(s) -> printf "MyString(%s)" s
  	| MyInteger(i) -> printf "MyInteger %d" i
    | MyFloat(f) -> printf "MyFloat %f" f
  	| MyBoolean (b) -> printf "MyBoolean(%b)" b
	| MyNull -> printf "MyNull"
  	| MyTuple (es) -> printf "MyTuple("; print_arg_list es; printf ")"
  	| Read -> printf "Read()"
  	| Print(e) -> printf "Print("; print_content e ""; printf ")"
  	| Identifier(s) -> printf "Identifier \"%s\"" s
  	| Let(s, e1, e2) -> printf "Let \"%s\",\n" s; print_content e1 (tab^t); printf ",\n"; print_content e2 (tab^t); printf ")"
  	| New(s, e1, e2) -> printf "New \"%s\",\n" s; print_content e1 (tab^t); printf ",\n"; print_content e2 (tab^t); printf ")"
	| Break -> printf "Break"
	| Continue -> printf "Continue"
	| Block(s, e) -> printf "Block(%s, )" s; print_content e (tab^t); printf ")"
	| BreakBlock(s, e) -> printf "BreakBlock(%s, )" s; print_content e (tab^t); printf ")"
	| ContinueBlock(s, e) -> printf "ContinueBlock(%s, )" s; print_content e (tab^t); printf ")")

(** Prints the name of the function, parameters it takes and its content in the syntax format. *)
let print_function (name, parameters, content) =
	printf "(\"%s\"," name;
	printf " ["; print_parameters parameters; printf "], ";
	printf "\n"; print_content content t

(**  Prints each of the functions and their definitions. *)
let rec print_functions = function
	| [] -> printf "]\n"
	| [f] -> print_function f; printf ")]\n"
	| f::fs -> print_function f;  printf "),\n "; print_functions fs

(** Prints the program as a list of function definitions. *)
let print_program p = match p with
	| [("", [], Nothing)] -> printf "[ Nothing ] \n"
	| _ -> printf "["; print_functions p
