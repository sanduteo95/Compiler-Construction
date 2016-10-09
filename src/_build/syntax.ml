(** Type used to define a function's parameter. *)
type parameter = 
	| None
	| Param of string

(** Type used to define a variety of operators. *)
type opcode =
  	| Plus | Minus | Times | Divide | Modulus
  	| Less | Leq | Greater| Geq | Eq | Noteq
  	| And | Or 

(** Type used to define an expression, which is just the behaviour of the program. *)
type expression =
  	| Nothing (** Only used when the program doesn't have any code inside. *)
  	| Seq of expression * expression (** Representation of a sequence of lines of code. *)
  	| While of expression * expression (** Representation of a while loop. *)
  	| If of expression * expression * expression (** Representation of an if-else statement. *)
  	| Asg of expression * expression (** Representation of an assignment. *)
  	| Deref of expression (** Representation of a variable on the right-hand side of an assignment or returned from a function. *)
  	| Negate of expression (** Representation of a negation. *)
  	| Operator of opcode * expression * expression (** Representation of an expression containing an operator. *)
  	| Application of string * string list (** Representation of a function application. *)
  	| Text of string (** Representation of a piece of text. *)
  	| Const of int (** Representation of a constant. *)
  	| Readint (** Representation of the reading functionality. *)
  	| Printint of expression (** Representation of the printing functionality. *)
  	| Identifier of string (** Representation of a variable. *)
  	| Let of string * expression * expression (** Representation of a let-in statement. *)
  	| New of string * expression * expression (** Representation of an initialisation. *)

(** Type used to define a function, consisting of a function name, a parameter list and an exprresion - the code inside the function, represented in ocaml. *)
type fundef = string * parameter list * expression 

(** Type used to define an entire program, which consists of multiple functions. *)
type program = fundef list