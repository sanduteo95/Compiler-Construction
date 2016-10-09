type parameter = 
	| None
	| Param of string

type opcode =
  	| Plus | Minus | Times | Divide | Modulus
  	| Less | Leq | Greater| Geq | Eq | Noteq
  	| And | Or 

type expression =
  	| Nothing
  	| Seq of expression * expression (* e; e *)
  	| While of expression * expression (* while (e) { e } *)
  	| If of expression * expression * expression (* if (e) { e } else { e } *)
  	| Asg of expression * expression (* e = e *)
  	| Deref of expression (* (e)' *)
  	| Negate of expression (* !e *)
  	| Operator of opcode * expression * expression (* e + e *)
  	| Application of expression * expression (* e(e) *)
  	| Const of int (* 7 *)
  	| Readint (* read_int () *)
  	| Printint of expression (* print_int (e) *)
  	| Identifier of string (* x *)
  	| Let of string * expression * expression (* let x = e in e *)
  	| New of string * expression * expression (* int x = e; e *)

type fundef = string * parameter list * expression 

type program = fundef list