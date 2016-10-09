## Description
Write description here. 

## Installation
Provide steps on how to extract code from GIT and how to build it.

## Syntax
Seq of expression * expression
	
	e.g.
	expression;
	expression;

If of expression * expression * expression 
	
	e.g.
	if (expression) {
		expression;
	}
	else {
		expression;
	}

While of expression * expression 
	
	e.g.
	while (expression) {
		expression;
	}

Asg of expression * expression
	
	e.g.
	expression = expression;

Deref of expression
	
	e.g.
	There are two cases: 
	case 1. x = x + 1; (the x in right-hand side of the equation needs to be dereferenced)
	case 2. return x;

Negate of expression (used to replace the NOT operator, as it is applied to two operations and I couldn't make sense of that)

	e.g.
	!expression

Operator of opcode * expression * expression 
	
	e.g.
	expression _ expression

Application of expression * expression (* e(e) *)
	
	e.g.
	Where a function is applied to an expression, given that the function was already defined:
	function f {
		...
	}
	...
	f(expression)

Const of int 

	e.g. 7

Readint
	
	e.g. read()

Printint of expression

	e.g. print(...)

Identifier of string 
	
	e.g. x

Let of string * expression * expression (* let x = e in e *)
	
	e.g. let x = expression in expression;

New of string * expression * expression (* new x = e in e *)

	e.g.
	var x = expression;
	expression;

## Code example
For example, a program such as: 
	
	function main () {
		var x=1; 
		x=x+1; 
		return x;
	}

would have the parse tree:

	[("main", [],
	   New ("x", Const 1,
	    Seq
	     (Asg (Identifier "x", Operator (Plus, Deref (Identifier "x"), Const 1)),
	     Deref (Identifier "x"))))]

Or a program such as:

	function double(var x) {
		var y=6;
		if(x>y) {
			return 2*x;
		}
		else {
			return y;
		}
	}
	function main() {
		var x=2;
		while(x>=10) {
			x=x+1;
		}
		return x;
	}

would have the parse tree:

	[("double", [x], 
		New(y, Const(6),
			If(Operator(Greater, Deref(Identifier(x)), Deref(Identifier(y))),
				Operator(Times, Const(2), Deref(Identifier(x))),
	 			Deref(Identifier(y))))
	, ("main", [], 
		New(x, Const(2),
			Seq(
				While(Operator(Geq, Deref(Identifier(x)), Const(10)),
	 				Asg(Identifier(x), Operator(Plus, Deref(Identifier(x)), Const(1))),
	 			Deref(Identifier(x))))
	 ]

## Tests
Point out the test script. 
