## Description
Write descroption here. 

## Installation
Provide steps on how to extract code from GIT and how to build it.

## Syntax
Seq of expression * expression (* e; e *)
	
	expression; expression;

If of expression * expression * expression 
	
	IF (expression) {
		expression
	}
	ELSE {
		expression
	}

While of expression * expression 
	
	WHILE (expression) {
		expression
	}

Asg of expression * expression
	
	expression = expression;

Deref of expression (* !e *)
	
	???

Operator of opcode * expression * expression (* e + e *)
	
	expression _ expression
	(Not ??? )

Application of expression * expression (* e(e) *)
	
	expression(expression)

Const of int (* 7 *)

	int

Readint (* read_int () *)
	
	iread(...)

Printint of expression (* print_int (e) *)

	iprint(...)

Identifier of string (* x *)
	
	var

Let of string * expression * expression (* let x = e in e *)
	
	temporary var=expression;
	expression

New of string * expression * expression (* new x = e in e *)

	int var = expression;
	expression
## Code example
Give an example of what the program does.
For example, a program such as: main () {int x=1; x=x+1; return x}
would have the parse tree:
	[("main", [],
	   New ("x", Const 1,
	    Seq
	     (Asg (Identifier "x", Operator (Plus, Deref (Identifier "x"), Const 1)),
	     Deref (Identifier "x"))))]

## Tests
Point out the test script. 

Tasks
1. Define a concrete syntax of the language, to correspond to the abstract syntax tree below.
2. Implement a lexer and a parser for the language.
3. Test your parser on 10 small test programs covering each syntactic feature and two larger programs implementing the root bisection algorithm [https://en.wikipedia.org/wiki/Bisection_method] first recursively then iteratively. 

Marking Scheme
1. Submit a simple step-by-step instructions for extracting your code from SVN or GIT.
2. Provide a README.md markdwon file with basic documentation for the syntax.
3. The README should indicate clearly how to build the compiler
4. The README should point out clearly a test script that will execute all 12 test cases in batch mode. 

Points will be awarded as follows:
2 points for the small test cases
4 points for the large test cases
2 points for software engineering process (clear documentation and scripts)
2 points for elegance
