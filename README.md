## Description
The programming language chosen is JavaScript-like, having found it the easiest to accommodate the needs for the syntax I have gone with. The initial OCaml syntax that was given to us HAS BEEN extended:
	
	Function definitions now consist of:
		- a function name
		- a list of parameters (which have been defined as a new type)
		- an expression

	The operations permitted on expressions have been extended too:
	 	- "modulus"
	 	- "less"
	 	- "greater"

	And the "not" operator has been removed, as it was replaced by the expression: Negate of expression.

	The expressions admitted by the type have been tweaked, as now an expression could do "Nothing".

	An output string can now be admitted as an expression too, in the form of Text(...) for the Printint functionality.
	
	Thee "Application" expression has been modified to be able to be applied to multiple expressions .

## Installation and Build
To extract the code from GIT you can either use the command line as explainged bellow or, by clicking on the "Clone or download" button, choose "Download Zip" to download a zip of the project in your preferred location. 

If command line is preferred, follow these steps:

	1. Open up a terminal and copy the following it: git clone https://github.com/sanduteo95/Compiler-Construction.git.
	2. Type in your username and password (if you haven't set up the environements to authenticate you by default).
	3. Then, to open the directory, type in "cd Compiler-Construction/".

After you have downloaded everything, you can run the parser and lexer by doing the following:
	
	1. While in the directory, type in "make" to build the project..
	2. Now, if you want to test it on my test cases, run "make all_tests".
	3. Otherwise, run "make own_test" and you will be prompted for the path to the file you want to test.
	5. If you want to remove the remains, run "make clean".


## Syntax
Here are a few examples of the types of syntax it likes.

Seq of expression * expression
	
	e.g. expression;
		 expression;

If of expression * expression * expression 
	
	e.g. if (expression) {
			 expression;
		 }
		 else {
			 expression;
		 };

While of expression * expression 
	
	e.g. while (expression) {
			 expression;
		 };

Asg of expression * expression
	
	e.g. x = expression;

Deref of expression
	
	e.g. x = x + 1; (the x in right-hand side of the equation needs to be dereferenced)

Negate of expression (used to replace the NOT operator, as it is applied to two operations and I couldn't make sense of that)

	e.g. !expression

Operator of opcode * expression * expression 
	
	e.g. expression _ expression (so far type checking isn't implemented)

Application of expression * expression (* e(e) *)
	
	e.g. Used when a function is applied to an expression, given that the function was already defined:
	function f {
		...
	}
	...
	x = f(expression);

	However, there are no check as of now to see if the function exists.

Const of int 

	e.g. 7

Readint
	
	e.g. x = read() (no type checking as of now)

Printint of expression

	e.g. print(expression)

Identifier of string 
	
	e.g. x

Let of string * expression * expression
	
	e.g. let x = expression in expression;

New of string * expression * expression

	e.g. var x = expression;
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
		New "x", Const 1,
			Seq(
				Asg(Identifier "x", Operator(Plus, Deref(Identifier "x"), Const 1),
	 			Deref(Identifier "x"))))]

Or a program such as:

	function double(var x) {
		var y=6;
		if(x>y) {
			return 2*x;
		}
		else {
			return y;
		};
	}
	function main() {
		var x=2;
		while(x>=10) {
			x=x+1;
		};
		return x;
	}

would have the parse tree:

	[("double", [x], 
		New "y", Const 6,
			If(Operator(Greater, Deref(Identifier "x"), Deref(Identifier "y")),
				Operator(Times, Const 2, Deref(Identifier "x")),
	 			Deref(Identifier "y"))),
	 ("main", [], 
		New "x", Const 2,
			Seq(
				While(Operator(Geq, Deref(Identifier "x"), Const 10),
	 				Asg(Identifier "x", Operator(Plus, Deref(Identifier "x"), Const 1)),
	 			Deref(Identifier "x"))))]

## Tests
There are 10 simple test cases in the "test cases" folder, which you can run using the Makefile I gave you. Then there are two more test cases, a bit more complicated, also included in the Makefile. If you want to run your own tests, go back to the "Installation and Build" section to find out how to do this.