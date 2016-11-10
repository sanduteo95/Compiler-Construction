## Description
The chosen programming language is JavaScript-like, having found it the easiest to accommodate the needs of the syntax I have gone with. The initial syntax that was given to us has been extended:

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

	An output string can now be admitted as an expression too, in the form of Text(...) for Prin.

	Thee "Application" expression has been modified to be able to be applied to multiple expressions .

## Installation and Build
To extract the code from GIT you can either use the command line as explainged bellow or, by clicking on the "Clone or download" button, choose "Download Zip" to download a zip of the project in your preferred location.

If command line is preferred, follow these steps:

	1. Open up a terminal and run: "git clone https://github.com/sanduteo95/Compiler-Construction.git".
	2. Type in your username and password if asked for them.
	3. Then, to open the directory, type in "cd Compiler-Construction/".

After you have downloaded everything, you can run the program by doing the following:

	1. While in the same directory, type in "make" to build the project.
	2. Now, if you want to:
		- see the parse tree, run "make parse"
		- evaluate the expression, run "make evaluate"
		- evaluate the expression after it was optimised, run "make optimise"
		- interpret the expression, run "make interpret"
		- generate machine code for the expression, run "make generate"
	3. If you want to clean up the project, run "make clean".
	!!! Make sure if you're testing the evaluator that you create another .txt file with the same name, inside a "results" folder,
	that contains the expected result.

If you want to run your own test, type in "./exp_test.native <flags> <path to file>", where:
	- <flags>:
		- "-o -p": print the optimised parse tree
		- "-p": print the non-optimised parse tree
		- "-o -e": evaluate the optimised parse tree
		- "-e": evaluate the non-optimised parse tree
	- <path to file>: e.g. test/part4/test1.txt


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
		 }

While of expression * expression

	e.g. while (expression) {
			 expression;
		 }

Asg of expression * expression

	e.g. x = expression;
	!!! Now the left-handside of an assignment can also be a "let" or an "if-else"

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
	!!! Also, the function can be a lambda function.

Const of int

	e.g. 7

Readint

	e.g. x = read() (no type checking as of now)

Printint of expression

	e.g. print(expression)
	!!! Expressions can be anything that represents a value (so not another print, or a return, if-else statement or while statement), as well as a string. (e.g. print("Hey."))

Identifier of string

	e.g. x
	!!! Variable names can only consist of:
		- lower and upper case letters
		- _ (not accepted at the start of the variable)
		- digits (not accepted at the start of the variable)

Let of string * expression * expression

	e.g. let x = expression in expression;

New of string * expression * expression

	e.g. var x = expression;
		 expression;

It is sensible to point out that the language also contains multiline comments of the form:

	\* Comment *\
	!!! Comments are sentences using sensible punctuation marks, but no characters such as: @, #, %, $, ^ etc.

## Parser and Lexer
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

## Evaluator (phase 1)
As of now, the evaluator cannot evaluate any function applications and files containing more than one function definitions in them.

I have decided to implement "Let" (call-by-value, the slow version) and "New", although I am aware at least that "New" would not be correct if there were more than one functions so that will need to be reimplemented (moreover, if we try to define a variable inside a while loop, it will not be happy). I only left them in so that I can use more of the syntax and in the hope of being able to evaluate tests from the previous part. Also, I have commented out the evaluation for "Read" and "Print", but if you want you can uncomment them in exp_store.ml and they should work just fine, but for the sake of test clarity I left them out.

As for uniformity over the syntax, I believe it is reasonaly uniform, as it accepts anything that my parser accepts. I have also extended my parser so that I can assign variable a pointer to another variable ( x=&y ), but it will not allow printing a pointer, as that would not make sense.

On top of that, I have also implemented some error handling, so that error messages can be more precise:
	- TypeError: it prints out what type the operator expected
	- VariableDeclaration: it prints out what variable has not been declared or has already been declared
	- NotImplementedError: it prints out that a functionality is not yet implemented
	- DivisionError: if trying to divide or get the rest of a division by 0, it prints an error

## Evaluator (phase 2)
For the next phase of the evaluator, I have implemented local variables according to the instructions on canvas, by using environments and stores as needed for the types of variables (let or new). I also implemented functions, first-order and higher-order, which can be be applied to parameters, returned from other functions or stored in a variable and used in the left-hand and right-hand side of an assignment expression.

I extended the error messages to include FunctionError, so that if a function is not well defined or a program doesn't contain a main function, the error message will clearly indicate that and the user can fix their mistakes.

I have also added strings to the language and a NULL value to use with pointers. As for pointers, I have further worked on them and added dereferencing (with the help of the '*'symbol'), so that if we define a pointer like so "var *x = &y", it will return the value of y if we type in "\*x".

Recursive calls are possible, as well as simple tuples, which can be assigned, compared and assigned to function arguments as well as returned from functions. Lambda functions are also added into the syntax and can be applied to other lambdas and used, similarly to functions, in very complex assignments and in collaboration with let and if statements,

## Optimisations
I have implemented the following technqiues in order to optimise our compiler:

1. Constant propagation
	- it checks if a variable is set through user input and if so, it doesn't replace it
	- otherwise, for any known variable and constant, it tries to optimise the entire expression so that the evaluator has to evaluate as few operations as possible
2. Constant folding:
	- it evaluates any constants
3. Function inlining
	- it checks if the argument of the function includes a print statement and if so, it doesn't apply function inlining at all
	- otherwise, it replaces the arguments inside the function and attempts to optimise that funciton
4. Loop unrolling
	- for while loops as well as for loops (which have been added to my language over the past week)
	- a maximum unrolling number is set globally, so currently I unroll a loop 5 times before I give up on it

I have also added a very easy way of counting steps, but not with the help of a monad. If you run all the tests, you'll be able to see the number of steps and time required for both the optimised and non-optimised compilers.

If you run the program with the option "-o" (which isn't compulsory) it'll optimise before it will evaluate, otherwise it will call the evaluator straight away:

## Interpreter and compiler
For this part of the assignment, I have managed to implement both the interpreter and compiler for a simple iterative language, including local variables, let statement, as well as if statements and while and for loops. The compiler print labels to each jump location, so that the code is easy to read, and has a variety of instructions for each operator and others for moving values around.

Let statement have variables that are allocated just on the stack, whereas local variables (declared with "var") are located on the heap, but there is also a pointer on the stack to that location on the heap. My interpreter and compiler both accept pointers too, which as far as I can see are working fine. I have also added function applications to the interpreter, although lambdas aren't currently working, as well as the generator, but it's not entirely perfect yet.

As for the stack and used registers, I have put register 0 and 1 aside for reading and printing and have only allowed a stack of size 1000, so that values bigger than this value will be situated on the heap. I can relax this value if I notice it's not enough for functions that can be evaluated by the previously implemented evaluator.

If you want to interpret and generate one file at a time, run this command:

	sh run.sh <flag> <path>
	where:
		- flag: -i or -g
		- path: the path to a file inside the test folder

There are also some test cases inside folder "part5", with their generated code inside the "results" folder.

## Tests
There are fours folders now inside the "test" folder, which contain test cases for each part of the assignment.

Those test cases can be run using the Makefile provided in the main folder.

The first folder, "part1", contains 10 simple test cases, some of which can't be parsed due to some errors which are more or less easier to spot due to the error messages I have implemented. There are two more test cases, a bit more complicated.

The second folder, "part2", contains another 10 simple test cases and a harder one, which can be evaluated and compared to the expected result inside the "results" folder.

The third folder, "part3", contains 10 medium-difficulty test cases which can be evaluated and compared to the expected result inside the "results" folder.

The third folder, "part 4", contains 12 edge test cases, used to see if the optimiser does its best at optimising the functions, without changing the result and affecting the evaluator.

If you want to run your own tests, go back to the "Installation and Build" section to find out how to do this.