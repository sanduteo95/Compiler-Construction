## Description
I have chosen a JavaScript-like programming language, with pointers, function pointers, lambdas, for loops and control structures. The syntax has been extended to include a few extra operators (modulus, less, greater), as well as floating points, strings and tuples, although these last three features are not available in the compiler.

## Installation and Build
To extract the code from GIT:

	1. Open terminal and run:
		git clone https://github.com/sanduteo95/Compiler-Construction.git
	2. Type in your username and password if asked for them
	3. Then, go to the directory named "Compiler-Construction"

To run the program:

	1. Type in "make" to build the project
	2. The command will show you what to do next (test the parser, evaluator, interpreter and assembler)
	3. If you want to clean up the project, run "make clean"

If you want to run your own test, type in "./exp_test.native <flag> <path to file>", where:

	- <flag>:
		- "-p": print the non-optimised parse tree
		-  "-p -o": print the optimised parse tree
		- "-e": evaluate the non-optimised parse tree
		- "-e": evaluate the optimised parse tree
		- "-i": interpret the non-optimised parse tree
		- "-i": interpret the optimised parse tree
		- "-s": generate the non-optimised x86 code
		- "-s -o": generate the optimised x86 code
	- <path to file>: e.g. test/part4/test1.txt


## Syntax
The syntax allowed by the language is:

1. Seq of expression * expression

		e.g. expression;
			 expression;

2. If of expression * expression * expression

		e.g. if (boolean expression) {
				 expression;
			 }
			 else {
				 expression;
			 }

3. While of expression * expression

	e.g. while (boolean expression) {
			 expression;
		 }

3. For of string * expression * expression * expression

		e.g. for (variable=expression:expression) {
				 expression;
			 }

4. Asg of expression * expression

		e.g. x = 2;

5. Deref of expression

		e.g. *x

6. Negate of expression

		e.g. !expression

7. Operator of opcode * expression * expression

		e.g. expression _ expression (so far type checking isn't implemented)

8. Application of expression * expression (* e(e) *)

		e.g. Used when a function is applied to an expression, given that the function was already defined:
		function f {
			...
		}
		...
		x = f(expression);

9. Lambda of string list * expression

		e.g (\y -> return y+1;)

10. Readint

		e.g. x = read() (no type checking as of now)

11. Printint of expression

		e.g. print(expression)

12. Identifier of string

		e.g. x
		!!! Variable names can only consist of:
			- lower and upper case letters
			- _ (not accepted at the start of the variable)
			- digits (not accepted at the start of the variable)

13. Let of string * expression * expression

		e.g. let x = expression in expression;

14. New of string * expression * expression

		e.g. var x = expression;
			 expression;

15. Block of string * expression

		block_name {
			...
		}

16. BreakBlock of string * expression

		break block_name { expression }

17. ContinueBlock of string * expression

		continue block_name { expression }

18. Break

		break;

19. Continue

		continue;

20. Return statements

		e.g. return 1;

It is worth pointing that the language also contains multiline comments of the form:

	\* Comment *\

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
For now, the evaluator cannot evaluate any function applications and files containing more than one function definitions in them.

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
5. Sub-expression elimination
	- if the program uses the same sub-expression more than once, a constant variable is used to store that expression so that we only evaluate it onec
6. Memoisaiton
	- if a function is called with the same arguments more than once, it will get the result directly from a table that holds all the function calls' results

I have also added a very easy way of counting steps, but not with the help of a monad. If you run all the tests, you'll be able to see the number of steps and time required for both the optimised and non-optimised compilers.

If you run the program with the option "-o" (which isn't compulsory) it'll optimise before it will evaluate, otherwise it will call the evaluator straight away:

## Interpreter and compiler
For this part of the assignment, I have managed to implement both the interpreter and compiler for a simple iterative language, including local variables, let statement, as well as if statements and while and for loops. The compiler print labels to each jump location, so that the code is easy to read, and has a variety of instructions for each operator and others for moving values around.

Let statement have variables that are allocated just on the stack, whereas local variables (declared with "var") are located on the heap, but there is also a pointer on the stack to that location on the heap. My interpreter and compiler both accept pointers too, which as far as I can see are working fine. I have also added function applications to the interpreter, although lambdas aren't currently working, as well as the generator, but it's not entirely perfect yet.

As for the stack and used registers, I have put register 0 and 1 aside for reading and printing and have only allowed a stack of size 1000, so that values bigger than this value will be situated on the heap. I can relax this value if I notice it's not enough for functions that can be evaluated by the previously implemented evaluator.

## x86 compiler
For the x86 code generator I have implemented the following:

- integers, booleans and a NULL value for pointers
- immutable and mutable variables: let statements, assignments, dereferencing
- pointers to mutable variables
- pointers to functions
- if statements
- while and for loops
- functions that can take any number of arguments
- break and continue statements
- lambdas
- blocks, labelled breaks and labelled continues

## Tests
There are fours folders now inside the "test" folder, which contain test cases for each part of the assignment.

Those test cases can be run using the Makefile provided in the main folder.

Each test folder contains tests that where used to check that each added functionality behaves as expected. Some of the old tests do not work in the compiler, as I haven't implemented floating points, strings and tuples.

If you want to run your own tests, go back to the "Installation and Build" section to find out how to do this.
