%{
open Syntax
%}

%token <int> INT 
%token <string> ID
%token <string> TEXT

%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MODULUS
%token LESS
%token LEQ
%token GREATER
%token GEQ
%token EQ
%token NOTEQ
%token AND
%token OR
%token NEGATE

%token READ
%token PRINT

%token WHILE
%token IF
%token ELSE
%token LET
%token IN

%token TYPE
%token ASSIGN
%token RETURN
%token FUNCTION

%token COMMA
%token SEMI_COLLON
%token LEFT_ROUND_BRACKET
%token RIGHT_ROUND_BRACKET
%token LEFT_CURLY_BRACKET
%token RIGHT_CURLY_BRACKET
%token EOF 

%left AND OR
%left EQ NOTEQ
%left LEQ GEQ LESS GREATER
%left PLUS MINUS      
%left TIMES DIVIDE MODULUS      
%right NEGATE

%start <Syntax.program> parse 
%%
parse :
	| EOF  { [("", [], Nothing)] }
	| FUNCTION; f = separated_nonempty_list(FUNCTION, func); EOF  { f }

func: 
	| id = ID; LEFT_ROUND_BRACKET; p = parameter_list; LEFT_CURLY_BRACKET; c = content  { (id, p, c) }

parameter_list:
	| RIGHT_ROUND_BRACKET { [] }
	| p = separated_nonempty_list(COMMA, parameter); RIGHT_ROUND_BRACKET  { p }

parameter: 
	| TYPE; id = ID  { Param(id) }

content: 
	| RIGHT_CURLY_BRACKET  { Nothing }
	| s = statements; RIGHT_CURLY_BRACKET  { s }  

statements:
	| s1 = statement; SEMI_COLLON; s2 = statements  { Seq(s1, s2) }
	| s = statement; SEMI_COLLON { s }
	| TYPE; id = ID; ASSIGN; a = assignment; SEMI_COLLON; s = statements  { New(id, a, s) }

statement:
	| WHILE; LEFT_ROUND_BRACKET; o = operator_expression; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s = statements; RIGHT_CURLY_BRACKET  { While(o, s) }
	| IF; LEFT_ROUND_BRACKET; o = operator_expression; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s1 = statements; RIGHT_CURLY_BRACKET; ELSE; LEFT_CURLY_BRACKET; s2 = statements; RIGHT_CURLY_BRACKET  { If(o, s1, s2) }
	| id = ID; ASSIGN; a = assignment  { Asg(Identifier(id), a)}
	| PRINT; LEFT_ROUND_BRACKET; p = print_value; RIGHT_ROUND_BRACKET  { Printint(p) }
	| LET; id = ID; ASSIGN; a = assignment; IN; e = expression  { Let(id, a, e) }
	| RETURN; v = expression { v }

expression: 
	| LEFT_ROUND_BRACKET; e = expression; RIGHT_ROUND_BRACKET  { e }
	| i = INT  { Const(i) } 
	| id = ID  { Deref(Identifier(id)) }
	| o = operator_expression  { o }
	| f = function_expression; LEFT_ROUND_BRACKET; a = separated_list(COMMA, argument); RIGHT_ROUND_BRACKET  { Application(f, a)}

assignment:  
	| e = expression  { e }
	| READ; LEFT_ROUND_BRACKET; RIGHT_ROUND_BRACKET  { Readint }

function_expression: 
	| id = ID  { Identifier(id) }

print_value:
	| v = expression  { v }
	| text = TEXT  { Text(text) }

operator_expression:
	| e1 = expression; PLUS;  e2 = expression  { Operator(Plus, e1, e2) }  
	| e1 = expression; MINUS; e2 = expression  { Operator(Minus, e1, e2) }
	| e1 = expression; TIMES; e2 = expression  { Operator(Times, e1, e2) }
	| e1 = expression; DIVIDE; e2 = expression  { Operator(Divide, e1, e2) }
	| e1 = expression; MODULUS; e2 = expression  { Operator(Modulus, e1, e2)}
	| e1 = expression; LEQ; e2 = expression  { Operator(Leq, e1, e2)}
	| e1 = expression; LESS; e2 = expression  { Operator(Less, e1, e2)}
	| e1 = expression; GEQ; e2 = expression  { Operator(Geq, e1, e2) }
	| e1 = expression; GREATER; e2 = expression  { Operator(Greater, e1, e2)}
	| e1 = expression; EQ; e2 = expression  { Operator(Eq, e1, e2) }
	| e1 = expression; NOTEQ; e2 = expression  { Operator(Noteq, e1, e2)}
	| e1 = expression; AND; e2 = expression  { Operator(And, e1, e2) }
	| e1 = expression; OR; e2 = expression  { Operator(Or, e1, e2) }
	| NEGATE; e = expression  { Negate(e) }

argument:
	| id = ID  { Deref(Identifier(id)) }
	| i = INT  { Const(i) } 
	| o = operator_expression  { o }