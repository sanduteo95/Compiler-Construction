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
	| s = statement; RIGHT_CURLY_BRACKET  { s }  

statement:
	| s1 = statement; s2 = statement  { Seq(s1, s2) }
	| WHILE; LEFT_ROUND_BRACKET; v = value_expresion; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s = statement; RIGHT_CURLY_BRACKET  { While(v, s) }
	| IF; LEFT_ROUND_BRACKET; v = value_expresion; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s1 = statement; RIGHT_CURLY_BRACKET; ELSE; LEFT_CURLY_BRACKET; s2 = statement; RIGHT_CURLY_BRACKET  { If(v, s1, s2) }
	| id = ID; ASSIGN; e = expression; SEMI_COLLON  { Asg(Identifier(id), e)}
	| PRINT; LEFT_ROUND_BRACKET; p = print_value; RIGHT_ROUND_BRACKET; SEMI_COLLON  { Printint(p) }
	| LET; id = ID; ASSIGN; v = value_expresion; IN; s = expression; SEMI_COLLON  { Let(id, v, s) }
	| TYPE; id = ID; ASSIGN; e = expression; SEMI_COLLON; s = statement  { New(id, e, s) }
	| RETURN; v = value_expresion; SEMI_COLLON { Deref(v) }

value_expresion: 
	| LEFT_ROUND_BRACKET; v = value_expresion; RIGHT_ROUND_BRACKET  { v }
	| i = INT  { Const(i) } 
	| id = ID  { Deref(Identifier(id)) }
	| o = operator_expression  { o }
	| f = function_expression; LEFT_ROUND_BRACKET; a = separated_list(COMMA, argument); RIGHT_ROUND_BRACKET  { Application(f, a)}

expression:  
	| v = value_expresion  { v }
	| READ; LEFT_ROUND_BRACKET; RIGHT_ROUND_BRACKET  { Readint }

function_expression: 
	| id = ID  { Identifier(id) }

print_value:
	| v = value_expresion  { v }
	| text = TEXT  { Text(text) }

operator_expression:
	| v1 = value_expresion; PLUS;  v2 = value_expresion  { Operator(Plus, v1, v2) }  
	| v1 = value_expresion; MINUS; v2 = value_expresion  { Operator(Minus, v1, v2) }
	| v1 = value_expresion; TIMES; v2 = value_expresion  { Operator(Times, v1, v2) }
	| v1 = value_expresion; DIVIDE; v2 = value_expresion  { Operator(Divide, v1, v2) }
	| v1 = value_expresion; MODULUS; v2 = value_expresion  { Operator(Modulus, v1, v2)}
	| v1 = value_expresion; LEQ; v2 = value_expresion  { Operator(Leq, v1, v2)}
	| v1 = value_expresion; LESS; v2 = value_expresion  { Operator(Less, v1, v2)}
	| v1 = value_expresion; GEQ; v2 = value_expresion  { Operator(Geq, v1, v2) }
	| v1 = value_expresion; GREATER; v2 = value_expresion  { Operator(Greater, v1, v2)}
	| v1 = value_expresion; EQ; v2 = value_expresion  { Operator(Eq, v1, v2) }
	| v1 = value_expresion; NOTEQ; v2 = value_expresion  { Operator(Noteq, v1, v2)}
	| v1 = value_expresion; AND; v2 = value_expresion  { Operator(And, v1, v2) }
	| v1 = value_expresion; OR; v2 = value_expresion  { Operator(Or, v1, v2) }
	| NEGATE; v = value_expresion  { Negate(v) }

argument:
	| id = ID  { Deref(Identifier(id)) }
	| i = INT  { Const(i) } 
	| o = operator_expression  { o }