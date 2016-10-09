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

%start <Syntax.program> top 
%%
top :
	| EOF  { [("", [], Nothing)] }
	| FUNCTION; funcs = separated_nonempty_list(FUNCTION, func); EOF  { funcs }

func: 
	| id = ID; LEFT_ROUND_BRACKET; params = param_list; LEFT_CURLY_BRACKET; content = function_content  { (id, params, content) }

param_list:
	| RIGHT_ROUND_BRACKET { [] }
	| p = separated_nonempty_list(COMMA, param); RIGHT_ROUND_BRACKET  { p }

param: 
	| TYPE; id = ID  { Param(id) }

function_content: 
	| RIGHT_CURLY_BRACKET  { Nothing }
	| s = stmt; RIGHT_CURLY_BRACKET  { s }  

stmt:
	| s1 = stmt; s2 = stmt  { Seq(s1, s2) }
	| WHILE; LEFT_ROUND_BRACKET; e = exp; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s = stmt; RIGHT_CURLY_BRACKET  { While(e, s) }
	| IF; LEFT_ROUND_BRACKET; e = exp; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s1 = stmt; RIGHT_CURLY_BRACKET; ELSE; LEFT_CURLY_BRACKET; s2 = stmt; RIGHT_CURLY_BRACKET  { If(e, s1, s2) }
	| id = ID; ASSIGN; e = exp; SEMI_COLLON  { Asg(Identifier(id), e)}
	| PRINT; LEFT_ROUND_BRACKET; p = print_value; RIGHT_ROUND_BRACKET; SEMI_COLLON  { Printint(p) }
	| LET; id = ID; ASSIGN; e = exp; IN; s = stmt; SEMI_COLLON  { Let(id, e, s) }
	| TYPE; id = ID; ASSIGN; e = exp; SEMI_COLLON; s = stmt  { New(id, e, s) }
	| RETURN; e = exp; SEMI_COLLON { e }

exp:  
	| LEFT_ROUND_BRACKET; e = exp; RIGHT_ROUND_BRACKET  { e }
	| e1 = exp; PLUS;  e2 = exp  { Operator(Plus, e1, e2) }  
	| e1 = exp; MINUS; e2 = exp  { Operator(Minus, e1, e2) }
	| e1 = exp; TIMES; e2 = exp  { Operator(Times, e1, e2) }
	| e1 = exp; DIVIDE; e2 = exp  { Operator(Divide, e1, e2) }
	| e1 = exp; MODULUS; e2 = exp  { Operator(Modulus, e1, e2)}
	| e1 = exp; LEQ; e2 = exp  { Operator(Leq, e1, e2)}
	| e1 = exp; LESS; e2 = exp  { Operator(Less, e1, e2)}
	| e1 = exp; GEQ; e2 = exp  { Operator(Geq, e1, e2) }
	| e1 = exp; GREATER; e2 = exp  { Operator(Greater, e1, e2)}
	| e1 = exp; EQ; e2 = exp  { Operator(Eq, e1, e2) }
	| e1 = exp; NOTEQ; e2 = exp  { Operator(Noteq, e1, e2)}
	| e1 = exp; AND; e2 = exp  { Operator(And, e1, e2) }
	| e1 = exp; OR; e2 = exp  { Operator(Or, e1, e2) }
	| NEGATE; e = exp  { Negate(e) }
	| id = ID; LEFT_ROUND_BRACKET; a = separated_list(COMMA, argument); RIGHT_ROUND_BRACKET  { Application(id, a)} 
	| i = INT  { Const(i) }  	
	| id = ID  { Deref(Identifier(id)) }
	| READ; LEFT_ROUND_BRACKET; RIGHT_ROUND_BRACKET  { Readint }

argument:
	| id = ID  { id }

print_value:
	| i = INT  { Const(i) } 
	| id = ID  { Deref(Identifier(id)) }
	| text = TEXT  { Text(text) }