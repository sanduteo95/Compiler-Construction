%{
open Syntax
%}

%token <int> INT 
%token <string> ID

%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token LEQ
%token GEQ
%token EQ
%token NOTEQ
%token AND
%token OR
%token NOT

%token READ
%token PRINT

%token WHILE
%token IF
%token ELSE
%token LET
%token IN

%token TYPE
%token ASSIGN
%token DEREF

%token COMMA
%token SEMI_COLLON
%token LEFT_ROUND_BRACKET
%token RIGHT_ROUND_BRACKET
%token LEFT_CURLY_BRACKET
%token RIGHT_CURLY_BRACKET
%token EOF 

%left AND OR
%left EQ NOTEQ
%left LEQ GEQ
%left PLUS MINUS      
%left TIMES DIVIDE      
%right NOT

%start <Syntax.fundef> top 
%%
top :
	| EOF { ("", [], Nothing) }
	| id = ID; LEFT_ROUND_BRACKET; params = param_list; LEFT_CURLY_BRACKET; content = function_content; EOF { (id, params, content) }

param_list:
	| RIGHT_ROUND_BRACKET { [] }
	| p = separated_nonempty_list(COMMA, param); RIGHT_ROUND_BRACKET  { p }

param: 
	| TYPE; id = ID  { Param(id) }

function_content: 
	| RIGHT_CURLY_BRACKET  { Nothing }
	| s = stmt; RIGHT_CURLY_BRACKET  { s }  

stmt:
	| e = exp  { e }
	| s1 = stmt; SEMI_COLLON; s2 = stmt  { Seq(s1, s2) }
	| WHILE; LEFT_ROUND_BRACKET; e = exp; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s = stmt; RIGHT_CURLY_BRACKET  { While(e, s) }
	| IF; LEFT_ROUND_BRACKET; e = exp; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s1 = stmt; RIGHT_CURLY_BRACKET; ELSE; LEFT_CURLY_BRACKET; s2 = stmt; RIGHT_CURLY_BRACKET  { If(e, s1, s2) }
	| e1 = exp; ASSIGN; e2 = exp  { Asg(e1, e2)}
	| READ; LEFT_ROUND_BRACKET; i = INT; RIGHT_ROUND_BRACKET  { Readint }
	| PRINT; LEFT_ROUND_BRACKET; e = exp; RIGHT_ROUND_BRACKET  { Printint(e) }
	| LET; id = ID; ASSIGN; e = exp; IN; s = stmt; SEMI_COLLON  { Let(id, e, s) }
	| TYPE; id = ID; ASSIGN; e = exp; SEMI_COLLON; s = stmt  { New(id, e, s) }

exp:  
	| LEFT_ROUND_BRACKET; e = exp; RIGHT_ROUND_BRACKET; DEREF  { Deref(e) }
	| e1 = exp; PLUS;  e2 = exp  { Operator(Plus, e1, e2) }  
	| e1 = exp; MINUS; e2 = exp  { Operator(Minus, e1, e2) }
	| e1 = exp; TIMES; e2 = exp  { Operator(Times, e1, e2) }
	| e1 = exp; DIVIDE; e2 = exp  { Operator(Divide, e1, e2) }
	| e1 = exp; LEQ; e2 = exp  { Operator(Leq, e1, e2)}
	| e1 = exp; GEQ; e2 = exp  { Operator(Geq, e1, e2) }
	| e1 = exp; EQ; e2 = exp  { Operator(Eq, e1, e2) }
	| e1 = exp; NOTEQ; e2 = exp  { Operator(Noteq, e1, e2)}
	| e1 = exp; AND; e2 = exp  {Operator(And, e1, e2) }
	| e1 = exp; OR; e2 = exp  { Operator(Or, e1, e2) }
	| e1 = exp; NOT; e2 = exp  {Operator(Not, e1, e2) }
	| e1 = exp; LEFT_ROUND_BRACKET; e2 = exp; RIGHT_ROUND_BRACKET  { Application(e1, e2)} 
	| i = INT  { Const(i) }  	
	| id = ID  { Identifier(id) }
