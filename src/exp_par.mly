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

%right ASSIGN
%right DEREF
%left AND OR
%left EQ NOTEQ
%left LEQ GEQ
%left PLUS MINUS      
%left TIMES DIVIDE      
%right NOT
%right WHILE IF
%right READ PRINT TYPE
%left SEMI_COLLON
%right LEFT_ROUND_BRACKET 

%start <Syntax.fundef> top 
%%
top :
	| EOF { ("", [], Nothing) }
	| f = func; EOF  { f }   

func:
	| id = ID; LEFT_ROUND_BRACKET; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; RIGHT_CURLY_BRACKET { (id, [None], Nothing) }
	| id = ID; LEFT_ROUND_BRACKET; p = separated_nonempty_list(COMMA, param); RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; RIGHT_CURLY_BRACKET { (id, p, Nothing) }
	| id = ID; LEFT_ROUND_BRACKET; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; e = exp; RIGHT_CURLY_BRACKET { (id, [None], e) }
	| id = ID; LEFT_ROUND_BRACKET; p = separated_nonempty_list(COMMA, param); RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; e = exp; RIGHT_CURLY_BRACKET { (id, p, e) }

param: 
	| TYPE; id = ID  { Param(id) }

exp:  
	| e = exp; SEMI_COLLON; f = exp; SEMI_COLLON  { Seq(e, f) }
	| WHILE; LEFT_ROUND_BRACKET; e = exp; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; f = exp; RIGHT_CURLY_BRACKET  { While(e, f) }
	| IF; LEFT_ROUND_BRACKET; e = exp; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; f = exp; RIGHT_CURLY_BRACKET; ELSE; LEFT_CURLY_BRACKET; g = exp; RIGHT_CURLY_BRACKET  { If(e, f, g) }
	| e = exp; ASSIGN; f = exp  { Asg(e, f)}
	| DEREF; e = exp  { Deref(e) }
	| e = exp; PLUS;  f = exp  { Operator(Plus, e, f) }  
	| e = exp; MINUS; f = exp  { Operator(Minus, e, f) }
	| e = exp; TIMES; f = exp  { Operator(Times, e, f) }
	| e = exp; DIVIDE; f = exp  { Operator(Divide, e, f) }
	| e = exp; LEQ; f = exp  { Operator(Leq, e, f)}
	| e = exp; GEQ; f = exp  { Operator(Geq, e, f) }
	| e = exp; EQ; f = exp  { Operator(Eq, e, f) }
	| e = exp; NOTEQ; f = exp  { Operator(Noteq, e, f)}
	| e = exp; AND; f = exp  {Operator(And, e, f) }
	| e = exp; OR; f = exp  { Operator(Or, e, f) }
	| NOT; e = exp; f = exp  {Operator(Not, e, f) }
	| e = exp; LEFT_ROUND_BRACKET; f = exp; RIGHT_ROUND_BRACKET  { Application(e, f)} 
	| i = INT  { Const(i) }  
	| READ; LEFT_ROUND_BRACKET; i = INT; RIGHT_ROUND_BRACKET  { Readint }
	| PRINT; LEFT_ROUND_BRACKET; e = exp; RIGHT_ROUND_BRACKET  { Printint(e) }
	| id = ID  { Identifier(id) }
	| TYPE; id = ID; ASSIGN; e = exp; LEFT_CURLY_BRACKET; f = exp; SEMI_COLLON; RIGHT_CURLY_BRACKET  { Let(id, e, f) }
	| TYPE; id = ID; ASSIGN; e = exp; SEMI_COLLON; f = exp  { New(id, e, f) }