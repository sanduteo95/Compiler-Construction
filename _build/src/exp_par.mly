%{
open Syntax
let length = List.length
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> TEXT
%token <bool> BOOL

%token NULL
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
%token FOR
%token TO
%token IF
%token ELSE
%token LET
%token IN
%token BREAK
%token CONTINUE

%token TYPE
%token ASSIGN
%token RETURN
%token FUNCTION
%token LAMBDA
%token FUN
%token ADDRESS_OF

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
	| EOF  { [] }
	| FUNCTION; f = separated_nonempty_list(FUNCTION, func); EOF  { f }

func:
	| id = ID; LEFT_ROUND_BRACKET; p = parameter_list; LEFT_CURLY_BRACKET; c = content  { (id, p, c) }

parameter_list:
	| RIGHT_ROUND_BRACKET { [] }
	| p = separated_nonempty_list(COMMA, parameter); RIGHT_ROUND_BRACKET  { p }

parameter:
	| TYPE; id = ID  { id }

content:
	| RIGHT_CURLY_BRACKET  { Nothing }
	| s = statements; RIGHT_CURLY_BRACKET  { s }

statements:
	| s1 = statement; s2 = statements  { Seq(s1, s2) }
	| s = statement  { s }
	| n = new_declaration  { n }

statement:
	| WHILE; LEFT_ROUND_BRACKET; o = operator_expression; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s = statements; RIGHT_CURLY_BRACKET  { While(o, s) }
	| IF; LEFT_ROUND_BRACKET; o = operator_expression; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s1 = statements; RIGHT_CURLY_BRACKET; ELSE; LEFT_CURLY_BRACKET; s2 = statements; RIGHT_CURLY_BRACKET  { If(o, s1, s2) }
	| f = for_loop  { f }
	| l = let_declaration  { l }
	| l = left_assignment; ASSIGN; r = right_assignment; SEMI_COLLON  { Asg(l, r)}
	| PRINT; LEFT_ROUND_BRACKET; e = expression; RIGHT_ROUND_BRACKET; SEMI_COLLON  { Print(e) }
	| RETURN; e = expression; SEMI_COLLON { e }
	| id = ID; SEMI_COLLON { Identifier(id) }
	| f = function_expression; LEFT_ROUND_BRACKET; a = separated_list(COMMA, expression); RIGHT_ROUND_BRACKET; SEMI_COLLON  { Application(f, a)}
	| BREAK; SEMI_COLLON  { Break }
	| CONTINUE; SEMI_COLLON  { Continue }
	
for_loop:
	| FOR; LEFT_ROUND_BRACKET id = ID; ASSIGN; TIMES; id1 = ID; TO; TIMES; id2 = ID; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s = statements; RIGHT_CURLY_BRACKET  { For(id, Deref(Identifier(id1)), Deref(Identifier(id2)), s) }
	| FOR; LEFT_ROUND_BRACKET; id = ID; ASSIGN; i = INT; TO; TIMES; id1 = ID; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s = statements; RIGHT_CURLY_BRACKET  { For(id, MyInteger(i), Deref(Identifier(id1)), s) }
	| FOR; LEFT_ROUND_BRACKET; id = ID; ASSIGN; TIMES; id1 = ID; TO; i = INT; RIGHT_ROUND_BRACKET LEFT_CURLY_BRACKET; s = statements; RIGHT_CURLY_BRACKET  { For(id, Deref(Identifier(id1)), MyInteger(i), s) }
	| FOR; LEFT_ROUND_BRACKET; id = ID; ASSIGN; i1 = INT; TO; i2 = INT; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s = statements; RIGHT_CURLY_BRACKET  { For(id, MyInteger(i1), MyInteger(i2), s) }

new_declaration:
	| TYPE; TIMES; id = ID; ASSIGN; r = right_assignment; SEMI_COLLON; s = statements  { New(id, r, s) }
	| TYPE; id = ID; ASSIGN; r = right_assignment; SEMI_COLLON; s = statements  { New(id, r, s) }

let_declaration:
	| LET; id = ID; ASSIGN; r = right_assignment; IN; s = statement { Let(id, r, s) }
	| LET; id = ID; ASSIGN; r = right_assignment; IN; me = min_expression; SEMI_COLLON  { Let(id, r, me) }

values:
	| v = value  { v }
	| LEFT_ROUND_BRACKET; vs = separated_nonempty_list(COMMA, value); RIGHT_ROUND_BRACKET  { MyTuple(vs) }

value:
	| i = INT  { MyInteger(i) }
	| f = FLOAT  { MyFloat(f) }
	| b = BOOL  { MyBoolean(b) }
	| t = TEXT  { MyString(t) }

min_expression:
	| vs = values  { vs }
	| ADDRESS_OF; id = ID { Identifier(id) }
	| TIMES; TIMES; id = ID { Deref(Deref(Identifier(id))) }
	| LEFT_ROUND_BRACKET; TIMES; TIMES; id = ID; RIGHT_ROUND_BRACKET { Deref(Deref(Identifier(id))) }
	| LEFT_ROUND_BRACKET; ts = separated_nonempty_list(COMMA, ids); RIGHT_ROUND_BRACKET  { MyTuple(ts) }
	| o = operator_expression  { o }

expression:
	| id = ID  { Identifier(id) }
	| TIMES; id = ID  { Deref(Identifier(id)) }
	| LEFT_ROUND_BRACKET; TIMES; id = ID; RIGHT_ROUND_BRACKET { Deref(Identifier(id)) }
	| me = min_expression  { me }
	| f = function_expression; LEFT_ROUND_BRACKET; a = separated_list(COMMA, expression); RIGHT_ROUND_BRACKET  { Application(f, a)}

left_assignment:
	| id = ID  { Identifier(id) }
	| ADDRESS_OF; id = ID { Identifier(id) }
	| TIMES; id = ID { Deref(Identifier(id)) }
	| LEFT_ROUND_BRACKET; TIMES; TIMES; id = ID; RIGHT_ROUND_BRACKET { Deref(Identifier(id)) }
	| LEFT_ROUND_BRACKET; IF; LEFT_ROUND_BRACKET; o = operator_expression; RIGHT_ROUND_BRACKET; LEFT_CURLY_BRACKET; s1 = statements; RIGHT_CURLY_BRACKET; ELSE; LEFT_CURLY_BRACKET; s2 = statements; RIGHT_CURLY_BRACKET; RIGHT_ROUND_BRACKET  { If(o, s1, s2) }
	| LEFT_ROUND_BRACKET; LET; id = ID; ASSIGN; r = right_assignment; IN; s = statement; RIGHT_ROUND_BRACKET  { Let(id, r, s) }
	| LEFT_ROUND_BRACKET; f = function_expression; LEFT_ROUND_BRACKET; a = separated_list(COMMA, expression); RIGHT_ROUND_BRACKET ; RIGHT_ROUND_BRACKET  { Application(f, a) }
	| TIMES; LEFT_ROUND_BRACKET; f = function_expression; LEFT_ROUND_BRACKET; a = separated_list(COMMA, expression); RIGHT_ROUND_BRACKET ; RIGHT_ROUND_BRACKET  { Deref(Application(f, a)) }

right_assignment:
	| NULL  { MyNull }
	| e = expression { e }
	| LEFT_ROUND_BRACKET; s = statement; RIGHT_ROUND_BRACKET  { s }
	| READ; LEFT_ROUND_BRACKET; RIGHT_ROUND_BRACKET  { Read }

operator_expression:
	| LEFT_ROUND_BRACKET; o = operator_expression; RIGHT_ROUND_BRACKET  { o }
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

function_expression:
	| id = ID  { Identifier(id) }
	| LEFT_ROUND_BRACKET; FUN; ids = separated_list(COMMA, ID); LAMBDA; s = statement; RIGHT_ROUND_BRACKET { Lambda(ids, s) }

ids:
	| id = ID  { Deref(Identifier(id)) }