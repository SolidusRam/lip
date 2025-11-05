%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token SKIP
%token SEQ
%token ASSIGN
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token INT
%token BOOL
%token <int> CONST
%token <string> ID
%token EOF

%left SEQ
%nonassoc ELSE
%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left ADD SUB
%left MUL

%start <cmd> prog

%%

prog:
  | c = cmd; EOF { c }
  ;

cmd:
  | SKIP { Skip }
  | x = ID; ASSIGN; e = expr { Assign(x, e) }
  | c1 = cmd; SEQ; c2 = cmd { Seq(c1, c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd { If(e, c1, c2) }
  | WHILE; e = expr; DO; c = cmd { While(e, c) }
  | LPAREN; c = cmd; RPAREN { c }
  | LBRACE; ds = decl_list; c = cmd; RBRACE { Decl(ds, c) }
  | LBRACE; c = cmd; RBRACE { Decl([], c) }
  ;

decl_list:
  | d = decl { [d] }
  | d = decl; ds = decl_list { d :: ds }
  ;

decl:
  | INT; x = ID; SEQ { IntVar(x) }
  | BOOL; x = ID; SEQ { BoolVar(x) }
  ;

expr:
  | TRUE { True }
  | FALSE { False }
  | n = CONST { Const(n) }
  | x = ID { Var(x) }
  | NOT; e = expr { Not(e) }
  | e1 = expr; AND; e2 = expr { And(e1, e2) }
  | e1 = expr; OR; e2 = expr { Or(e1, e2) }
  | e1 = expr; ADD; e2 = expr { Add(e1, e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1, e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1, e2) }
  | e1 = expr; EQ; e2 = expr { Eq(e1, e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1, e2) }
  | LPAREN; e = expr; RPAREN { e }
  ;