%{
  open Ast
%}
%token <int> NUMBER
%token <string> ID
%token IF ELSE TRUE FALSE SEMICOLON COLON DEF REF STAR WHILE RETURN FUNDEF
%token PLUS MINUS LESSTHAN GREATERTHAN EQ AND OR COMMA
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS LEFT_BRACE RIGHT_BRACE
%token EOF
%left PLUS MINUS 
%left EQ 
%left LESSTHAN GREATERTHAN AND OR 

%type <Ast.def> fundef
%type <Ast.expr> expr
%type <Ast.expr list> expr_list
%type <string list> param_list
%type <Ast.stmt> stmt
%type <Ast.def list> fundef*
%type <Ast.stmt list> stmt*

%start <Ast.prog> parse
%%

parse: 
  | dl=fundef* sl=stmt* EOF { Program (dl, sl) }
  ;
fundef:
  | FUNDEF f=ID LEFT_PARENTHESIS RIGHT_PARENTHESIS LEFT_BRACE sl=stmt* RIGHT_BRACE { FunDef (f, [], sl) }
  | FUNDEF f=ID LEFT_PARENTHESIS pl=param_list RIGHT_PARENTHESIS LEFT_BRACE sl=stmt* RIGHT_BRACE { FunDef (f, pl, sl) }
  ;
param_list:
  | p=ID { [p] }
  | p=ID COMMA pl=param_list { p :: pl }
  ;
stmt:
  | WHILE e=expr LEFT_BRACE sl=stmt* RIGHT_BRACE { LoopStmt (e, sl) }
  | DEF x=ID EQ e=expr SEMICOLON { DefStmt (x, e) }
  | x=ID EQ e=expr SEMICOLON { StoreStmt (Ref x, e) }
  | x=ID EQ STAR e=expr SEMICOLON { LoadStmt (x, e) }
  | STAR e1=expr EQ e2=expr SEMICOLON { StoreStmt (e1, e2) }
  | IF e=expr LEFT_BRACE sl=stmt* RIGHT_BRACE { IfStmt (e, sl, []) }
  | IF e=expr LEFT_BRACE sl1=stmt* RIGHT_BRACE ELSE LEFT_BRACE sl2=stmt* RIGHT_BRACE { IfStmt (e, sl1, sl2) }
  | RETURN e=expr SEMICOLON { ReturnStmt e }
  | x=ID EQ f=ID LEFT_PARENTHESIS RIGHT_PARENTHESIS SEMICOLON { CallStmt (x, f, []) }
  | x=ID EQ f=ID LEFT_PARENTHESIS el=expr_list RIGHT_PARENTHESIS SEMICOLON { CallStmt (x, f, el) }
  ;
expr_list:
  | e=expr { [e] }
  | e=expr COMMA el=expr_list { e :: el }
  ;
expr:
  | n=NUMBER { Num n }
  | x=ID { Id x }
  | REF x=ID { Ref x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LEFT_PARENTHESIS e=expr RIGHT_PARENTHESIS { e }
  | e1=expr PLUS e2=expr { Add (e1, e2) }
  | e1=expr MINUS e2=expr { Sub (e1, e2) }
  | e1=expr LESSTHAN e2=expr { Lt (e1, e2) }
  | e1=expr GREATERTHAN e2=expr { Gt (e1, e2) }
  | e1=expr EQ EQ e2=expr { Eq (e1, e2) }
  | e1=expr AND e2=expr { And (e1, e2) }
  | e1=expr OR e2=expr { Or (e1, e2) }
  ;
