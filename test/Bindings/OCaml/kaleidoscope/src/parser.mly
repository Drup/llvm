%{ open Ast %}


%token Eof
%token Var Def Extern
%token <string> Ident
%token <float> Number
%token If Then Else For In
%token Equal
%token EqualEqual Le Ge
%token Plus Minus Time Div
%token Bang
%token LParen RParen Comma Semi

%nonassoc In
%nonassoc Else
%nonassoc Equal
%left EqualEqual Le Ge
%left Plus Minus
%left Time Div

%nonassoc UMinus
%nonassoc Bang

%start main
%type <Ast.statement list> main

%start main_statement
%type <Ast.statement option> main_statement

%%

main:
  statements = list(statement) Eof { statements }

main_statement:
 | statement = statement { Some statement }
 | Eof { None }


statement:

 (** 'extern' name '(' arg ',' ... ')' ';' *)
 | Extern proto=prototype Semi { Extern proto }

 (** 'def' name '(' arg ',' ... ')' body ';' *)
 | Def proto=prototype Equal body=expr Semi { Def {proto ; body} }

 (** expr *)
 | expr=expr Semi { Expr expr }


(** name '(' arg1 ',' .... ')' *)
prototype:
  name=Ident LParen arguments=separated_list(Comma, Ident) RParen
   { {name ; arguments = Array.of_list arguments} }

expr:
 | n=Number { Number n }

 (** expr op expr *)
 | e1=expr op=binop e2=expr {Binary (op, e1, e2)}

 (** op expr *)
 | op=unop e=expr            { Unary(op, e) }
 (** Little bit of parser magic to overwrite the priority for unary minus. *)
 | Minus e=expr %prec UMinus { Unary("-",e) }

 (** 'if' expr 'then' expr 'else' expr *)
 | If b=expr Then e1=expr Else e2=expr { If (b,e1,e2) }

 (** 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression *)
 | For i=Ident Equal
   start=expr Comma cond=expr step=preceded(Comma,expr)?
   In body=expr
   { For (i, start, cond, step, body) }

 (** 'var' var_dec (',' var_dec)* 'in' expression *)
 | Var l=separated_nonempty_list(Comma, var_dec) In body=expr
   { Var (Array.of_list l, body) }

 (** '(' expr ')' *)
 | LParen e=expr RParen { e }

 (** expr '(' ( expr (, expr)* )? ')' *)
 | f=Ident LParen args=separated_list(Comma, expr) RParen
   { Call (f, Array.of_list args) }

 | var=Ident { Variable var }

 (** identifier '=' expr *)
 | var=Ident Equal e=expr { Assign (var, e) }

(** identifier ('=' expression)? *)
var_dec:
   id=Ident Equal value=expr {(id,value)}


%inline unop:
 | Bang { "!" }

%inline binop:
 | EqualEqual { "==" }
 | Le    { "<" }
 | Ge    { ">" }
 | Plus  { "+" }
 | Minus { "-" }
 | Time  { "*" }
 | Div   { "/" }
