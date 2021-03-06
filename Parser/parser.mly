%{
  open Ast
%}

%token DEFINE DEFINEREC LAMBDA LET LETREC IF BEGIN SET
%token LPAREN RPAREN LBRACK RBRACK
%token DELAY FORCE CALLCC EVAL
%token DOT QUOTE
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token PLUS MINUS MUL DIV MOD
%token LIST CONS CAR CDR NIL NULL
%token TRUE FALSE
%token LOAD
%token <string> ID
%token <int> INT
%token <string> STRING
%token EOF

%type <Ast.expr list> main
%type <Ast.expr list> seq
%type <Ast.expr> expr

%start main

%%
main:
   seq EOF           { $1 }
 | EOF               { [] }
;

seq  :
   expr seq   { $1 :: $2 }
 | expr        { [$1] }
;

idseq:
   ID                { [$1] }
 | LPAREN ids RPAREN { $2 }
;

ids  :
   ID ids { $1 :: $2 }
 | ID     { [$1] }
;

expr :
   INT       { Int_e $1 }
 | ID        { Id_e $1 }
 | STRING    { Str_e $1 }
 | TRUE      { Bool_e true }
 | FALSE     { Bool_e false }
 | QUOTE expr { Quote_e $2 }
 | LPAREN BEGIN seq RPAREN { Begin_e $3 }
 | LPAREN QUOTE expr RPAREN           { Quote_e $3 }
 | LPAREN LAMBDA LPAREN ids RPAREN expr RPAREN    { Fun_e ($4, $6) }
 | LPAREN EVAL expr RPAREN            { Eval_e $3 }
 | LPAREN DEFINE idseq expr RPAREN    { Def_e ($3, $4) }
 | LPAREN DEFINEREC idseq expr RPAREN { Defrec_e ($3, $4) }
 | LPAREN LET ID expr expr RPAREN
     { Let_e ($3, $4, $5) }
 | LPAREN SET ID expr RPAREN          { Set_e (Id_e $3, $4) }
 | LPAREN LETREC ID expr expr RPAREN
     { Letrec_e ($3, $4, $5) }
 | LPAREN IF expr expr expr RPAREN    { If_e ($3, $4, $5) }
 | LPAREN expr seq RPAREN             { Apply_e ($2, $3) }
 | LPAREN LIST seq RPAREN
     { List.fold_right (fun x a -> Cons_e (x, a)) $3 Nil_e }
 | LPAREN LIST RPAREN                 { Nil_e }
 | LPAREN RPAREN                      { Nil_e }
 | NIL                                { Nil_e }
 | LPAREN CONS expr expr RPAREN       { Cons_e ($3, $4) }
;
