%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT CONJ DISJ
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ REC
%token RARROW FUN

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
Expr SEMISEMI { Exp $1 }
| topLet SEMISEMI { Decl $1 }
| LET REC ID EQ FUN ID RARROW Expr SEMISEMI { RecDecl ($3,$6, $8) }

topLet:
LET ID EQ Expr { ($2, $4)::[] }
| LET ID EQ Expr topLet { ($2, $4)::$5 }

Expr :
IfExpr { $1 }
| LetExpr { $1 }
| DISExpr { $1 }
| FunExpr { $1 }
| LetRecExpr { $1 }

LetExpr:
LET ID EQ Expr IN Expr { LetExp ($2, $4, $6) }

LetRecExpr:
LET REC ID EQ FUN ID RARROW Expr IN Expr {LetRecExp ($3,$6,$8,$10)}


FunExpr:
FUN ID RARROW Expr { FunExp($2, $4) }

DISExpr :
CONExpr DISJ CONExpr {BinOp(Disj,$1,$3)}
| CONExpr {$1}
    
CONExpr :
LTExpr CONJ LTExpr {BinOp(Conj,$1,$3)}
| LTExpr {$1}

LTExpr : 
    PExpr LT PExpr { BinOp (Lt, $1, $3) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | MExpr { $1 }

MExpr : 
    MExpr MULT AppExpr { BinOp (Mult, $1, $3) }
  | AppExpr { $1 }

AppExpr:
AppExpr AExpr { AppExp ($1, $2) }
  | AExpr { $1 }

AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | LPAREN Expr RPAREN { $2 }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

   
