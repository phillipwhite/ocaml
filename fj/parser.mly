%{
open Syntax
let createClassDec cname supName triple =
  match triple with
      (fdec, con, meth) -> {cname=cname; super_name=supName;
			    fdec=fdec; con=con; mdecs=meth}

let rec take ls n =
  if n <= 0 || ls = [] then []
  else List.hd ls :: take (List.tl ls) (n - 1)

let rec drop ls n =
  if n <= 0 || ls = [] then ls
  else drop (List.tl ls) (n-1)

let rec append l1 l2 =
match l1 with
    [] -> l2
  | x :: rest -> x :: append rest l2

let rec reverse = function
[] ->[]
  | x::rest -> append (reverse rest) [x];;

let createConstructorDec para body =
  let sl = List.length body.sup in
  {sfields=take para sl; dfields=drop para sl; body=body}


let createMethodDec mname arg retty mbody = 
  {mname=mname; para=arg; retty=retty; mbody=mbody}

(*

引数リストとか逆になってるくさい

->引数の型チェックの時などに変になるのでNG
少なくともそこはreverseすべし
*)
%}

%token LPAREN RPAREN LCURL RCURL SEMI DOT COLON
%token CLASS SUPER RET NEW EXTENDS THIS EQ 

%token <string> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
ClassDec{ CDec $1 }
| Term SEMI{ Exp $1 }
    
ClassDec :
CLASS ID EXTENDS ID LCURL ClassBody RCURL { createClassDec $2 $4 $6 }

ClassBody:
fDecs Constructor Methods { (reverse $1,$2,$3) }
| fDecs Constructor { (reverse $1,$2,[])}
| Constructor Methods {([],$1,$2)}
|Constructor {([],$1,[])}
    
fDecs:
fDec { $1 :: []}
| fDecs fDec{ $2::$1}

fDec:
ID ID SEMI { (ClassTy $1 , $2) }

Constructor:
ID LPAREN parameter RPAREN LCURL ConBody RCURL
 { createConstructorDec (reverse $3) $6}
| ID LPAREN RPAREN LCURL ConBody RCURL {createConstructorDec [] $5}

ConBody:
SUPER LPAREN RPAREN SEMI { {sup=[] ; init=[]}}
|SUPER LPAREN super_argument RPAREN SEMI { {sup=reverse $3 ; init=[]}}
|SUPER LPAREN super_argument RPAREN SEMI These { {sup=reverse $3 ; init=reverse $6} } 
|SUPER LPAREN RPAREN SEMI These { {sup=[] ; init=reverse $5} } 

These:
THIS DOT ID EQ ID SEMI { FieldAssign("this", $3, $5) :: []}
| THIS DOT ID EQ ID SEMI These { FieldAssign("this", $3, $5) :: $7}

parameter:
ID ID {(ClassTy $1 ,$2) :: []}
| parameter COLON ID ID { (ClassTy $3 , $4) :: $1}

super_argument:
ID { $1 :: []}
| super_argument COLON ID { $3 :: $1}

Methods:
Method { $1:: [] }
|Methods Method { $2 :: $1 }

Method:
ID ID LPAREN parameter RPAREN LCURL RET Term SEMI RCURL
{ createMethodDec $2 (reverse $4) (ClassTy $1) $8 }
| ID ID LPAREN RPAREN LCURL RET Term SEMI RCURL
{ createMethodDec $2 [] (ClassTy $1) $7 }

Term:
ID { Var $1}
| THIS { Var "this" }
| Term DOT ID { FieldAccess ($1,$3)}
| Term DOT ID LPAREN expr_argument RPAREN { MethCall ($1, $3, reverse $5)}
| Term DOT ID LPAREN RPAREN { MethCall ($1, $3, [])}
| NEW ID LPAREN expr_argument RPAREN { New ((ClassTy ($2)), reverse $4) }
| NEW ID LPAREN RPAREN { New ((ClassTy ($2)), []) }
| LPAREN LPAREN ID RPAREN Term RPAREN { Cast (ClassTy ($3), $5)}

expr_argument:
Term { $1:: [] }
| expr_argument COLON Term { $3 :: $1}



