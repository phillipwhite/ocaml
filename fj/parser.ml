type token =
  | LPAREN
  | RPAREN
  | LCURL
  | RCURL
  | SEMI
  | DOT
  | COLON
  | CLASS
  | SUPER
  | RET
  | NEW
  | EXTENDS
  | THIS
  | EQ
  | ID of (string)

open Parsing;;
# 2 "parser.mly"
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
# 59 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* LCURL *);
  260 (* RCURL *);
  261 (* SEMI *);
  262 (* DOT *);
  263 (* COLON *);
  264 (* CLASS *);
  265 (* SUPER *);
  266 (* RET *);
  267 (* NEW *);
  268 (* EXTENDS *);
  269 (* THIS *);
  270 (* EQ *);
    0|]

let yytransl_block = [|
  271 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\004\000\004\000\004\000\004\000\005\000\
\005\000\008\000\006\000\006\000\010\000\010\000\010\000\010\000\
\012\000\012\000\009\000\009\000\011\000\011\000\007\000\007\000\
\013\000\013\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\014\000\014\000\000\000"

let yylen = "\002\000\
\001\000\002\000\007\000\003\000\002\000\002\000\001\000\001\000\
\002\000\003\000\007\000\006\000\004\000\005\000\006\000\005\000\
\006\000\007\000\002\000\004\000\001\000\003\000\001\000\002\000\
\010\000\009\000\001\000\001\000\003\000\006\000\005\000\005\000\
\004\000\006\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\027\000\037\000\
\001\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\033\000\000\000\000\000\
\000\000\000\000\000\000\032\000\000\000\031\000\000\000\034\000\
\000\000\000\000\000\000\000\000\008\000\000\000\030\000\000\000\
\000\000\003\000\000\000\009\000\000\000\000\000\023\000\000\000\
\000\000\000\000\010\000\000\000\000\000\024\000\000\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\012\000\000\000\020\000\000\000\000\000\000\000\
\021\000\000\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\016\000\000\000\022\000\000\000\000\000\
\000\000\015\000\026\000\000\000\000\000\025\000\000\000\000\000\
\000\000\018\000"

let yydgoto = "\002\000\
\008\000\009\000\023\000\034\000\035\000\036\000\046\000\037\000\
\050\000\061\000\074\000\084\000\047\000\024\000"

let yysindex = "\004\000\
\000\255\000\000\005\255\018\255\020\255\000\000\000\000\000\000\
\000\000\053\255\050\255\052\255\065\255\000\000\054\255\066\255\
\056\255\001\255\069\255\009\255\064\255\000\000\067\255\002\255\
\016\255\051\255\059\255\000\000\009\255\000\000\030\255\000\000\
\006\255\068\255\059\255\061\255\000\000\067\255\000\000\021\255\
\072\255\000\000\061\255\000\000\063\255\061\255\000\000\076\255\
\070\255\041\255\000\000\061\255\079\255\000\000\073\255\000\000\
\080\255\071\255\023\255\083\255\084\255\073\255\074\255\087\255\
\045\255\024\255\000\000\088\255\000\000\077\255\090\255\086\255\
\000\000\047\255\000\000\009\255\085\255\081\255\091\255\082\255\
\055\255\009\255\092\255\000\000\081\255\000\000\095\255\057\255\
\089\255\000\000\000\000\096\255\093\255\000\000\094\255\097\255\
\081\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\039\255\000\000\000\000\000\000\048\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\099\255\000\000\049\255\000\000\000\000\
\000\000\000\000\101\255\000\000\000\000\102\255\000\000\000\000\
\000\000\000\000\000\000\104\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\106\255\000\000\000\000\
\000\000\000\000\000\000\000\000\107\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\108\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\255\255\000\000\000\000\066\000\070\000\079\000\
\056\000\054\000\000\000\201\255\244\255\092\000"

let yytablesize = 117
let yytable = "\010\000\
\003\000\003\000\022\000\028\000\001\000\011\000\040\000\004\000\
\029\000\003\000\005\000\005\000\006\000\006\000\007\000\007\000\
\003\000\030\000\026\000\005\000\041\000\006\000\048\000\007\000\
\064\000\072\000\005\000\038\000\006\000\090\000\007\000\039\000\
\012\000\054\000\013\000\049\000\029\000\049\000\073\000\054\000\
\029\000\098\000\057\000\029\000\029\000\029\000\071\000\058\000\
\079\000\035\000\036\000\058\000\032\000\080\000\035\000\036\000\
\015\000\014\000\015\000\087\000\015\000\092\000\015\000\017\000\
\016\000\018\000\027\000\020\000\019\000\025\000\021\000\042\000\
\015\000\033\000\081\000\045\000\051\000\053\000\055\000\059\000\
\088\000\060\000\062\000\066\000\056\000\063\000\076\000\067\000\
\069\000\070\000\078\000\075\000\077\000\083\000\082\000\085\000\
\086\000\089\000\091\000\094\000\043\000\097\000\007\000\093\000\
\005\000\006\000\095\000\004\000\096\000\013\000\014\000\017\000\
\052\000\044\000\065\000\068\000\031\000"

let yycheck = "\001\000\
\001\001\001\001\002\001\002\001\001\000\001\001\001\001\008\001\
\007\001\001\001\011\001\011\001\013\001\013\001\015\001\015\001\
\001\001\002\001\020\000\011\001\015\001\013\001\002\001\015\001\
\002\001\002\001\011\001\029\000\013\001\085\000\015\001\002\001\
\015\001\046\000\015\001\015\001\007\001\015\001\015\001\052\000\
\002\001\097\000\002\001\005\001\006\001\007\001\002\001\007\001\
\002\001\002\001\002\001\007\001\002\001\007\001\007\001\007\001\
\006\001\005\001\006\001\005\001\006\001\005\001\006\001\012\001\
\015\001\001\001\003\001\002\001\015\001\001\001\015\001\004\001\
\006\001\015\001\076\000\015\001\005\001\015\001\003\001\001\001\
\082\000\009\001\003\001\001\001\015\001\015\001\010\001\004\001\
\015\001\003\001\005\001\004\001\003\001\013\001\010\001\005\001\
\015\001\006\001\004\001\004\001\035\000\005\001\004\001\015\001\
\004\001\004\001\014\001\004\001\015\001\004\001\004\001\004\001\
\043\000\035\000\059\000\062\000\025\000"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LCURL\000\
  RCURL\000\
  SEMI\000\
  DOT\000\
  COLON\000\
  CLASS\000\
  SUPER\000\
  RET\000\
  NEW\000\
  EXTENDS\000\
  THIS\000\
  EQ\000\
  "

let yynames_block = "\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ClassDec) in
    Obj.repr(
# 52 "parser.mly"
        ( CDec _1 )
# 211 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    Obj.repr(
# 53 "parser.mly"
           ( Exp _1 )
# 218 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'ClassBody) in
    Obj.repr(
# 56 "parser.mly"
                                          ( createClassDec _2 _4 _6 )
# 227 "parser.ml"
               : 'ClassDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fDecs) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Constructor) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Methods) in
    Obj.repr(
# 59 "parser.mly"
                          ( (reverse _1,_2,_3) )
# 236 "parser.ml"
               : 'ClassBody))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fDecs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Constructor) in
    Obj.repr(
# 60 "parser.mly"
                    ( (reverse _1,_2,[]))
# 244 "parser.ml"
               : 'ClassBody))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Constructor) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Methods) in
    Obj.repr(
# 61 "parser.mly"
                      (([],_1,_2))
# 252 "parser.ml"
               : 'ClassBody))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Constructor) in
    Obj.repr(
# 62 "parser.mly"
             (([],_1,[]))
# 259 "parser.ml"
               : 'ClassBody))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fDec) in
    Obj.repr(
# 65 "parser.mly"
     ( _1 :: [])
# 266 "parser.ml"
               : 'fDecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fDecs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fDec) in
    Obj.repr(
# 66 "parser.mly"
            ( _2::_1)
# 274 "parser.ml"
               : 'fDecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 69 "parser.mly"
           ( (ClassTy _1 , _2) )
# 282 "parser.ml"
               : 'fDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'parameter) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'ConBody) in
    Obj.repr(
# 73 "parser.mly"
 ( createConstructorDec (reverse _3) _6)
# 291 "parser.ml"
               : 'Constructor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'ConBody) in
    Obj.repr(
# 74 "parser.mly"
                                       (createConstructorDec [] _5)
# 299 "parser.ml"
               : 'Constructor))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
                         ( {sup=[] ; init=[]})
# 305 "parser.ml"
               : 'ConBody))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'super_argument) in
    Obj.repr(
# 78 "parser.mly"
                                         ( {sup=reverse _3 ; init=[]})
# 312 "parser.ml"
               : 'ConBody))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'super_argument) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'These) in
    Obj.repr(
# 79 "parser.mly"
                                               ( {sup=reverse _3 ; init=reverse _6} )
# 320 "parser.ml"
               : 'ConBody))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'These) in
    Obj.repr(
# 80 "parser.mly"
                                ( {sup=[] ; init=reverse _5} )
# 327 "parser.ml"
               : 'ConBody))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 83 "parser.mly"
                       ( FieldAssign("this", _3, _5) :: [])
# 335 "parser.ml"
               : 'These))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'These) in
    Obj.repr(
# 84 "parser.mly"
                               ( FieldAssign("this", _3, _5) :: _7)
# 344 "parser.ml"
               : 'These))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
      ((ClassTy _1 ,_2) :: [])
# 352 "parser.ml"
               : 'parameter))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'parameter) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
                        ( (ClassTy _3 , _4) :: _1)
# 361 "parser.ml"
               : 'parameter))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parser.mly"
   ( _1 :: [])
# 368 "parser.ml"
               : 'super_argument))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'super_argument) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
                          ( _3 :: _1)
# 376 "parser.ml"
               : 'super_argument))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Method) in
    Obj.repr(
# 95 "parser.mly"
       ( _1:: [] )
# 383 "parser.ml"
               : 'Methods))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Methods) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Method) in
    Obj.repr(
# 96 "parser.mly"
                ( _2 :: _1 )
# 391 "parser.ml"
               : 'Methods))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'parameter) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    Obj.repr(
# 100 "parser.mly"
( createMethodDec _2 (reverse _4) (ClassTy _1) _8 )
# 401 "parser.ml"
               : 'Method))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    Obj.repr(
# 102 "parser.mly"
( createMethodDec _2 [] (ClassTy _1) _7 )
# 410 "parser.ml"
               : 'Method))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "parser.mly"
   ( Var _1)
# 417 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
       ( Var "this" )
# 423 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 107 "parser.mly"
              ( FieldAccess (_1,_3))
# 431 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr_argument) in
    Obj.repr(
# 108 "parser.mly"
                                          ( MethCall (_1, _3, reverse _5))
# 440 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 109 "parser.mly"
                            ( MethCall (_1, _3, []))
# 448 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr_argument) in
    Obj.repr(
# 110 "parser.mly"
                                     ( New ((ClassTy (_2)), reverse _4) )
# 456 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 111 "parser.mly"
                       ( New ((ClassTy (_2)), []) )
# 463 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    Obj.repr(
# 112 "parser.mly"
                                      ( Cast (ClassTy (_3), _5))
# 471 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 115 "parser.mly"
     ( _1:: [] )
# 478 "parser.ml"
               : 'expr_argument))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_argument) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 116 "parser.mly"
                           ( _3 :: _1)
# 486 "parser.ml"
               : 'expr_argument))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.program)
