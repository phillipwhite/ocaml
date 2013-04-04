type token =
  | LPAREN
  | RPAREN
  | SEMISEMI
  | PLUS
  | MULT
  | LT
  | CONJ
  | DISJ
  | IF
  | THEN
  | ELSE
  | TRUE
  | FALSE
  | LET
  | IN
  | EQ
  | REC
  | RARROW
  | FUN
  | INTV of (int)
  | ID of (Syntax.id)

open Parsing;;
# 2 "parser.mly"
open Syntax
# 28 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* SEMISEMI *);
  260 (* PLUS *);
  261 (* MULT *);
  262 (* LT *);
  263 (* CONJ *);
  264 (* DISJ *);
  265 (* IF *);
  266 (* THEN *);
  267 (* ELSE *);
  268 (* TRUE *);
  269 (* FALSE *);
  270 (* LET *);
  271 (* IN *);
  272 (* EQ *);
  273 (* REC *);
  274 (* RARROW *);
  275 (* FUN *);
    0|]

let yytransl_block = [|
  276 (* INTV *);
  277 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\002\000\002\000\002\000\
\002\000\002\000\005\000\008\000\007\000\006\000\006\000\009\000\
\009\000\010\000\010\000\011\000\011\000\012\000\012\000\013\000\
\013\000\014\000\014\000\014\000\014\000\014\000\004\000\000\000"

let yylen = "\002\000\
\002\000\002\000\009\000\004\000\005\000\001\000\001\000\001\000\
\001\000\001\000\006\000\010\000\004\000\003\000\001\000\003\000\
\001\000\003\000\001\000\003\000\001\000\003\000\001\000\002\000\
\001\000\001\000\001\000\001\000\001\000\003\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\027\000\028\000\000\000\000\000\
\026\000\029\000\032\000\000\000\000\000\006\000\007\000\008\000\
\009\000\010\000\000\000\000\000\000\000\000\000\000\000\025\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\002\000\
\000\000\000\000\000\000\000\000\000\000\024\000\000\000\000\000\
\030\000\000\000\000\000\000\000\000\000\014\000\016\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\000\000\000\
\031\000\000\000\000\000\011\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\000\000\000\012\000"

let yydgoto = "\002\000\
\011\000\012\000\063\000\014\000\015\000\016\000\017\000\018\000\
\019\000\020\000\021\000\022\000\023\000\024\000"

let yysindex = "\003\000\
\002\255\000\000\024\255\024\255\000\000\000\000\244\254\252\254\
\000\000\000\000\000\000\021\255\025\255\000\000\000\000\000\000\
\000\000\000\000\023\255\027\255\026\255\030\255\006\255\000\000\
\245\254\038\255\036\255\029\255\031\255\033\255\000\000\000\000\
\006\255\006\255\006\255\006\255\006\255\000\000\034\255\032\255\
\000\000\024\255\046\255\024\255\024\255\000\000\000\000\030\255\
\059\255\006\255\054\255\024\255\043\255\047\255\254\254\000\000\
\052\255\057\255\024\255\061\255\064\255\024\255\000\000\065\255\
\000\000\055\255\080\255\000\000\081\255\024\255\024\255\024\255\
\005\255\086\255\091\255\000\000\024\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\145\255\135\255\050\255\101\255\073\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\115\255\
\125\255\087\255\000\000\000\000\000\000\000\000\107\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\107\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\253\255\112\000\000\000\000\000\000\000\000\000\000\000\
\081\000\086\000\088\000\096\000\097\000\235\255"

let yytablesize = 160
let yytable = "\026\000\
\027\000\038\000\003\000\001\000\028\000\039\000\003\000\076\000\
\029\000\040\000\004\000\061\000\062\000\005\000\006\000\007\000\
\030\000\005\000\006\000\077\000\008\000\009\000\010\000\031\000\
\003\000\009\000\010\000\032\000\038\000\035\000\033\000\036\000\
\004\000\034\000\037\000\005\000\006\000\025\000\053\000\041\000\
\055\000\056\000\008\000\009\000\010\000\042\000\044\000\052\000\
\058\000\043\000\045\000\019\000\019\000\059\000\051\000\065\000\
\019\000\019\000\068\000\019\000\019\000\054\000\035\000\019\000\
\019\000\060\000\073\000\074\000\075\000\057\000\064\000\062\000\
\070\000\078\000\023\000\023\000\023\000\023\000\023\000\023\000\
\023\000\066\000\023\000\023\000\067\000\069\000\023\000\023\000\
\022\000\022\000\022\000\022\000\022\000\022\000\022\000\071\000\
\022\000\022\000\072\000\061\000\022\000\022\000\021\000\021\000\
\021\000\077\000\021\000\021\000\021\000\004\000\021\000\021\000\
\013\000\046\000\021\000\021\000\020\000\020\000\020\000\047\000\
\020\000\020\000\020\000\049\000\020\000\020\000\018\000\018\000\
\020\000\020\000\048\000\018\000\018\000\050\000\018\000\018\000\
\017\000\017\000\018\000\018\000\000\000\000\000\017\000\000\000\
\017\000\017\000\015\000\015\000\017\000\017\000\000\000\000\000\
\000\000\000\000\015\000\015\000\000\000\000\000\015\000\015\000"

let yycheck = "\003\000\
\004\000\023\000\001\001\001\000\017\001\017\001\001\001\003\001\
\021\001\021\001\009\001\014\001\015\001\012\001\013\001\014\001\
\021\001\012\001\013\001\015\001\019\001\020\001\021\001\003\001\
\001\001\020\001\021\001\003\001\050\000\004\001\008\001\006\001\
\009\001\007\001\005\001\012\001\013\001\014\001\042\000\002\001\
\044\000\045\000\019\001\020\001\021\001\010\001\016\001\016\001\
\052\000\021\001\018\001\002\001\003\001\011\001\021\001\059\000\
\007\001\008\001\062\000\010\001\011\001\016\001\004\001\014\001\
\015\001\019\001\070\000\071\000\072\000\016\001\019\001\015\001\
\018\001\077\000\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\021\001\010\001\011\001\021\001\021\001\014\001\015\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\016\001\
\010\001\011\001\018\001\014\001\014\001\015\001\002\001\003\001\
\004\001\015\001\006\001\007\001\008\001\003\001\010\001\011\001\
\001\000\033\000\014\001\015\001\002\001\003\001\004\001\034\000\
\006\001\007\001\008\001\036\000\010\001\011\001\002\001\003\001\
\014\001\015\001\035\000\007\001\008\001\037\000\010\001\011\001\
\002\001\003\001\014\001\015\001\255\255\255\255\008\001\255\255\
\010\001\011\001\002\001\003\001\014\001\015\001\255\255\255\255\
\255\255\255\255\010\001\011\001\255\255\255\255\014\001\015\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  SEMISEMI\000\
  PLUS\000\
  MULT\000\
  LT\000\
  CONJ\000\
  DISJ\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  LET\000\
  IN\000\
  EQ\000\
  REC\000\
  RARROW\000\
  FUN\000\
  "

let yynames_block = "\
  INTV\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 19 "parser.mly"
              ( Exp _1 )
# 191 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'topLet) in
    Obj.repr(
# 20 "parser.mly"
                  ( Decl _1 )
# 198 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : Syntax.id) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 21 "parser.mly"
                                            ( RecDecl (_3,_6, _8) )
# 207 "parser.ml"
               : Syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 24 "parser.mly"
               ( (_2, _4)::[] )
# 215 "parser.ml"
               : 'topLet))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'topLet) in
    Obj.repr(
# 25 "parser.mly"
                        ( (_2, _4)::_5 )
# 224 "parser.ml"
               : 'topLet))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'IfExpr) in
    Obj.repr(
# 28 "parser.mly"
       ( _1 )
# 231 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LetExpr) in
    Obj.repr(
# 29 "parser.mly"
          ( _1 )
# 238 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'DISExpr) in
    Obj.repr(
# 30 "parser.mly"
          ( _1 )
# 245 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'FunExpr) in
    Obj.repr(
# 31 "parser.mly"
          ( _1 )
# 252 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LetRecExpr) in
    Obj.repr(
# 32 "parser.mly"
             ( _1 )
# 259 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 35 "parser.mly"
                       ( LetExp (_2, _4, _6) )
# 268 "parser.ml"
               : 'LetExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : Syntax.id) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : Syntax.id) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 38 "parser.mly"
                                         (LetRecExp (_3,_6,_8,_10))
# 278 "parser.ml"
               : 'LetRecExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Syntax.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 42 "parser.mly"
                   ( FunExp(_2, _4) )
# 286 "parser.ml"
               : 'FunExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'CONExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'CONExpr) in
    Obj.repr(
# 45 "parser.mly"
                     (BinOp(Disj,_1,_3))
# 294 "parser.ml"
               : 'DISExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'CONExpr) in
    Obj.repr(
# 46 "parser.mly"
          (_1)
# 301 "parser.ml"
               : 'DISExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'LTExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'LTExpr) in
    Obj.repr(
# 49 "parser.mly"
                   (BinOp(Conj,_1,_3))
# 309 "parser.ml"
               : 'CONExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LTExpr) in
    Obj.repr(
# 50 "parser.mly"
         (_1)
# 316 "parser.ml"
               : 'CONExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 53 "parser.mly"
                   ( BinOp (Lt, _1, _3) )
# 324 "parser.ml"
               : 'LTExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PExpr) in
    Obj.repr(
# 54 "parser.mly"
          ( _1 )
# 331 "parser.ml"
               : 'LTExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'MExpr) in
    Obj.repr(
# 57 "parser.mly"
                     ( BinOp (Plus, _1, _3) )
# 339 "parser.ml"
               : 'PExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'MExpr) in
    Obj.repr(
# 58 "parser.mly"
          ( _1 )
# 346 "parser.ml"
               : 'PExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'MExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 61 "parser.mly"
                       ( BinOp (Mult, _1, _3) )
# 354 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppExpr) in
    Obj.repr(
# 62 "parser.mly"
            ( _1 )
# 361 "parser.ml"
               : 'MExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 65 "parser.mly"
              ( AppExp (_1, _2) )
# 369 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AExpr) in
    Obj.repr(
# 66 "parser.mly"
          ( _1 )
# 376 "parser.ml"
               : 'AppExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 69 "parser.mly"
         ( ILit _1 )
# 383 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
         ( BLit true )
# 389 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
          ( BLit false )
# 395 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.id) in
    Obj.repr(
# 72 "parser.mly"
       ( Var _1 )
# 402 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 73 "parser.mly"
                       ( _2 )
# 409 "parser.ml"
               : 'AExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 76 "parser.mly"
                                ( IfExp (_2, _4, _6) )
# 418 "parser.ml"
               : 'IfExpr))
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
