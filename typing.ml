open Syntax

exception Error of string

let err s = raise (Error s)

let rec map f = function
    [] -> []
  | x::rest -> (f x) :: map f rest

(* Type Environment *)

type tyenv = ty Environment.t

type subst = (tyvar * ty) list

let  subst_type subst = 
  let rec sbsttype sbst =  function
  TyInt -> TyInt
    | TyBool -> TyBool
    | TyVar x -> 
      (match sbst with
	  []-> TyVar x
	| (a,t)::rest -> 
	  if x = a then sbsttype rest t
	  else sbsttype rest (TyVar x))
    | TyFun (x,y) -> TyFun ( sbsttype sbst x, sbsttype sbst y)
  in sbsttype subst


let rec eqs_of_subst s =
  match s with
      []->[]
    | ((a,t): tyvar * ty)::rest -> (TyVar a , t)::(eqs_of_subst rest)

let rec subst_eqs s eqs = 
  match eqs with
      [] -> []
    | (t1, t2)::rest -> ((subst_type s t1), (subst_type s t2))::(subst_eqs s rest)

let rec unify l = 
  match l with
      [] -> []
    | (x , y) :: rest when x = y -> unify rest
    | (t , TyVar a) :: rest ->
      if not (MySet.member a (Syntax.freevar_ty t)) then
	(a,t)::(unify (List.map 
			 (fun (i , j) -> 
			   if i = TyVar a then
			     if j = TyVar a then
			       (t,t) else (t,j)
			   else
			     if j = TyVar a then
			       (i,t) else (i,j))
			 rest ))
      else
	err("alpha is in FTV(t)")
    | (TyVar a , t) :: rest ->
      if not (MySet.member a (Syntax.freevar_ty t)) then
	(a,t)::(unify (List.map 
			 (fun (i , j) -> 
			   if i = TyVar a then
			     if j = TyVar a then
			       (t,t) else (t,j)
			   else
			     if j = TyVar a then
			       (i,t) else (i,j))
			 rest ))
      else
	err("alpha is in FTV(t)")
    | (TyFun (t11,t12), TyFun(t21,t22))::rest ->
      unify ((t11,t21)::(t12,t22)::rest)
    | _ -> err("unknown type pair")


(* let ty_prim op ty1 ty2 = match op with *)
(*     Plus -> (match ty1, ty2 with *)
(* 	TyInt, TyInt -> TyInt *)
(*       | _ -> err ("Argument must be of integer: +")) *)
(*   | Mult -> (match ty1, ty2 with *)
(* 	TyInt, TyInt -> TyInt *)
(*       | _ -> err ("Argument must be of integer: *")) *)
(*   | Conj -> (match ty1, ty2 with *)
(* 	TyBool, TyBool -> TyBool *)
(*       | _ -> err ("Argument must be of integer: &&")) *)
(*   | Disj -> (match ty1, ty2 with *)
(* 	TyBool, TyBool -> TyBool *)
(*       | _ -> err ("Argument must be of integer: ||")) *)
(*   | Lt -> (match ty1, ty2 with *)
(* 	TyInt, TyInt -> TyBool *)
(*       | _ -> err ("Argument must be of integer: <")) *)
(* (\*  | Cons -> err "Not Implemented!" *)
(* *\) *)

  (* | IfExp (exp1, exp2, exp3) -> *)
  (*   let t1 = ty_exp tyenv exp1 in *)
  (*       (match t1 with *)
  (* 	    TyBool -> *)
  (* 	      let t2 = ty_exp tyenv exp2 in *)
  (* 	      let t3 = ty_exp tyenv exp3 in  *)
  (* 	      if t2 = t3 then *)
  (* 		t2 *)
  (* 	      else *)
  (* 		err("two expressions must have same type: if") *)
  (* 	  | _ -> err ("Test expression must be boolean: if")) *)

let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Conj -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Disj ->([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)

let rec ty_exp tyenv = function
Var x ->
  (try ([], Environment.lookup x tyenv) with
      Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let (eqs3, ty) = ty_prim op ty1 ty2 in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
    let s3 = unify eqs in (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
    let (s1,ty1) = ty_exp tyenv exp1 in
    let (s2,ty2) = ty_exp tyenv exp2 in
    let (s3,ty3) = ty_exp tyenv exp3 in 
    let eqs = (eqs_of_subst s1) @ [(ty1, TyBool)] @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ [(ty2,ty3)] in
    let s4 = unify eqs in (s4, subst_type s4 ty2)
	  (*if ty2 = ty3 then
	    ([],ty2)
	    else
	    err("two expressions must have same type: if")*)
  (* | _ -> err ("Test expression must be boolean: if"))  *)
  | LetExp(id, exp1, exp2) ->
    let (_,t1) = ty_exp tyenv exp1 in
    ty_exp (Environment.extend id t1 tyenv) exp2
  | FunExp (id, exp) ->
    let domty = TyVar (fresh_tyvar ()) in
    let s, ranty =
      ty_exp (Environment.extend id domty tyenv) exp  in
    (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) ->
    let (s1,ty1) = ty_exp tyenv exp1 in
    (match ty1 with
	TyFun (t1,t2) ->
	  let (s2,ty2) = ty_exp tyenv exp2 in
	  let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(t1, ty2)] in
	  let s3 = unify eqs in (s3, subst_type s3 t2)
      |_ -> err ("e1 in T-App must be TyFun"))
  | _ -> err (" Not Implemented")


let ty_decl tyenv = function
Exp e -> (ty_exp tyenv e),tyenv
  | Decl ((id,e) :: []) ->
    let s,ty = ty_exp tyenv e in
    (s,ty),(Environment.extend id ty tyenv)
(*  | RecDecl (id, para, exp) -> 
    let dummyenv = ref Environment.empty in
    let newenv = Environment.extend id (ProcV (para, exp, dummyenv)) tyenv in 
    dummyenv := newenv;
    (id, newenv,IntV 0) :: ("-end",newenv,IntV 0) :: []*)
  | _ -> err ("Not Implemented!")

 
      
