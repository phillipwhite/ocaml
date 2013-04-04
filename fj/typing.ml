open Syntax
open Environment

exception Error of string

let err s = raise (Error s)

(* Type Environment *)

(*type tyenv = ty Environment.t*)

let chk_tybar tybar para =
  let rec chk tybar para =
    match para with
	[] -> true
      | (t,_) :: rest -> 
	(match tybar with
	    [] -> err "argument number is wrong"
	  | x :: trest -> 
	    if subtype x t then
	      chk trest rest
	    else
	      err "wrong type in argument")
  in
  chk tybar para
	  
let rec ty_explist tyenv tytenv = function
[] -> []
  | x :: rest -> (ty_exp tyenv tytenv x) :: (ty_explist tyenv tytenv rest)
    
and  ty_exp tyenv tytenv = function
Var x when x = "this" ->
  if tytenv.cty <> ClassTy("Object") then
    tytenv.cty
  else
    err "Error about \"this\""
  |Var x ->
  (try tylookup x tyenv with
      Not_bound -> err ("variable not bound: " ^ x))
 (* | FieldAccess (New(c,tbar),f) ->
    flookup (getClass c) f*)
  | FieldAccess (Var(t),f) when t = "this" ->
    tylookup f tytenv.flist
  | FieldAccess (t,f) ->
    let cty = ty_exp tyenv tytenv t in
    let cl = getClass cty in
    flookup cl f
      
  | MethCall (t,mname,tbar_m) ->
    let cty = ty_exp tyenv tytenv t in
    let cl = getClass cty in
    let meth = mlookup cl mname in
    let tybar_m = ty_explist tyenv tytenv tbar_m in
    let hoge = chk_tybar tybar_m meth.para in
    meth.retty
      
  | New(c,tbar) ->
    let tybar = ty_explist tyenv tytenv tbar in
    let cl = getClass c in
    let hoge = chk_tybar tybar (append cl.con.sfields cl.con.dfields) in
    c
  | Cast (d, t) ->
    let c = ty_exp tyenv tytenv t in
    if subtype c d  || (subtype d c && c<>d) then
      d
    else if not (subtype c d) && not (subtype d c) then
      (print_string "stupid warning\n";
       d)
    else(
      pp_ty c;
      pp_ty d;
      err "Typing.Cast_error")
  | _ -> err " not implemented"



let existClass cname =
  let rec gc cname = function
  [] -> false
    | x::rest -> if cname = x.cname then true
      else gc cname rest
  in
  gc cname !ctable

let existMethod cl mname =
  let methods = getMethods cl in
  let rec lkup mname methods =
    match methods with 
	[] -> false
      | x::rest -> 
	if mname = x.mname then
	  true
	else
	  lkup mname rest
  in lkup mname methods

let rec chk_para spara para = 
  match spara with
	  [] -> if para = [] then true else false
	| (x,_) :: srest ->
	  (match para with
	      [] -> false
	    | (y,_) :: rest -> 
	      if x = y then
		chk_para srest rest
	      else false)

let chk_smeth cdec mdec =
  try
    let smeth = mlookup cdec mdec.mname in
    chk_para smeth.para mdec.para & (smeth.retty = mdec.retty)
  with
      _ -> true

let checkMDec cdec mdec sl =
  let tyenvbuf = mdec.para in
  let tytenvbuf = {cty=ClassTy(cdec.cname); flist=getFields cdec} in
  let exp_ty = ty_exp tyenvbuf tytenvbuf mdec.mbody in
  if subtype exp_ty mdec.retty && chk_smeth sl mdec then
    true
  else 
    false

let checkCDec cdec = 
  let sl = getSuperClass cdec in
  let rec chkmdecs = function
  [] -> true
    | x :: rest -> if checkMDec cdec x sl then
	chkmdecs rest else false
  in
  if chk_para cdec.con.sfields (getFields sl) then
    if chkmdecs cdec.mdecs then
      true
    else(
      print_string "Method Declaration Error";false)
  else(
    print_string "Constructor's parameter is wrong";false)


  (*let sflen = List.length sl.con.sfields + List.length sl.con.dfields in
  (sflen = List.length cdec.con.sfields ) & not (existClass cdec.cname)
  *)


let ty_decl tyenv tytenv = function
Exp e -> ty_exp tyenv tytenv  e
  | CDec cdec ->
    ctable := cdec :: !ctable;
    if checkCDec cdec then 
       ClassTy(cdec.cname)
    else
      let x::rest = !ctable in
      ctable := rest;
      print_newline();
      err "Class Declaration error"
  | _ -> err ("Not Implemented!")

 
      
