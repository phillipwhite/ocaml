open Syntax
open Environment

exception Error of string


let err s = raise (Error s)

(* pretty printing *)


let rec string_val = function
New (ClassTy(cty),tbar) -> "new " ^ cty ^ "(" ^ string_tbar tbar ^ ")"
  | _ -> err "Evaluation Error"

and string_tbar = function
  [] -> ""
  | t::rest when rest <> [] -> string_val t ^ ", " ^ string_tbar rest
  | t::rest -> string_val t

let pp_val v = print_string (string_val v)

let rec apply_arg para arg env =
  match arg with
      [] -> 
	if para = [] then
	  env
	else
	  err "wrong argument number"
    | x::rest ->
      let (_,aname) = List.hd para in
      apply_arg (List.tl para) rest (env @ (aname,x)::[])


let rec take ls n =
  if n <= 0 || ls = [] then []
  else List.hd ls :: take (List.tl ls) (n - 1)

let rec drop ls n =
  if n <= 0 || ls = [] then ls
  else drop (List.tl ls) (n-1)
 

(* コンストラクタの引数と変数の名前、一緒だろうからenvはそのまま使うか *)
(* return (field * value?) list *)
let new_inst (cl:classdec)  arg = 
let rec newinst (cl:classdec) arg  =
  let sflen = List.length cl.con.body.sup in
  let envbuf = apply_arg cl.con.dfields (drop arg sflen) [] in
  if cl.cname = "Object" then
    []
  else
    append (newinst 
	      (getSuperClass cl) 
	      (take arg sflen)) envbuf
in {c=cl;fields=newinst cl arg}

(* let lkup f = function *)
(* [] -> err "field is not found" *)
(*   | (fname,v) :: rest -> if fname = f then v else lkup f rest  *)


let rec eval_explist env thisenv = function
[] -> []
  | x :: rest -> (eval_exp env thisenv x) :: (eval_explist env thisenv rest)

and eval_exp env thisenv = function
Var x when x = "this" -> 
  let rec tlist = function
  [] -> []
    | (t,x) :: rest -> x:: tlist rest in 
  New(ClassTy(thisenv.c.cname),tlist thisenv.fields)

  |Var x -> lookup x env

  | FieldAccess (New(c,tbar),f) when isvalue (New(c,tbar))-> 
    let envbuf = new_inst (getClass c) (eval_explist env thisenv tbar) in
    lookup f envbuf.fields

  | FieldAccess (Var(t),f) when t = "this" ->
    lookup f thisenv.fields
  | FieldAccess (t,f) -> 
    let fa = FieldAccess(eval_exp env thisenv t, f) in
    eval_exp env thisenv fa
    
  | MethCall (New(c,tbar),mname,tbar_m) when isvalue (New(c,tbar))->
    let cl = getClass c in
    let thisenvbuf = new_inst cl (eval_explist env thisenv tbar)in
    let meth = mlookup cl mname in
    let envbuf =  apply_arg meth.para (eval_explist env thisenv tbar_m ) [] in
    eval_exp envbuf thisenvbuf meth.mbody

  | MethCall (Var(t), mname,tbar_m) when t = "this" ->
    let cl = thisenv.c in
    let meth = mlookup cl mname in
    let envbuf =  apply_arg meth.para (eval_explist env thisenv tbar_m ) [] in
    eval_exp envbuf thisenv meth.mbody

  | MethCall (t,mname,tlist) ->
    MethCall ((eval_exp env thisenv t), mname, tlist) 
  | New (c, tbar) when isvalue (New(c,tbar)) ->
    New(c, (eval_explist env thisenv tbar))
  | Cast (d, New(c, tbar)) when  isClass d && isvalue (New(c,tbar))->
    (* if subtype c d  *)
    (*   || (subtype d c && c=d) then *)
    eval_exp env thisenv (New(c,tbar))
    (* else err "Cast error" *)
  | Cast (c, t) when isClass c -> 
    let e = Cast(c,eval_exp env thisenv t) in
    eval_exp env thisenv e
  | _ -> err "wrong Class name may be used."

let existClass cname =
  let rec gc cname = function
  [] -> false
    | x::rest -> if cname = x.cname then true
      else gc cname rest
  in
  gc cname !ctable

let checkCDec cdec = 
  let sl = getSuperClass cdec in
  let sflen = List.length sl.con.sfields + List.length sl.con.dfields in
  (sflen = List.length cdec.con.sfields ) & not (existClass cdec.cname)
    
   
let rec eval_decl env thisenv = function
CDec cdec -> 
    Printf.printf "\n%d fields, %d mdecs\n" (List.length cdec.fdec) (List.length cdec.mdecs);
    New(ClassTy(cdec.cname), [])
  | Exp e ->eval_exp env thisenv e
