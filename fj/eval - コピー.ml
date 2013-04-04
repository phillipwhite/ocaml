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
      [] -> env
    | x::rest ->
      let (_,aname) = List.hd para in
      apply_arg (List.tl para) rest ((aname,x) :: env)


let rec take ls n =
  if n <= 0 || ls = [] then []
  else List.hd ls :: take (List.tl ls) (n - 1)

let rec drop ls n =
  if n <= 0 || ls = [] then ls
  else drop (List.tl ls) (n-1)
 

(* コンストラクタの引数と変数の名前、一緒だろうからenvはそのまま使うか *)
(* return (field * value?) list *)
let rec new_inst (cl:classdec) arg ctable  =
  let sflen = List.length cl.con.body.sup in
  let envbuf = apply_arg cl.con.dfields (drop arg sflen) [] in
  if cl.cname = "Object" then
    []
  else
    append envbuf (new_inst 
		     (getSuperClass cl ctable) 
		     (take arg sflen) ctable)

(* let lkup f = function *)
(* [] -> err "field is not found" *)
(*   | (fname,v) :: rest -> if fname = f then v else lkup f rest  *)



let rec eval_explist env thisenv ctable = function
[] -> []
  | x :: rest -> (eval_exp env thisenv ctable x) :: (eval_explist env thisenv ctable rest)

and eval_exp env  thisenv ctable = function
Var x -> lookup x env

  | FieldAccess (New(ClassTy(c),tbar),f) -> 
    let envbuf = new_inst (getClass c ctable) (eval_explist env thisenv ctable tbar) ctable in
    lookup f envbuf

  | FieldAccess (Var(t),f) when t = "this" ->
    lookup f thisenv
  | FieldAccess (t,f) -> FieldAccess(eval_exp env thisenv ctable t, f)
    
  | MethCall (New(ClassTy(c),tbar),mname,tbar_m) ->
    let cl = getClass c ctable in
    let thisenvbuf = new_inst cl (eval_explist env thisenv ctable tbar) ctable in
    let meth = mlookup cl mname ctable in
    let envbuf =  apply_arg meth.para (eval_explist env thisenv ctable tbar_m ) [] in
    eval_exp envbuf thisenvbuf ctable meth.mbody

  (* | MethCall ( this )*)

  | MethCall (t,mname,tlist) ->
    MethCall ((eval_exp env thisenv ctable t), mname, tlist) 
  | New (cty, tbar) -> New(cty, (eval_explist env thisenv ctable tbar))
  | _ -> err "Not Implemented"


let rec eval_decl ctable env thisenv = function
CDec cdec -> (cdec::ctable, New(ClassTy(cdec.cname), []))
  | Exp e -> (ctable, eval_exp env thisenv ctable e)
  

