open Syntax
type 'a t = (Syntax.field * 'a) list

exception Not_bound
exception Error of string
let err s = raise (Error s)

let objcon = {sfields=[]; dfields=[];
	     body={sup=[]; init=[]}}

let init_ctable = {cname="Object"; super_name=""; fdec=[];
	      con=objcon; mdecs=[] }::[]

let empty = []
let extend x v env = (x,v)::env

let ctable = ref init_ctable

let rec lookup x env = 
  try List.assoc x env with Not_found -> raise Not_bound

let rec map f = function
    [] -> []
  | (id, v)::rest -> (id, f v) :: map f rest

let rec fold_right f env a = 
  match env with
      [] -> a
    | (_, v)::rest -> f v (fold_right f rest a)

let rec append l1 l2 = 
  match l1 with
      [] -> l2
    | x :: rest -> x :: (append rest l2);;

let rec reverse = function
[] ->[]
  | x::rest -> append (reverse rest) [x];;


let getSuperClass cl = 
  let rec gsc cl = function
  [] -> err "SuperClass not found"
    | x::rest  -> if  cl.super_name = x.cname then x
      else gsc cl rest
  in gsc cl !ctable
  
let getClass cty =
  let cname = match cty with ClassTy(x) -> x in
  let rec gc cname = function
  [] -> err "Class not found"
    | x::rest -> if cname = x.cname then x
      else gc cname rest
  in
  gc cname !ctable

let rec getFields cl =
  if cl.cname = "Object" then []
  else append (getFields (getSuperClass cl)) cl.fdec

let rec getMethods cl=
  if cl.cname = "Object" then []
  else append cl.mdecs (getMethods (getSuperClass cl))

(* type‚ð•Ô‚· *)
let rec assocty fname = function
[] -> err "Not_bound at assocty"
  | (t,f)::rest -> 
    if f = fname then
      t
    else
      assocty fname rest

let tylookup fname flist = 
  assocty fname flist
    
let flookup cl fname =
  assocty fname (getFields cl) 

let  mlookup cl mname =
  let methods = getMethods cl in
  let rec lkup mname methods =
    match methods with 
	[] -> err "Method not found"
      | x::rest -> 
	if mname = x.mname then
	  x
	else
	  lkup mname rest
  in lkup mname methods


(* c <: d *)
let subtype c d =
  let cl = getClass c in
  let dl = getClass d in
  let rec st cl =
    if cl.cname = dl.cname then
      true
    else
      if cl.cname = "Object" then
	false
      else
	st (getSuperClass cl)
  in
  st cl

let isClass cty =
  try
    let a = getClass cty in
    true
  with
      Not_bound -> false


let isvalue t =
  match t with
      New(c,tbar) ->
	if isClass c then
	  let cl = getClass c in
	  let cflen = (List.length cl.con.sfields) + (List.length cl.con.dfields) in
	  if cflen = List.length tbar then
	    true
	  else
	    err "wrong argument number"
	else
	  err "Class don't exist";
    | _ -> false
