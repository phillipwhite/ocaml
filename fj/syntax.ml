(* ML interpreter / type reconstruction *)

type ty = ClassTy of string
type field = string

type classdec = {cname : string; super_name : string ;
		 fdec : (ty * field) list ; con : con ; mdecs : mdec list}

and con = { sfields : (ty * field ) list ; dfields : (ty * field ) list;
	    body : conbody}

and conbody = { sup : field list ; init : init list }

and init = FieldAssign of string * field * field

and mdec = {mname : string; para : (ty * string) list; 
	    retty : ty; mbody : term}

and term = 
    Var of string
  | FieldAccess of term * field
  | MethCall of term * string * (term list)
  | New of ty * (term list)
  | Cast of ty * term

type inst = {c : classdec; fields : (field * term) list}

type instty = {cty : ty; flist:(ty * field) list}

type program =
    CDec of classdec
  | Exp of term

let pp_ty = function
ClassTy x -> print_string x

(*let fresh_tyvar = 
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; v
  in body

let rec freevar_ty ty =
  match ty with
      TyInt -> MySet.empty
    | TyBool -> MySet.empty
    | TyVar arg -> MySet.singleton arg
    | TyFun (arg1, arg2) -> MySet.union (freevar_ty arg1) (freevar_ty arg1)
*)
