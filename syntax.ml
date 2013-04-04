(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | Conj | Disj 

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp

type pair = id * exp

type program = 
    Exp of exp
  | Decl of pair list  | RecDecl of id * id * exp

type tyvar = int

type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty

let rec pp_ty = function
TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar i -> print_string "'a"
  | TyFun (x,y) -> pp_ty x; print_string " -> " ; pp_ty y

let fresh_tyvar = 
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
