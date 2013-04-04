type exp =
    Var of string
  | Abs of string * exp
  | App of exp * exp

let gensym  = 
  let counter = ref 0 in
  fun () ->
    incr counter;
    "g" ^ string_of_int !counter

let rec subst e2 x e1=
  match e1 with
      Var y ->
	if x = y then e2 else Var(y)
    | Abs(y,e) ->
      let y1 = gensym () in
      Abs( y1, subst e2 x (subst (Var(y1)) y e))
    | App(e3,e4) ->
      App(subst e2 x e3, subst e2 x e4)

let rec step e =
  match e with
      Var x -> []
    | Abs(y,e0) ->
      List.map 
	(fun e0' -> Abs(y,e0'))
	(step e0)
    | App(e1,e2) ->
      (match e1 with
	  Abs(x,e0) -> [subst e2 x e0]
	| _ -> []) @ 
	List.map (fun e1' -> App(e1', e2)) (step e1) @
	List.map (fun e2' -> App(e1, e2')) (step e2)

let rec repeat e =
  match step e with
      [] -> e
    | e' :: _ -> repeat e'


let x = Var "x"
let y = Var "y"
let z = Var "z"

blet i = Abs("x", Var("x"))

let k = Abs("x", Abs("y",Var("x")))

let s = Abs("x", Abs("y", Abs("z", App(App(Var("x"),Var("z")), App(Var("y"), Var("z"))))))

(*  É…xÉ…y.x *)
let t = k

(* É…xÉ…y.y *)
let f = App(k,i)

let mynot = App(App(s,App(App(s,i),App(k,f))),App(k,t))

let myor = App(App(s,i),App(k,t))

let myand = App(App(s,s),App(k,App(k,f)))
