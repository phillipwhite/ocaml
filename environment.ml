type 'a t = (Syntax.id * 'a) list

exception Not_bound

let empty = []
let extend x v env = (x,v)::env

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
