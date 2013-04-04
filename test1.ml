1+1;;
int_of_char 'Z';;
float_of_int 2 +. 2.5;;
int_of_float 0.7;;
int_of_string "0xff";;
5.0 ** 2.0;;
if true&&false then 2 else 2;;
8*2;;
int_of_float (-0.7);;
not (true && false);;
float_of_int (int_of_float 5.0);;
(sin 3.14 /. 2.0) ** (2.0 +. cos 3.14 /. 2.0) ** 2.0;;
int_of_float(sqrt(float_of_int(3*3 + 4*4)));;
let x=1 in let x=3 in let x=x+2 in x*x;;
let x = 2 and y = 3 in (let y = x and x = y + 2 in x  * y )+y;;
let x = 2 in let y = 3 in let y = x in let z = y + 2 in x * y * z;;
(1,1.0,0.1,"hgoehoge")
let bigtuple = ("Atsushi", "Igarashi", 10, 142)
let (a,b,c,d) = bigtuple;;
let ((a,_),(b,_)) = ((4,3),(2,1))
let geo_mean (x , y) = sqrt (x *. y);;
3.0**3.0;;
let rec fact n = if n = 1 then 1 else fact(n-1) * n;;

let integral f a b = 
  let n = 10000 in 
  let delta = (b -. a) /. float_of_int(n) in 
  let rec loop i =
    if i > n then 0.0 else
      (f(a +. float_of_int(i - 1) *. delta) +. f(a +. float_of_int(i) *. delta)) *. delta /. 2. +. loop(i+1)
  in loop 1;;

let pi = 3.1415926535;;

let curry f x y = f (x , y);;
let average (x , y) = (x +. y ) /. 2.;;
let curried_avg = curry average;;
average(5.0,3.0);;
curried_avg 5.0 3.0;;

let uncurry f (x , y) = f x y;;
let re_avg = uncurry curried_avg;;
re_avg (3. , 5.);;

(* This makes error. *)
let test x = if x then 0 else x;;

let apply f x = f x;;

let double f x = f ( f x);;

double (fun x -> x + 1) 1;;

let rec repeat f n x =
  if n > 0 then repeat f (n - 1) (f x) else x;;

let bigtuple = (1, true, "Objective Caml", 4.0);;
let (i, b, s, f) = bigtuple;;

let fib n = 
  let (fibn , _) = 
    repeat  (fun (a , b) -> ((a + b),a)) n (1,0)
  in fibn;;

let id x = x;;
let apply f x = f x;;
let ($) f g x = f (g x);;

let rec funny f n =
  if n = 0 then id
  else if n mod 2 = 0 then funny (f $ f) ( n / 2)
  else funny (f $ f) (n / 2) $ f;;

sqrt(3.0);;
(funny sqrt 2)81.;;

let rec sum_list l = 
  match l with 
      [] -> 0
    | n  :: rest -> n + (sum_list rest);;

let nextrand seed =
  let a = 16807.0 and m = 2147483647.0 in
  let t = a *. seed
  in t -. m *. floor (t /. m)
let rec randlist n seed tail =
  if n = 0 then (seed, tail)
  else randlist (n - 1) (nextrand seed) (seed::tail);;

let rec insert (x : float) = function
[] -> [x]
  | (y :: rest) as l -> if x < y then x :: l else y :: (insert x rest);;

let rec insertion_sort = function
[] -> []
  | x :: rest -> insert x (insertion_sort rest);;

randlist 10 3. [];;
insertion_sort (randlist 10 3. []);;

let hd (x::rest) = x
let tl (x::rest) = rest;;

let null = function [] -> true | _ -> false;;

let rec nth n l =
  if n = 1 then hd l else nth (n - 1) (tl l);;
let rec take n l = 
  if n = 0 then [] else (hd l) :: (take (n - 1) (tl l));;
let rec drop n 1 = 
  if n = 0 then l else drop (n - 1) (tl l);;

let rec length = function [] -> 0 | _ :: rest -> 1 + length rest ;;

let rec append l1 l2 = match l1 with [] -> l2 | x :: rest -> x :: (append rest l2);;

let rec reverse = function [] -> [] | x :: rest -> append ( reverse rest) [x];;

let rec revAppend l1 l2 =
  match l1 with[] -> l2
    | x :: rest -> revAppend rest ( x :: l2);;
let rev x = revAppend x [];;

let rec map f = function
[] -> []
  | x :: rest -> f x :: map f rest;;

let rec forall p = function
[] -> true
  | x :: rest -> if p x then forall p rest else false;;

let rec exists p = function
[] -> false
  | x:: rest -> (p x) or (exists p rest);;

let rec fold_right f l e =
  match l with [] -> e | x :: rest -> f x (fold_right f rest e);;
let rec fold_left f e l = 
  match l with [] -> e | x :: rest -> fold_left f (f e x) rest;;

let car = function [] -> [] | x :: rest -> x;;
let cdr = function [] -> [] | _ :: rest -> rest;;

let rec quick = function
[] -> []
  | [x] -> [x]
  | x :: xs -> (* x is the pivot *)
    let rec partition left right = function
    [] -> (quick left) @ (x :: quick right)
      | y :: ys -> if x < y then partition left (y :: right) ys
	else partition (y :: left) right ys
    in partition [] [] xs;;

insertion_sort ( snd ( randlist 10 1.0 []));;
quick (snd (randlist 10 1. []));;

let rec quicker l sorted = 
  match l with
      [] -> sorted
    | [x] -> x::sorted
    | x::xs ->
      let rec partition left right = function
      [] -> (quicker left (quicker [x] (quicker right sorted)))
	| y :: ys -> if x < y then partition left (y :: right) ys
	  else partition (y :: left) right ys
      in partition [] [] xs;;

