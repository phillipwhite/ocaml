let rec factorial n = 
  if n < 1. then
    1. 
  else
    n *. factorial (n -. 1.);;

let ifactorial n =
  let fact-iter product


(* toward general print function   *)
type printtype =
    I of int
  | S of string;;

let p = function
I x -> print_int x
  | S s -> print_string s;;

let balance = ref 100;;

let withdraw amount = 
  if !balance >= amount then(
    balance := !balance - amount;
    print_int !balance
  ) else(
    print_string "insufficient funds");;

let rec tak x y z = 
  if x <= y then z
  else
    tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y);;

Toploop.initialize_toplevel_env();;

let eval txt = let lb = (Lexing.from_string txt) in
                 let phr = !Toploop.parse_toplevel_phrase lb in
                 Toploop.execute_phrase true Format.std_formatter phr;;

(* ŽžŠÔŒv‘ª *)
let timing cont = 
  let a = Sys.time () in
    cont ();
    print_float (Sys.time () -. a)

(*Usage
timing closure(lambda-function)
*)
timing (fun x -> tak 18 9 0);;
