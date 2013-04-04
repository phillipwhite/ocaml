open Syntax
open Eval
open Typing

let cdr = function [] -> [] | _ :: rest -> rest;;

(* let rec reverse = function [] -> [] | x :: rest -> append ( reverse rest) [x];; *)

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  (*let (id, newenv, v) = eval_decl env decl in
  Printf.printf "val %s = " id;
  pp_val v;*)
  let (s,ty),newtyenv = 
    try ty_decl tyenv decl with
	_ -> 
	  print_string "not implemented\n";
	  ([],TyVar(9999)),Environment.empty
  in
  let varlist = eval_decl env decl in
  let rec getvar vlist = 
    match vlist with
      [] -> env
      | ("-end",env,_)::[] -> env
      | ((id, env, v)::rest) ->
	Printf.printf "val %s : " id;
	pp_ty ty;
	print_string " = ";
	pp_val v;
	print_newline();
	getvar rest
  in
  let newenv= getvar varlist in
  read_eval_print newenv newtyenv

let rec read_eval_print2 env tyenv =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  (*let (id, newenv, v) = eval_decl env decl in
  Printf.printf "val %s = " id;
  pp_val v;*)
  let varlist = eval_decl env decl in
  let rec getvar vlist = 
    match vlist with
      [] -> env
      | ("-end",env,_)::[] -> env
      | ((id, env, v)::rest) ->
	Printf.printf "val %s : " id;
	(* pp_ty ty; *)
	print_string " = ";
	pp_val v;
	print_newline();
	getvar rest
  in
  let newenv= getvar varlist in
  read_eval_print2 newenv tyenv

      
  


let initial_env = 
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5) 
       (Environment.extend "x" (IntV 10)
	  (Environment.extend "ii" (IntV 2)
	     (Environment.extend "iii" (IntV 3)
		(Environment.extend "iv" (IntV 4) Environment.empty)))))

let initial_tyenv = 
  Environment.extend "i" TyInt
    (Environment.extend "v" TyInt
       (Environment.extend "x" TyInt Environment.empty))


let _ =
  if Array.length Sys.argv = 1 then
    read_eval_print initial_env initial_tyenv
  else
    read_eval_print2 initial_env initial_tyenv
