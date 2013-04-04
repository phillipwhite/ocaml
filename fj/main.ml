open Syntax
open Eval
open Typing

let cdr = function [] -> [] | _ :: rest -> rest;;

exception Not_bound

let inputdata = [
  "class Pair extends Object {
 Object fst;
 Object snd;
 Pair(Object fst, Object snd){
  super(); this.fst=fst; this.snd=snd;
 }
 Pair setfst(Object newfst) {
  return new Pair(newfst, this.snd);
 }}";
"class A extends Object {  A(){super();}}";
"class B extends Object {  B(){super();}}";
"class cPair extends Pair {
 Pair p;
 cPair(Object fst, Object snd, Pair p){
 super(fst,snd);this.p=p;
}
 Object getp(){
  return this.p;
}
 Object getp_fst(){
  return ((Pair)this.getp()).fst;
}
 cPair getthis(){
  return this;
}}";
"((Pair)new Pair(new Pair(new A(), new B()), new A()).fst).snd;";
"new cPair(new A(), new B(), new Pair(new Object(), new A())).getp_fst();";]

let thisenv = {c=List.hd Environment.init_ctable;fields=[]};;
let tytenv = {cty=ClassTy("Object");flist=[]};;

(* let rec reverse = function [] -> [] | x :: rest -> append ( reverse rest) [x];; *)
let rec read_eval_print ?(opt="") () =
  print_string "# ";
  let input = ref (Lexing.from_string "") in
  if opt = "" then
    (
      flush stdout;
      input := Lexing.from_channel stdin
    )
  else
    (print_string opt;
     print_newline();
     input := Lexing.from_string opt);
  (try
     let decl = Parser.toplevel Lexer.main !input in
    let retty = ty_decl [] tytenv decl in
    let v = eval_decl [] thisenv decl in
    Printf.printf "val  = ";
    pp_val v;
    print_newline();
    Printf.printf "type = ";
    pp_ty retty;
  with
    | Parsing.Parse_error ->
      print_string ("Parsing.Parse_error:");
    |  Error s | Environment.Error s->
      print_string ("Error:" ^ s);
    | Environment.Not_bound ->
      print_string "Error:Not_bound";
    | _ ->
      print_string "Error:");
  print_newline();
  print_newline();
  if opt = "" then(
    read_eval_print ())


let rec read_eval_print2  () =
  print_string "(^p^)>>";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let v = eval_decl [] thisenv decl in
  Printf.printf "val  = ";
  pp_val v;
  print_newline();
  read_eval_print2 ()

let _ =
  if Array.length Sys.argv = 1 then
    read_eval_print ()
  else
    if Sys.argv.(1) = "-d" then 
      read_eval_print2 ()
    else if Sys.argv.(1) = "-autoinput" then
      let rec autoinput = function
      [] -> read_eval_print ()
	| x::rest -> 
	  read_eval_print ~opt:x ();
	  autoinput rest
      in autoinput inputdata
