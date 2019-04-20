open Ast 
open Eval

let parse (s : string) : expr = 
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.prog Lexer.read lexbuf in 
  ast

let eval (e : expr) : value = Eval.EnvModel.eval e

let interpret (s : string) : value = 
  let exp = parse s in 
  let v = eval exp in 
  v

let s = "let x = 5 + 6 in if 3 <= x then x + 31 else 0"
let s2 = "let x = 5 in let f = fun y -> y + x in let x = 1000 in f (-47)"