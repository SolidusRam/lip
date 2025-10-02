open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"


let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(_,_,_) -> failwith "TODO"
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval = function
    True -> true
  | False -> false
  | If(b1,b2,b3) -> if eval b1 then eval b2 else eval b3
