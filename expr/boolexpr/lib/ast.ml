type boolExpr =
    True
  | False
  | If of boolExpr * boolExpr * boolExpr
  | Or of boolExpr * boolExpr
  | Not of boolExpr
  | Implies of boolExpr * boolExpr
;;

let is_value : boolExpr -> bool = function
  | True -> true
  | False -> true
  | _ -> false
;;
