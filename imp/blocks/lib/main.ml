open Ast
open Types

let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf


(*eval_expr : state -> expr -> memval*)

let rec eval_expr st = function
  | True -> Bool true
  | False -> Bool false
  | Const n -> Int n
  | Var x -> 
      let env = topenv st in
      let loc = (match env x with
        | IVar l -> l
        | BVar l -> l) in
      getmem st loc
  | Not e -> 
      (match eval_expr st e with
       | Bool b -> Bool (not b)
       | _ -> raise (TypeError "Not expects a boolean"))
  | And (e1, e2) ->
      (match eval_expr st e1, eval_expr st e2 with
       | Bool b1, Bool b2 -> Bool (b1 && b2)
       | _ -> raise (TypeError "And expects two booleans"))
  | Or (e1, e2) ->
      (match eval_expr st e1, eval_expr st e2 with
       | Bool b1, Bool b2 -> Bool (b1 || b2)
       | _ -> raise (TypeError "Or expects two booleans"))
  | Add (e1, e2) ->
      (match eval_expr st e1, eval_expr st e2 with
       | Int n1, Int n2 -> Int (n1 + n2)
       | _ -> raise (TypeError "Add expects two integers"))
  | Sub (e1, e2) ->
      (match eval_expr st e1, eval_expr st e2 with
       | Int n1, Int n2 -> Int (n1 - n2)
       | _ -> raise (TypeError "Sub expects two integers"))
  | Mul (e1, e2) ->
      (match eval_expr st e1, eval_expr st e2 with
       | Int n1, Int n2 -> Int (n1 * n2)
       | _ -> raise (TypeError "Mul expects two integers"))
  | Eq (e1, e2) ->
      (match eval_expr st e1, eval_expr st e2 with
       | Int n1, Int n2 -> Bool (n1 = n2)
       | Bool b1, Bool b2 -> Bool (b1 = b2)
       | _ -> raise (TypeError "Eq expects two values of the same type"))
  | Leq (e1, e2) ->
      (match eval_expr st e1, eval_expr st e2 with
       | Int n1, Int n2 -> Bool (n1 <= n2)
       | _ -> raise (TypeError "Leq expects two integers"))



let eval_decl st decls =
  (* Funzione helper per processare UNA dichiarazione *)
  let process_decl (env_acc, loc_acc) decl =
    match decl with
    | IntVar x -> 
        let new_env = bind_env env_acc x (IVar loc_acc) in
        (new_env, loc_acc + 1)
    | BoolVar x -> 
        let new_env = bind_env env_acc x (BVar loc_acc) in
        (new_env, loc_acc + 1)
  in
  
  (* Processa TUTTE le dichiarazioni *)
  let (new_env, new_firstloc) = 
    List.fold_left process_decl (topenv st, st.firstloc) decls
  in
  
  (* Restituisci nuovo stato *)
  { envstack = new_env :: st.envstack;
    memory = st.memory;
    firstloc = new_firstloc }

let rec trace1 = function
  | St _ -> raise NoRuleApplies
  | Cmd (c,st) -> 
      match c with
      | Skip -> St st
      | Assign(x, e) ->  
          let env = topenv st in
          let loc = (match env x with
            | IVar l -> l
            | BVar l -> l) in
          let v = eval_expr st e in
          let new_mem = bind_mem st.memory loc v in
          St (setmem st new_mem)
      | Seq(Skip, c2) -> Cmd(c2, st)
      | Seq(c1, c2) ->
        (match trace1 (Cmd(c1, st)) with
         | St st' -> Cmd(c2, st')  (* c1 è terminato, passa a c2 *)
         | Cmd(c1', st') -> Cmd(Seq(c1', c2), st'))  (* c1 non è finito, continua *)
      | If (cond, c_then, c_else) ->
          (match eval_expr st cond with
           | Bool true -> Cmd(c_then, st)
           | Bool false -> Cmd(c_else, st)
           | _ -> raise (TypeError "Errore di tipo nell'if si aspetta un booleano"))
      | While (e, c) ->
        (*trasformo in if (e,seq(c,While e c)),skip*)
        Cmd(If(e, Seq(c, While(e, c)), Skip), st)
      | Decl(decls, c) ->
        let st' = eval_decl st decls in
        Cmd(Block(c), st')
      | Block(Skip) ->
        St (setenv st (popenv st))
      | Block(c) ->
        (match trace1 (Cmd(c, st)) with
         | St st' -> Cmd(Block(Skip), st')
         | Cmd(c', st') -> Cmd(Block(c'), st'))


let trace n cmd =
  let rec trace_aux conf acc count =
    if count >= n then List.rev (conf :: acc)
    else
      match conf with
      | St _ -> List.rev (conf :: acc)
      | Cmd _ ->
          try
            let conf' = trace1 conf in
            trace_aux conf' (conf :: acc) (count + 1)
          with NoRuleApplies -> List.rev (conf :: acc)
  in
  trace_aux (Cmd(cmd, state0)) [] 0