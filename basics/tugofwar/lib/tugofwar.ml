(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
let explode s = 
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let toklist_of_string s = 
  let char_list = explode s in
  let rec aux tkl =
    match tkl with
    | [] -> []
    | 'A'::tl -> A :: aux tl
    | 'B'::tl -> B :: aux tl
    | '='::tl -> X :: aux tl
    | _::_ -> failwith "invalid character in input string"
  in aux char_list;;


(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l = 
  let rec aux lst seen_x seen_b =
    match lst with
    | [] -> true
    | A::tl -> if seen_x || seen_b then false else aux tl false false
    | X::tl -> if seen_b then false else aux tl true false
    | B::tl -> aux tl seen_x true
  in aux l false false


(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l =
  let rec aux ls scoreA scoreB =
    match ls with
    | [] -> if scoreA > scoreB then A else if scoreB > scoreA then B else X
    | A::tl -> aux tl (scoreA + 1) scoreB
    | B::tl -> aux tl scoreA (scoreB + 1)
    | X::tl -> aux tl scoreA scoreB
  in aux l 0 0


(* val string_of_winner : token -> string *)
let string_of_winner w = 
  match w with
  | A -> "A wins"
  | B -> "B wins"
  | X -> "Tie"
