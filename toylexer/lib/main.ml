open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n tokens =
  (* conta le occorrenze e forma le tuple (token * int) *) 
  let count_token acc t =
    if List.mem_assoc t acc then (*se trovo il token nella lista acc *)
      List.map (fun (tok, c) -> if tok = t then (tok, c + 1) else (tok, c)) acc
    else
      (t, 1) :: acc
  in
  (*esecuzione della funzione count_token
    partendo da una lista vuota *)
  let counts = List.fold_left count_token [] tokens in
  (* ordina per frequenza decrescente, e per token crescente in caso di parità *)
  let sorted = List.sort (fun (t1, c1) (t2, c2) -> 
    let c = compare c2 c1 in
    if c <> 0 then c else compare t1 t2
  ) counts in
  List.filteri (fun i _ -> i < n) sorted
