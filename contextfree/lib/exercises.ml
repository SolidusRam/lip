open Types

(* Use this grammar record as a blueprint for the exercises. *)
let todo : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [              (* Indexes *)
        S --> "0S0"; (* 0 *)
        S --> "1S1"; (* 1 *)
        S --> "";    (* 2 *)
      ];
    start = S;
  }


(* #### Exercise 1, easy (zero_n_one_n) *)
let zero_n_one_n : grammar = 
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [              (* Indexes *)
        S --> "0S1"; (* 0 *)
        S --> "";    (* 1 *)
      ];
    start = S;
  }


(* #### Exercise 2, easy (palindromes) *)
let palindromes : grammar = 
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [              (* Indexes *)
        S --> "0S0"; (* 0 *)
        S --> "1S1"; (* 1 *)
        S --> "0";   (* 2 *)
        S --> "1";   (* 3 *)
        S --> "";    (* 4 *)
      ];
    start = S;
  }


(* #### Exercise 3, medium (balanced_parentheses) *)
let balanced_parentheses : grammar = 
  {
    symbols = [ S ];
    terminals = [ '('; ')'; '['; ']'; '{'; '}' ];
    productions =
      [                 (* Indexes *)
        S --> "SS";    (* 0 *)
        S --> "(S)";   (* 1 *)
        S --> "[S]";   (* 2 *)
        S --> "{S}";   (* 3 *)
        S --> "";      (* 4 *)
      ];
    start = S;
  }


(* #### Exercise 4, hard (same_amount)

   Hint: model the language of words where the number of 0's is
   one greater than the number of 1's and viceversa, then combine them.
*)
let same_amount : grammar =
  {
    symbols = [ S; A; B ];
    terminals = [ '0'; '1' ];
    productions =
      [
        S --> "0B";   (* 0: Aggiungo 0, devo recuperare con un blocco "più 1" *)
        S --> "1A";   (* 1: Aggiungo 1, devo recuperare con un blocco "più 0" *)
        S --> "";     (* 2: Vuoto *)
        
        A --> "0S";   (* 3: Genero un eccesso di 0 mettendo 0 e poi tornando bilanciato *)
        A --> "1AA";  (* 4: Metto 1 (debito), devo recuperare con DUE blocchi "più 0" *)
        
        B --> "1S";   (* 5: Genero un eccesso di 1 mettendo 1 e poi tornando bilanciato *)
        B --> "0BB";  (* 6: Metto 0 (debito), devo recuperare con DUE blocchi "più 1" *)
      ];
    start = S;
  }