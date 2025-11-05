(* Tipo per rappresentare una regola S/B *)

type rule = {
  survive: int list;  (* S: numeri di vicini vivi necessari per sopravvivere *)
  born: int list;     (* B: numeri di vicini vivi necessari per nascere *)
}

(* Regola di Conway: S23/B3 *)
let conway = {
  survive = [2; 3];
  born = [3];
}