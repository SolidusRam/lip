(*utility functions*)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* 
[01]+ 
*)
let lang1 w = 
  match w with
  | [] -> false
  | _ -> List.for_all (fun c -> c='0' || c='1') w

(* 
0?1
 *)
let lang2 w =
  match w with
  | '0'::'1':: x -> List.for_all (fun c -> c='1') x
  | '1':: x -> List.for_all (fun c -> c='1') x
  | _ -> false

(* 
0[01]*0  
*)
let lang3 w = 
  match w with
  | '0':: tail -> (
    match List.rev tail with
    | '0':: tail_rev ->  List.for_all (fun c -> c='0' || c='1') (List.rev tail_rev)
    | _ -> false
    )
  | _ -> false



(*
0*10*10*
*)
let lang4 w = 
  let rec aux w count =
    match w with
    | [] -> count = 2 
    | '0':: tail -> aux tail count (* Ignora gli zeri *)
    | '1':: tail -> 
      if count < 2 then 
        aux tail (count + 1) 
      else 
        false 
    | _ -> false
  in
  aux w 0

(*
(00|11)+
*)
let lang5 w =
  let rec aux w =
    match w with
    | [] -> true
    | '0' :: '0':: tail -> aux tail
    | '1' :: '1':: tail -> aux tail
    | _ -> false
  in
  match w with
  | [] -> false
  | _ -> aux w


let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers

