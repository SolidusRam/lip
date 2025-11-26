open Recognizer
(* Test lang1: [01]+ *)
let%test "lang1_empty" = lang1 (explode "") = false
let%test "lang1_0" = lang1 (explode "0") = true
let%test "lang1_1" = lang1 (explode "1") = true
let%test "lang1_01" = lang1 (explode "01") = true
let%test "lang1_invalid" = lang1 (explode "012") = false

(* test lang2 *)
let%test "lang2_01" = lang2 (explode "01") = true
let%test "lang2_011" = lang2 (explode "011") = true
let%test "lang2_1" = lang2 (explode "1") = true
let%test "lang2_11" = lang2 (explode "11") = true
let%test "lang2_10" = lang2 (explode "10") = false
let%test "lang2_001" = lang2 (explode "001") = false
let%test "lang2_0" = lang2 (explode "0") = false

(* test lang3 0[01]*0 *)
let%test "lang3_00" = lang3 (explode "00") = true
let%test "lang3_010" = lang3 (explode "010") = true
let%test "lang3_0110" = lang3 (explode "0110") = true
let%test "lang3_10" = lang3 (explode "10") = false
let%test "lang3_1" = lang3 (explode "1") = false
let%test "lang3_01" = lang3 (explode "01") = false


(* test lang4 0*10*10*  *)
let%test "lang4_010" = lang4 (explode "010") = false
let%test "lang4_001010" = lang4 (explode "001010") = true
let%test "lang4_101" = lang4 (explode "101") = true
let%test "lang4_00011" = lang4 (explode "00011") = true
let%test "lang4_000" = lang4 (explode "000") = false
let%test "lang4_111" = lang4 (explode "111") = false

(* test lang5 (00|11)+ *)

let%test "lang5_00" = lang5 (explode "00") = true
let%test "lang5_11" = lang5 (explode "11") = true
let%test "lang5_0011" = lang5 (explode "0011") = true
let%test "lang5_1100" = lang5 (explode "1100") = true
let%test "lang5_000011" = lang5 (explode "000011") = true
let%test "lang5_empty" = lang5 (explode "") = false
let%test "lang5_0" = lang5 (explode "0") = false
let%test "lang5_01" = lang5 (explode "01") = false
let%test "lang5_110"= lang5 (explode "110") = false


let%test "jolly word" = belongsTo (explode "0011") = [true;true;true;true;true]