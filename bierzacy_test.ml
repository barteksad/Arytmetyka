#use "arytmetyka.ml";;

let a = wartosc_od_do 3. 7.;;                (* [3., 7.]                      *)

let b = wartosc_od_do (-2.) 5.;;             (* [-2., 5.]                     *)

let c = podzielic a b;;                      (* [-inf, -1.5] U [0.6, inf]     *)

let d = podzielic c b;;                 (*   [-inf, -0.3] U [0.12, inf]    *)

let e = plus d (wartosc_dokladna 2.);;       (* [-inf, 1.7] U [2.12, inf]     *)

