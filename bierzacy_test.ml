#use "arytmetyka.ml";;

assert(compare (plus (Dopelnienie(-100.0,2.)) (wartosc_dokladna (-0.001))) (Dopelnienie (-100.001, 1.999)) = 0);;
