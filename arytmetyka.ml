type wartosc = 
    | Przedzial of float * float
    | Dopelnienie of float*float
    | Zbior_pusty of float;;

(*
wartosc - główny typ danych

Przedzial - typ dla pojedyńczych przedziałów np. [1;192] , [-inf;+inf] [-inf;0]

Dopelnienie - typ dla sumy dwóch przedziałów np. Depelnienie [1;2] = [-inf;1] U [2;+inf],
[-100;0]= [-inf;-100] U [0;+inf]

Zbior_pusty - typ dla nieokreślonych wyników np. 0. /. 0. = NaN

*)

let wartosc_dokladnosc x  p  =
    let poczatek_przedzialu = x -. abs_float x*.p /. 100. in
    let koniec_przedzialu = x +. abs_float x*.p /. 100. in
    Przedzial (poczatek_przedzialu,koniec_przedzialu);;

