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

let wartosc_od_do x y = 
    Przedzial (x,y);;

let wartosc_dokladna x = 
    Przedzial (x,x);;

let in_wartosc x y = 
    match x with
    | Zbior_pusty p -> false
    | Przedzial (a,b) -> y>=a && y<=b
    | Dopelnienie (a,b) -> y<=a || y>=b;;

let plus x y = 

    match x, y with
    (* x lub y jest NaN *)
    | Zbior_pusty p,_ -> Zbior_pusty nan
    | _,Zbior_pusty p -> Zbior_pusty nan

    (* przedział + przedział *)
    | Przedzial(a,b),Przedzial(k,l) ->
        let pocz = 
        match classify_float a, classify_float k with
        | FP_infinite,_ -> neg_infinity
        | _,FP_infinite -> neg_infinity
        | _,_ -> a +. k 
        in
        let kon =
        match classify_float b,classify_float l with
        | FP_infinite,_ -> infinity
        | _,FP_infinite -> infinity
        | _,_ -> b +. l
        in
        Przedzial (pocz,kon)

    (* Dopełnienie + przedział  *)
    | Przedzial(a,b),Dopelnienie(k,l) ->
        if classify_float a = FP_infinite || classify_float b = FP_infinite then Przedzial(neg_infinity,infinity) else
        if b+.k  >= l || a +. l <= k then Przedzial (neg_infinity,infinity) else
        Dopelnienie (b+.k,a +. l)

    | Dopelnienie(k,l),Przedzial(a,b) ->
        if classify_float a = FP_infinite || classify_float b = FP_infinite then Przedzial(neg_infinity,infinity) else
        if b+.k  >= l || a +. l <= k then Przedzial (neg_infinity,infinity) else
        Dopelnienie (b+.k,a +. l)

    | Dopelnienie(a,b), Dopelnienie(k,l) -> Przedzial(neg_infinity,infinity);;


let min_wartosc x = 
    match x with
    | Zbior_pusty p ->  Float.nan
    | Przedzial (a,b) -> a
    | Dopelnienie (a,b) -> neg_infinity;;

let max_wartosc x = 
    match x with
    | Zbior_pusty p -> Float.nan
    | Przedzial (a,b) -> b
    | Dopelnienie (a,b) -> infinity;;

let sr_wartosc x = 
    match min_wartosc x,max_wartosc x with
    | _, nan -> Float.nan
    | nan, _ -> Float.nan
    | a,b -> (a +. b)/. 2.0;;