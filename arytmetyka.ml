type wartosc = 
    | Przedzial of float * float
    | Dopelnienie of float*float
    | Przedzial_pusty
    | Nie_liczba of float;;

(*
wartosc - główny typ danych

Przedzial - typ dla pojedyńczych przedziałów np. [1;192] , [-inf;+inf] [-inf;0]

Dopelnienie - typ dla sumy dwóch przedziałów np. Depelnienie [1;2] = [-inf;1] U [2;+inf],
[-100;0]= [-inf;-100] U [0;+inf]

Przedzial_pusty = [] dla 0.0/0.0

Nie_liczba - typ dla nieokreślonych wyników np. sr_wartosc [-inf,inf]
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
    | Nie_liczba p -> false
    | Przedzial_pusty -> false
    | Przedzial (a,b) -> y>=a && y<=b
    | Dopelnienie (a,b) -> y<=a || y>=b;;

let plus x y = 

    match x, y with
    (* x lub y jest NaN *)
    | Nie_liczba p,_ -> Nie_liczba nan
    | _,Nie_liczba p -> Nie_liczba nan

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

    | Dopelnienie(a,b), Dopelnienie(k,l) -> Przedzial(neg_infinity,infinity)

    (* Nan już rozpatrzone *)
    | cokolwiek,Przedzial_pusty -> Przedzial_pusty
    | Przedzial_pusty, cokolowiek -> Przedzial_pusty;;


let min_wartosc x = 
    match x with
    | Nie_liczba p ->  Float.nan
    | Przedzial_pusty -> infinity
    | Przedzial (a,b) -> a
    | Dopelnienie (a,b) -> neg_infinity;;

let max_wartosc x = 
    match x with
    | Nie_liczba p -> Float.nan
    | Przedzial_pusty -> neg_infinity
    | Przedzial (a,b) -> b
    | Dopelnienie (a,b) -> infinity;;

let sr_wartosc x = 
    let min_x,max_x = min_wartosc x,max_wartosc x in
    match classify_float min_x, classify_float max_x with

    | FP_nan, _ -> Float.nan
    | _, FP_nan -> Float.nan

    | FP_infinite, _ -> Float.nan
    | _,FP_infinite -> Float.nan

    | _,_ -> (min_x +. max_x) /. 2.;;


let rec razy x y = 
    match x, y with
    | Nie_liczba _,_ -> Nie_liczba Float.nan
    | _,Nie_liczba _ -> Nie_liczba Float.nan
    | _,Przedzial_pusty -> Przedzial_pusty
    | Przedzial_pusty,_ -> Przedzial_pusty
    | Przedzial(a,b),Przedzial(k,l) -> Przedzial((min (a*. k) (min (a*. l) (min (b*. k) (b*. l)))),(max (a*. k) (max (a*. l) (max (b*. k) (b*. l)))))
    | Przedzial(a,b),Dopelnienie(k,l) ->
        let Przedzial(pom_pocz_1,pom_kon_1), Przedzial(pom_pocz_2,pom_kon_2) = (razy (Przedzial(a,b)) (Przedzial(neg_infinity,k)) , (razy (Przedzial(a,b)) (Przedzial(l,infinity)))) in
            let pocz =  pom_kon_1 in
            let kon = pom_pocz_2 in
            if pocz>= kon then Przedzial(neg_infinity,infinity) else Dopelnienie(pocz,kon)
    | Dopelnienie(k,l),Przedzial(a,b) -> 
        let Przedzial(pom_pocz_1,pom_kon_1), Przedzial(pom_pocz_2,pom_kon_2) = (razy (Przedzial(a,b)) (Przedzial(neg_infinity,k)) , (razy (Przedzial(a,b)) (Przedzial(l,infinity)))) in
            let pocz =  pom_kon_1 in
            let kon = pom_pocz_2 in
            if pocz>= kon then Przedzial(neg_infinity,infinity) else Dopelnienie(pocz,kon)
    | Dopelnienie(k,l),Dopelnienie(a,b) -> 
        let Przedzial(pom_pocz_1,pom_kon_1), Przedzial(pom_pocz_2,pom_kon_2) = (razy (Przedzial(neg_infinity,k)) (Dopelnienie(a,b)) , (razy (Przedzial(l,infinity)) (Dopelnienie(a,b)))) in
        if pom_pocz_1 < 0. && pom_kon_2 > 0. then if pom_kon_1 >= pom_pocz_2 then Przedzial(neg_infinity,infinity) else Dopelnienie(pom_kon_1,pom_kon_2) else
        if pom_kon_2 >= pom_pocz_1 then Przedzial(neg_infinity,infinity) else Dopelnienie(pom_kon_2,pom_kon_1);;


let minus x y = 
    plus x (razy y (Przedzial(-1.0,-1.0)));;


let podzielic x y = 
    match x, y with
    | Nie_liczba _,_ -> Nie_liczba Float.nan
    | _,Nie_liczba _ -> Nie_liczba Float.nan
    | _,Przedzial_pusty -> Przedzial_pusty
    | Przedzial_pusty,_ -> Przedzial_pusty
    | _,Przedzial(0.0,0.0) -> Nie_liczba Float.nan
    | _,Przedzial(a,b) -> if (a *. b) < 0.0 then razy x (Dopelnienie(1.0 /. a,1.0 /. b)) else razy x (Przedzial(1.0 /. a,1.0 /. b)) 
    | _,Dopelnienie(a,b) -> if (a *. b) < 0.0 then razy x (Przedzial(1.0 /. a, 1.0 /. b)) else razy x (Przedzial(neg_infinity,infinity));;
