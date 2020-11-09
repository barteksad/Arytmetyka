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
    let poczatek_przedzialu = x -. abs_float x *. p /. 100. in
    let koniec_przedzialu = x +. abs_float x *. p /. 100. in
    Przedzial (poczatek_przedzialu,koniec_przedzialu);;

let wartosc_od_do x y = 
    Przedzial (x,y);;

let wartosc_dokladna x = 
    Przedzial (x,x);;

let in_wartosc x y = 
    match x with
    | Nie_liczba p -> false
    | Przedzial_pusty -> false
    | Przedzial (a,b) -> y>=(a -. Float.epsilon) && y<=(b +. Float.epsilon)
    | Dopelnienie (a,b) -> y<=(a +. Float.epsilon) || y>=(b -. Float.epsilon);;

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
        if b+.k  >= l && a +. l <= k then Przedzial (neg_infinity,infinity) else
        Dopelnienie (b+.k,a +. l)

    | Dopelnienie(k,l),Przedzial(a,b) ->
        if classify_float a = FP_infinite || classify_float b = FP_infinite then Przedzial(neg_infinity,infinity) else
        if b+.k  >= l && a +. l <= k then Przedzial (neg_infinity,infinity) else
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

    | FP_infinite, FP_infinite -> Float.nan
    | _,FP_infinite -> infinity
    | FP_infinite,_ -> neg_infinity

    | _,_ -> (min_x +. max_x) /. 2.;;


let rec razy x y = 
    match x, y with
    | Nie_liczba _,_ -> Nie_liczba Float.nan
    | _,Nie_liczba _ -> Nie_liczba Float.nan
    | _,Przedzial_pusty -> Przedzial_pusty
    | Przedzial_pusty,_ -> Przedzial_pusty
    | Przedzial(0.0,0.0),_-> Przedzial(Float.zero,Float.zero)
    | _, Przedzial(0.0,0.0) ->  Przedzial(Float.zero,Float.zero)

    (* | Przedzial(neg_infinity,infinity), Przedzial(0.0,b) -> Przedzial(neg_infinity,infinity)
    | Przedzial(0.0,b), Przedzial(neg_infinity,infinity) -> Przedzial(neg_infinity,infinity)
    | Przedzial(neg_infinity,infinity), Przedzial(b,0.0) -> Przedzial(neg_infinity,infinity)
    | Przedzial(b,0.0), Przedzial(neg_infinity,infinity) -> Przedzial(neg_infinity,infinity) *)

    | Przedzial(a,b),Przedzial(k,l) -> 
        (* let a = if a = neg_infinity then Float.succ neg_infinity else a in
        let b = if b = infinity then Float.pred infinity else b in
        let k = if k = neg_infinity then Float.succ neg_infinity else k in
        let l = if l = infinity then Float.pred infinity else l in *)
        let mul x1 x2 = if (classify_float x1 = FP_infinite && x2 = 0.0) ||  (classify_float x2 = FP_infinite && x1 = 0.0) then 0.0 else (x1 *. x2) in
        if b < Float.zero && l < Float.zero then Przedzial(  mul b l,  mul a k) else
        if a > Float.zero && k > Float.zero then Przedzial(  mul a  k, mul b l) else
        if b < Float.zero && k > Float.zero then Przedzial(  mul l  a,  mul b  k) else
        if a > Float.zero && l < Float.zero then Przedzial(  mul k  b, mul a  l) else
        let pocz,kon = (min (mul a  k) (min (mul a   l) (min ( mul b  k) ( mul b  l)))),(max ( mul a  k) (max ( mul a  l) (max ( mul b  k) ( mul b  l)))) in
        if in_wartosc (Przedzial(pocz,pocz)) 0.0 then Przedzial(0.0,kon) else if in_wartosc (Przedzial(kon,kon)) 0.0 then Przedzial(pocz,0.0) else Przedzial(pocz,kon)


    | Przedzial(a,b),Dopelnienie(k,l) ->
        let Przedzial(pom_pocz_1,pom_kon_1), Przedzial(pom_pocz_2,pom_kon_2) = (razy (Przedzial(a,b)) (Przedzial(neg_infinity,k)) , (razy (Przedzial(a,b)) (Przedzial(l,infinity)))) in
            if pom_pocz_1 = neg_infinity && pom_kon_2 = infinity then if pom_kon_1 > pom_pocz_2 then Przedzial(neg_infinity,infinity) else Dopelnienie(pom_kon_1,pom_pocz_2) else
            if pom_kon_1 < pom_pocz_2 then Przedzial(neg_infinity,infinity) else Dopelnienie(pom_kon_2,pom_pocz_1)

    | Dopelnienie(k,l),Przedzial(a,b) -> 

        let Przedzial(pom_pocz_1,pom_kon_1), Przedzial(pom_pocz_2,pom_kon_2) = (razy (Przedzial(a,b)) (Przedzial(neg_infinity,k)) , (razy (Przedzial(a,b)) (Przedzial(l,infinity)))) in
            if pom_pocz_1 = neg_infinity && pom_kon_2 = infinity then if pom_kon_1 > pom_pocz_2 then Przedzial(neg_infinity,infinity) else Dopelnienie(pom_kon_1,pom_pocz_2) else
            if pom_pocz_2 = neg_infinity && pom_kon_1 = infinity then if pom_pocz_1 < pom_kon_2 then Przedzial(neg_infinity,infinity) else Dopelnienie(pom_kon_2,pom_pocz_1) else
            if pom_kon_1 < pom_pocz_2 then Przedzial(neg_infinity,infinity) else Dopelnienie(pom_kon_2,pom_pocz_1)






    | Dopelnienie(k,l),Dopelnienie(a,b) -> 
        let wynik1, wynik2 = (razy (Przedzial(neg_infinity,k)) (Dopelnienie(a,b)) , (razy (Przedzial(l,infinity)) (Dopelnienie(a,b)))) in
        match wynik1,wynik2 with
        | Przedzial(pom_pocz_1,pom_kon_1),Przedzial(pom_pocz_2,pom_kon_2) ->
            if pom_pocz_1 < 0. && pom_kon_2 > 0. then if pom_kon_1 >= pom_pocz_2 then Przedzial(neg_infinity,infinity) else Dopelnienie(pom_kon_1,pom_kon_2) else
            if pom_kon_2 >= pom_pocz_1 then Przedzial(neg_infinity,infinity) else Dopelnienie(pom_kon_2,pom_kon_1)
        | Przedzial(pom_pocz_1,pom_kon_1), Dopelnienie(pom_pocz_2,pom_kon_2) ->
            if pom_pocz_1 <= pom_pocz_2 then if pom_kon_1>= pom_kon_2 then Przedzial(neg_infinity,infinity) else Dopelnienie(pom_kon_1,pom_kon_2) else Dopelnienie(pom_pocz_2,pom_pocz_1)
        | Dopelnienie(pom_pocz_2,pom_kon_2),Przedzial(pom_pocz_1,pom_kon_1) ->
            if pom_pocz_1 <= pom_pocz_2 then if pom_kon_1>= pom_kon_2 then Przedzial(neg_infinity,infinity) else Dopelnienie(pom_kon_1,pom_kon_2) else Dopelnienie(pom_pocz_2,pom_pocz_1)
        | Dopelnienie(pom_pocz_1,pom_kon_1),Dopelnienie(pom_pocz_2,pom_kon_2) ->
            let nowy_pocz,nowy_kon = max pom_pocz_1 pom_pocz_2, min pom_kon_1 pom_kon_2 in
            if nowy_pocz>=nowy_kon then Przedzial(neg_infinity,infinity) else Dopelnienie(nowy_pocz,nowy_kon);;

        (* let Przedzial(pom_pocz_1,pom_kon_1), Przedzial(pom_pocz_2,pom_kon_2) = (razy (Przedzial(neg_infinity,k)) (Dopelnienie(a,b)) , (razy (Przedzial(l,infinity)) (Dopelnienie(a,b)))) in
        if pom_pocz_1 < 0. && pom_kon_2 > 0. then if pom_kon_1 >= pom_pocz_2 then Przedzial(neg_infinity,infinity) else Dopelnienie(pom_kon_1,pom_kon_2) else
        if pom_kon_2 >= pom_pocz_1 then Przedzial(neg_infinity,infinity) else Dopelnienie(pom_kon_2,pom_kon_1);; *)


let minus x y = 
    plus x (razy y (Przedzial(-1.0,-1.0)));;


let podzielic x y = 
    match x, y with
    | Nie_liczba _,_ -> Nie_liczba Float.nan
    | _,Nie_liczba _ -> Nie_liczba Float.nan
    | _,Przedzial_pusty -> Przedzial_pusty
    | Przedzial_pusty,_ -> Przedzial_pusty
    | _,Przedzial(0.0,0.0) -> Nie_liczba Float.nan
    | _,Przedzial(0.0,a) -> if a = infinity then razy x (Przedzial((Float.succ Float.zero),infinity)) else razy x (Przedzial(1. /. a,infinity))
    | _,Przedzial(a,0.0) -> if a = neg_infinity then razy x (Przedzial(neg_infinity,(Float.pred Float.zero))) else razy x (Przedzial(neg_infinity,1. /.a))
    | _,Przedzial(a,b) -> 
        let pocz = if a = neg_infinity then (Float.pred Float.zero) else 1.0 /. a in
        let kon = if b = infinity then (Float.succ Float.zero) else 1.0 /. b in
        if a < 0. && b > 0. then
        razy x (Dopelnienie(pocz,kon)) else razy x (Przedzial(kon,pocz))

    (* | _,Przedzial(a,b) -> razy x (Przedzial(1.0 /. a,1.0 /. b))  *)
    | _,Dopelnienie(a,b) ->
        (* if a < 0.0 && b > 0.0 then razy x (Przedzial(a,b)) else Dopelnienie(1. /. b, 1./. a);; *)
        if a < 0.0 && b > 0.0 then razy x (Przedzial(1./. a ,1. /. b)) else razy x (Dopelnienie(1. /. b, 1./. a));;
