(* Zadanie: Origami              Autor: Maria Nazarczuk                                     *)

type point = float * float                                         (* Punkt na płaszczyźnie *)

type kartka = point -> int                                             (* poskładana kartka *)

let epsilon = 1e-8                                                         (* niedokładność *)

let prostokat (a, b) (c, d) =                   (* sprawdzamy, czy punkt (x, y) znajduje    *)
    function (x, y) ->                          (* w prostokącie o lewym dolnym wierzchołku *)
        if x +. epsilon >= a &&                 (* w (a, b) i prawym górnym w (c, d), czyli *)
           x -. epsilon <= c && 				(* czy spełnione są odpowiednie nierówności *)
           y +. epsilon >= b && 
           y -. epsilon <= d 
        then 1
        else 0


let kolko (a, b) r =                            (* punkt (x, y) leży wewnątrz koła o środku *)
    function (x, y) -> 
        let p1 = (a -. x) /. r in                (* w (a, b) i promieniu r, jeśli spełnione *)
        let p2 = (b -. y) /. r in                (* jest (a-x)^2 + (b-y)^2 <= r^2, dzielimy *)
        if p1 *. p1 +. p2 *. p2 <= 1. then 1     (* przez r^2 aby nie wyjść poza max        *)
        else 0

let zloz p1 p2 k =                                                       (* złożenie kartki *)
    let plus (a, b) (c, d) = (a +. c, b +. d) in                      (* dodawanie wektorów *)
    let minus (a, b) (c, d) = (a -. c, b -. d) in                   (* odejmowanie wektorów *)
    function x ->
        let (a, b) = minus p2 p1 in
        let (c, d) = minus x p1 in
        let wek = a *. d -. b *. c in                                  (* iloczyn wektorowy *)
        if wek >= -. epsilon && wek <= epsilon then k x
        else if wek > 0. then
            let ska (a, b) (c, d) = a *. c +. b *. d in                 (* iloczyn skalarny *)
            let razy c (a, b) = (c *. a, c *. b) in        (* mnożenie wektora przez skalar *)
            let pom = (ska (minus p2 p1) (minus x p1)) /. (ska (minus p2 p1) (minus p2 p1)) in
            let x_prim = minus (razy 2. (plus p1 (razy pom (minus p2 p1)))) x in
            k x + k x_prim
        else 0

(* Jeśli punkt x leży na zagięciu kartki, to liczymy go raz. Jeśli punkt x leży na lewo od  *)
(* prostej przechodzącej przez punkty p1 i p2, to punkt x dbijamy sypemtrycznie względem    *)
(* prostej przechodzącej przez punkty p1 i p2 otrzymyjąc punkt x'. Jeśli punkt leży na      *)
(* od prostej przechodzącej przez punkty p1 i p2, to mamy 0                                 *)

let skladaj lista k =                                               (* multizłożenie kartki *)
    List.fold_left (fun k (p1, p2) -> zloz p1 p2 k) k lista

