(* Zadanie: Drzewa Lewicowe		Autor: Maria Nazarczuk	                             		*)

type 'a queue =                                           (* Typ a' queue trzyma nam:       *)
    Tree of 'a * ('a queue) * ('a queue) * int |          (* - wartość, synów oraz wysokość *)
    Null                                                  (* - puste drzewo   *)
;;

(* ------------------------------------- KONSTRUKTORY ------------------------------------- *)

let empty =                                            (* tworzy pustą kolejkę priorytetową *)
    Null
;;

(* --------------------------------------- SELEKTORY -------------------------------------- *)

let is_empty q =                                         (* sprawdza czy kolejka jest pusta *)
    match q with
        | Null -> true
        | _ -> false 
;; 

(* -------------------------------------- MODYFIKATORY ------------------------------------ *)

let rec join q1 q2 =                                               (* łączy kolejki q1 i q2 *)
    let height x =                                             (* zwraca wysokość poddrzewa *)
        match x with
            | Null -> 0
            | Tree (_, _, _, h) -> h
    in
    let rotate x =                                 (* zamienia poddrzewa miejscami jeśli to *)
        match x with                               (* potrzebne, tak aby powstało poprawne  *)
            | Null -> Null                         (* drzewo lewicowe                       *)
            | Tree (a, left, right, _) ->
                if height left < height right then 
                     Tree (a, right, left, (height left + 1))
                else 
                     Tree (a, left, right, (height right + 1))
    in
    match q1 , q2 with
        | _ , Null -> q1
        | Null , _ -> q2
        | Tree (a, left, right, _) , Tree (b, _, _, _) ->
            if a <= b then rotate (Tree (a, left, (join right q2), -1))
            else join q2 q1
;;

let add e q =                                 (* dołącza element e do kolejki q czyli łączy *)
    join q (Tree (e, Null, Null, 1))          (* kolejkę q z drzewen zawierającym tylko e   *)
;;

exception Empty             (* Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)

let delete_min q =                                   (* usuwa element minimalny e z kolejki *)
    match q with                                     (* oraz zwraca e oraz powstałą kolejkę *)
        | Null -> raise Empty
        | Tree (e, left, right, _) -> e , join left right
;;

