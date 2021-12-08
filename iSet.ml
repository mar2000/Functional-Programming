(* Zadanie: Modyfikacja drzew     Autor: Maria Nazarczuk                                     *)
                                                                                            
type t =                  (* Node trzyma nam: lewy syn, przedział, prawy syn, wysokość, suma *)
    | Node of {left : t; interval : int * int; right : t; height : int; sum : int }
    | Empty
    
let empty = Empty                                                            (* puste drzewo *)

let is_empty x =                                           (* sprawdza czy drzewo jest puste *)
    x = Empty

let height = function                                              (* zwraca wysokość drzewa *)
    | Node {height = h} -> h
    | Empty -> 0

let sum = function                                          (* zwraca sumę dla danego drzewa *)
    | Node {sum = s} -> s
    | Empty -> 0

let check x =                                   (* zabezpieczenie przed wyjściem poza zakres *)
    if x >= 0 then x 
    else max_int

let make l (a, b) r =    (* łączy wyważone drzewa wraz z przedziałem o różnicy wysokości < 3 *)
    Node {left = l; interval = (a, b); right = r; height = ( max (height l) (height r) ) + 1;
          sum = check (check (b-a) + 1 + sum l + sum r)}

let bal l k r =                                  (* łączy wyważone drzewa wraz z przedziałem *)
    let hl = height l in                         (* o różnicy wysokości <= 3                 *)
    let hr = height r in 
    if hl > hr + 2 then 
        match l with
            | Node {left = ll; interval = lk; right = lr} ->
                if height ll >= height lr then make ll lk (make lr k r)
                else (match lr with
                        | Node {left = lrl; interval = lrk; right = lrr} -> 
                            make (make ll lk lrl) lrk (make lrr k r)
                        | Empty -> assert false)
            | Empty -> assert false     
    else if hr > hl + 2 then 
        match r with
            | Node {left = rl; interval = rk; right = rr} ->
                if height rr >= height rl then make (make l k rl) rk rr
                else (match rl with
                        | Node {left = rll; interval = rlk; right = rlr} ->
                            make (make l k rll) rlk (make rlr rk rr)
                        | Empty -> assert false)
            | Empty -> assert false             
    else make l k r 

let rec min_elt = function                         (* zwraca najmniejszy przedział w drzewie *)
    | Node {left = Empty; interval = k} -> k
    | Node {left = l} -> min_elt l
    | Empty -> raise Not_found

let rec remove_min_elt = function                    (* usuwa najmniejszy przedział z drzewa *)
    | Node {left = Empty; right = r} -> r
    | Node {left = l; interval = k; right = r} -> bal (remove_min_elt l) k r
    | Empty -> invalid_arg "ISet.remove_min_elt"

let cmp (a, b) c =                              (* patrzy na położenie c w przedziale (a, b) *)
    if c > b then 1
    else if c < a then -1
    else 0
    
let rec add_one (a, b) set =                                (* dodaje przedział, który jest  *)
    if b < a then set                                       (* rozłączny z pozostałymi       *)
    else match set with
        | Node {left = l; interval = k; right = r} ->
            let pom =                                              (* porównyjemy przedziały *)
                let (c, d) = k in
                if a = c then 
                    if b = d then 0
                    else if b > d then 1
                    else -1
                else if a > c then 1 
                else -1 
            in
            if pom = 0 then make l (a, b) r
            else if pom < 0 then let nl = add_one (a, b) l in bal nl k r
            else let nr = add_one (a, b) r in bal l k nr
        | Empty -> make Empty (a, b) Empty

let rec join l k r =        (* łączy wyważone drzewa wraz z przedziałem o dowolnej wysokości *)
    match l , r with
        | Node {left = ll; interval = lk; right = lr; height = lh} , 
          Node {left = rl; interval = rk; right = rr; height = rh} ->
              if lh > rh + 2 then bal ll lk (join lr k r)
              else if rh > lh + 2 then bal (join l k rl) rk rr
              else make l k r
        | Empty , _ -> add_one k r 
        | _ , Empty -> add_one k l

let split x set =                                            (* zwraca trójkę [(l, pres, r)] *)
    let rec loop x = function
        | Node {left = l; interval = (a, b); right = r} -> 
            let pom = cmp (a, b) x in 
            let pom1 x = if x = min_int then min_int else x-1 in
            let pom2 x = if x = max_int then max_int else x+1 in
            if pom = 0 then (add_one (a, pom1 x) l, true, add_one (pom2 x, b) r)
            else if pom < 0 then 
                let (ll, pres, rl) = loop x l in (ll, pres, join rl (a, b) r)
            else let (lr, pres, rr) = loop x r in (join l (a, b) lr, pres, rr)
        | Empty -> (Empty, false, Empty)
    in 
    loop x set 

let split_vol2 x set =                                    (* zwraca parę (a, b), gdzie a to  *)
    let rec loop x = function                             (* trójka jak w split, natomiast b *)
        | Node {left = l; interval = k; right = r} ->     (* to przedział do którego należał *)
            let pom = cmp k x in                          (* x, gdy x nie należał do żadnego *)
            if pom = 0 then ((l, true, r), k)             (* przedziału, zwraca (max, min)   *)
            else if pom < 0 then 
                let ((ll, pres, rl), q) = loop x l in ((ll, pres, join rl k r), q)
            else let ((lr, pres, rr), q) = loop x r in ((join l k lr, pres, rr), q)
        | Empty -> ((Empty, false, Empty), (max_int, min_int))
    in
    loop x set

let add (a, b) set =                          (* dodaje do drzewa liczby z przedziału (a, b) *)
    let pom1 x = if x = min_int then min_int else x-1 in
    let pom2 x = if x = max_int then max_int else x+1 in
    let ((l, _, r), (minl, maxl)) = split_vol2 (pom1 a) set in 
    let ((_, _, r), (_, maxr)) = split_vol2 (pom2 b) r in
    join l (min minl a, max (max maxl maxr) b) r 

let remove (a, b) set =                         (* usuwa liczby z przedziału (a, b) z drzewa *)
    let (l, _, r) = split a set in                       (* drzewo elementów mniejszych od a *)
    let (_, _, r) = split b r in                         (* drzewo elementów większych od b  *)
    if r = Empty then l                                            
    else let pom = min_elt r in join l pom (remove_min_elt r)       (* łączymy te dwa drzewa *)
    
let mem x set =                        (* sprawdza, czy liczba należy do jakiegoś przedziału *)
    let rec loop = function
        | Node {left = l; interval = (a, b); right = r} -> 
            let pom = cmp (a, b) x in 
            pom = 0 || loop (if pom < 0 then l else r)
        | Empty -> false 
    in
    loop set 

let iter f set =                            (* aplikuje funkcję f na wszystkich przedziałach *)
    let rec loop = function
        | Node {left = l; interval = k; right = r} -> loop l; f k; loop r
        | Empty -> ()
    in 
    loop set 

let fold f set acc =                      (* oblicza [(f xN ... (f x2 (f x1 a))...)], gdzie  *)
    let rec loop acc = function           (* x1 ... xN są przedziałami w kolejności rosnącej *)
        | Node {left = l; interval = k; right = r} -> loop (f k (loop acc l)) r 
        | Empty -> acc
    in 
    loop acc set 
    
let elements set =                           (* zwraca listę zawierającą przedziały z drzewa *)
    let rec loop acc = function
        | Node {left = l; interval = k; right = r} -> loop (k::(loop acc r)) l    
        | Empty -> acc
    in
    loop [] set

let below x set =                        (* zwraca liczbę liczb niewiększych niż x w drzewie *)
    let rec loop = function
        | Node {left = l; interval = (a, b); right = r} ->
            let pom = cmp (a, b) x in 
            if pom = 0 then check (check (x-a) + sum l + 1)
            else if pom > 0 then check (check (b-a) + loop r + sum l + 1)
            else loop l
        | Empty -> 0
    in 
    loop set 
    


(* max_int = 4611686018427387903  *)
(* min_int = -4611686018427387904 *)
