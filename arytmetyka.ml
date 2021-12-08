(* Zadanie: Arytmetyka 		Autor: Maria Nazarczuk 		                        			*)

type wartosc = 							 		       (* Typ wartosc trzyma nam            *)
	Interval of float * float | 		 			   (* - przedziały [x, y]               *)
	Complement of float * float | 		 		       (* - dopełniania (-oo, x] u [y, +oo) *)
	Empty											   (* - zniór pusty                     *)
;;

(* ---------------------------------- FUNKCJE POMOCNICZE ---------------------------------- *)

let real = 												   (* przedział liczb rzeczywistych *)
	Interval (neg_infinity, infinity)
;;

let absolute (x : float) = 								     (* funkcja obliczająca wartość *)
	if x < 0.0 then -. x											(* bezwzględną z liczby *)
	else x 
;;

(* ------------------------------------- KONSTRUKTORY ------------------------------------- *)

let wartosc_dokladnosc x p = 
	let inaccuracy x p = (absolute x) *. p /. 100.0 in 					   (* niedokładność *)
	Interval (x -. inaccuracy x p, x +. inaccuracy x p )
;;


let wartosc_od_do x y = 
	Interval (x, y)
;;

let wartosc_dokladna x =
	Interval (x, x)
;;

let wartosc_dopelnienie x y = 								(* funkcja tworząca dopełnienie *)
	if x < y then Complement (x, y)
	else real
;;

(* --------------------------------------- SELEKTORY -------------------------------------- *)
	
let in_wartosc x y =
	match x with 
		| Empty -> false 
		| Interval (a, b) -> a <= y && b >= y 
	  	| Complement (a, b) -> y <= a || y >= b 
;;

let min_wartosc x = 
	match x with 
	    | Empty -> (*infinity*) nan 
	  	| Interval (a, _) -> a 
	  	| Complement (_, _) -> neg_infinity
;;

let max_wartosc x =
	match x with
	    | Empty -> (*neg_infinity*) nan 
	  	| Interval (_, b) -> b 
	  	| Complement (_, _) -> infinity
;;

let sr_wartosc x = 
	match x with 
	    | Interval (a, b) -> (a +. b) /. 2.0 
	    | _ -> nan
;;

(* -------------------------------------- MODYFIKATORY ------------------------------------ *)

let plus x y = 
	let pom x y =
	    match x , y with
	        | Interval (a, b) , Interval (c, d) -> wartosc_od_do (a +. c) (b +. d) 
	        | Interval (a, b) , Complement (c, d) -> wartosc_dopelnienie (c +. b) (d +. a) 
	        | Complement (_, _) , Complement (_, _) -> real 
	        | _ -> Empty 
	in 
	match x with
	    | Complement (_, _) -> pom y x 
	    | _ -> pom x y									     
;;

let minus x y = 
	let opposite x = 									  (* zwraca zbiór liczb przeciwnych *)
	match x with 
	    | Empty -> Empty 
	    | Interval (a, b) -> Interval (-. b, -. a) 
	    | Complement (a, b) -> Complement (-. b, -. a) 
	in 
	plus x (opposite y)
;;

let razy x y = 
	let pom x y = 
	    match x , y with
	        | Interval (a, b) , Interval (c, d) -> 
	            if (a = 0.0 && b = 0.0) || (c = 0.0 && d = 0.0) then Interval (0.0, 0.0)
	            else if x = real || y = real then real
	            else 
	            	let minmax funkcja p q r s = (* zwraca min lub max z czterech elementów *)
			            let p = if (classify_float p) = FP_nan then q else p in
			            let q = if (classify_float q) = FP_nan then p else q in
			            let r = if (classify_float r) = FP_nan then s else r in
			            let s = if (classify_float s) = FP_nan then r else s in
			            funkcja p (funkcja q (funkcja r s))
	            	in  		(* jeśli a = 0.0 oraz b = infinity lub b = neg_infinity to  *)
	            		  (* a *. b = nan zatem p albo q może być nan-em oraz r albo s może *)
	            			 (* być nan-em zamieniamy więc dany nan na drugi element z pary *)
	               wartosc_od_do (minmax min (a *. c) (a *. d) (b *. c) (b *. d))
	                    		 (minmax max (a *. c) (a *. d) (b *. c) (b *. d))
			| Interval (a, b) , Complement (c, d) -> 
	            if a = 0.0 && b = 0.0 then Interval (0.0, 0.0)
	            else if a <= 0.0 && b >= 0.0 then real 
	            else if a > 0.0 then 
	                if c <= 0.0 && d >= 0.0 then wartosc_dopelnienie (a *. c) (a *. d)
	                else if c > 0.0 && d > 0.0 then wartosc_dopelnienie (b *. c) (a *. d)
	                else wartosc_dopelnienie (a *. c) (b *. d)
	            else 
	                if c <= 0.0 && d >= 0.0 then wartosc_dopelnienie (b *. d) (b *. c)
	                else if c > 0.0 && d > 0.0 then wartosc_dopelnienie (b *. d) (a *. c)
	                else wartosc_dopelnienie (a *. d) (b *. c)
			| Complement (a, b) , Complement (c, d) ->         
	            if a > 0.0 || b < 0.0 || c > 0.0 || d < 0.0 then real
	            else wartosc_dopelnienie (max (b *. c) (a *. d)) (min (a *. c) (b *. d))  
			| _ -> Empty
	in 
	match x with 
	    | Complement (_, _) -> pom y x 
	  	| _ -> pom x y
;;   
	   
let podzielic x y = 
	let inverse x = 									   (* zwraca zbiór liczb odwrotnych *)
		match x with 
			| Empty -> Empty 
			| Interval (a, b) -> 
			    if a = 0.0 && b = 0.0 then Empty
			    else if a = 0.0 then wartosc_od_do (1.0 /. b) infinity
			    else if b = 0.0 then wartosc_od_do neg_infinity (1.0 /. a)
			    else if a < 0.0 && b > 0.0 then wartosc_dopelnienie (1.0 /. a) (1.0 /. b)
			    else wartosc_od_do (1.0 /. b) (1.0 /. a)
			| Complement (a, b) -> 
			    if a = 0.0 then wartosc_od_do neg_infinity (1.0 /. b)
			    else if b = 0.0 then wartosc_od_do (1.0 /. a) infinity
			    else if a < 0.0 && b > 0.0 then wartosc_od_do (1.0 /. a) (1.0 /. b)
			    else wartosc_dopelnienie (1.0 /. b) (1.0 /. a)
	in 
	razy x (inverse y)
;;

