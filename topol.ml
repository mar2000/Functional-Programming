(* Zadanie: Sortowanie topologiczne   Autor: Marysia Nazarczuk                              *)

open PMap

type top = Before | During | After                         (* stan odwiedzenia wierzchołków *)

exception Cykliczne	                                                             (* wyjątek *)

let topol lista =
    let make_a_graph lista =                                             (* tworzenie grafu *)
        let a_to_int = ref empty in
        let int_to_a = ref [] in
        let i = ref 0 in                                    (* liczba wierzchołków w grafie *)
        let add v =                                                (* dodawanie wierzchołka *)
            if not (mem v !a_to_int) then
            begin
                a_to_int := add v !i !a_to_int;
                int_to_a := v :: (!int_to_a);
                i := !i + 1
            end
        in
        let pom1 (v, l) = add v; List.iter add l in               (* dodawanie wierzchołków *)
        List.iter pom1 lista;
        let graph = Array.make !i [] in
        let pom2 (v, l) =                                             (* konstuowanie grafu *)
            let int_v = find v !a_to_int in
            let int_l = List.map (fun a -> find a !a_to_int) l in
            graph.(int_v) <- int_l @ graph.(int_v)
        in
        List.iter pom2 lista;
        (graph, Array.of_list (List.rev !int_to_a))
    in
    let (graph, int_to_a) = make_a_graph lista in
    let n = Array.length graph in                                    (* liczba wierzchołków *)
    let visited = Array.make n Before in                          (* odwiedzone wierzchołki *)
    let stack = Stack.create () in                    (* stos wierzchołków zawierający stan *)
    let result = ref [] in                                        (* lista trzymająca wynik *)
    let found = ref false in
    for i = 0 to n - 1 do
        if visited.(i) = Before then Stack.push (i, During) stack;
        while not (Stack.is_empty stack || !found) do
            let (v, q) = Stack.pop stack in
            let l = graph.(v) in                                              (* sąsiedzi v *)
            if q = After then                   (* jeśli całe poddrzewo zostało odwiedzione *)
            begin
                result := v :: (!result);
                visited.(v) <- After
            end
            else if visited.(v) = Before then      (* else to wywołujemy DFS-a dla sąsiadów *)
            begin
                Stack.push (v, After) stack;
                visited.(v) <- During;
                let pom u =
                    if visited.(u) = Before then Stack.push (u, During) stack
                    else if visited.(u) = During then found := true     (* znaleźliśmy cykl *)
                in
                List.iter pom l
            end
        done
    done;
    if !found then raise Cykliczne
    else List.map (fun x -> int_to_a.(x)) !result
    
(* Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] zwraca liste, na     *)
(* ktorej kazdy z elementow a_i oraz a_ij wystepuje dokladnie raz i ktora jest              *)
(* uporzadkowana w taki sposob, ze kazdy element a_i jest przed kazdym z elementow          *)
(* a_i1 ... a_il                                                                            *)

