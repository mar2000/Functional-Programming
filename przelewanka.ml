(* Zadanie: Przelewanka      Autor: Marysia Nazarczuk                                       *)

let przelewanka table =
    let rec nwd x y =
        if x = 0 then y
        else if x > y then nwd y x  
        else nwd (y mod x) x
    in
    if table = [||] then 0
    else
    let table_fst = Array.map fst table in                           (* rozdzielamy tablice *)
    let table_snd = Array.map snd table in
    let gcd = Array.fold_left nwd table_fst.(0) table_fst in
    if Array.fold_left nwd gcd table_snd < gcd then -1
    else
    if not (Array.fold_left                    (* czy nie istnieje pusta szklanka na koniec *)
       (fun a (x, y) -> a || x = y || y = 0)
       false table) then -1 
    else
    if Array.fold_left                                 (* szklanki mają być puste lub pełne *)
       (fun a (x, y) -> a && (x = y || y = 0)) 
       true table then Array.fold_left 
       (fun a x -> a + if x > 0 then 1 else 0) 0 
       table_snd
    else             (* chcemy wlać do jednej szklanki i istnieje inna odpowiednia szklanka *)
    let l = Array.length table in
    let sorted = Array.copy table_snd in
    Array.sort (fun a b -> if a > b then -1 else if a < b then 1 else 0)
    sorted;
    if l > 1 && sorted.(0) > 0 && sorted.(1) = 0 && Array.mem sorted.(0) table_fst then 2
    else                                                      (* bfs w przeciwnym przypadku *)
    let hash_table = Hashtbl.create 1000009 in
    let queue = Queue.create () in
    let ans = ref (-1) in
    let add new_state time =                 (* dodajemy jeśli nie był jeszcze rozpatrywany *)
        if not (Hashtbl.mem hash_table new_state) then
        begin
            if new_state = table_snd then ans := time;
            Queue.push (new_state, time) queue;
            Hashtbl.add hash_table new_state ()
        end
    in
    add (Array.make l 0) 0;
    while not (Queue.is_empty queue) && !ans = -1
    do
        let (state, time) = Queue.take queue in 
        for i = 0 to l - 1 do                          (* przelewamy wode między szklankami *)
            for j = 0 to l - 1 do
                if i <> j then
                begin
                    let after = Array.copy state in
                    if state.(i) > table_fst.(j) - state.(j) then    (* można przelać tylko *)
                    begin                                            (* część               *)
                        after.(i) <- state.(i) + state.(j) - table_fst.(j);
                        after.(j) <- table_fst.(j)
                    end                    
                    else                                          (* można przelać wszystko *)
                    begin
                        after.(i) <- 0;
                        after.(j) <- state.(i) + state.(j)
                    end;
                    add after (time + 1)
                end
            done
        done;
        for i = 0 to l - 1 do                              (* dolewamy - d lub wylewamy - w *)
            let after_d = Array.copy state in
            after_d.(i) <- table_fst.(i);
            add after_d (time + 1);
            let after_w = Array.copy state in
            after_w.(i) <- 0;
            add after_w (time + 1)
        done
    done;
    !ans
