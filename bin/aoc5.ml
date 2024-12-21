let get_lines filename =
    let channel = open_in filename in
    let rec read_lines acc =
        try
            read_lines ((input_line channel) :: acc)
        with
        | End_of_file ->
            close_in channel;
            List.rev acc
    in
    read_lines []
;;

let max_pages = 100;;

let get_rules lines =
    let rec get_rules_aux lines acc =
        match lines with
        | x::xs ->
            if (String.length x) == 0 then
                (acc, xs)
            else
                let numbers = String.split_on_char '|' x in
                let rule =
                    match numbers with
                    | a::b::_ -> (int_of_string a, int_of_string b)
                    | _::[] | [] -> failwith "Unexpected rule"
                in
                    get_rules_aux xs (rule::acc)
        | [] -> failwith "Unexpected end of rules"
    in
        let (rules, xs) = get_rules_aux lines [] in
        let rules_array = Array.make max_pages [] in
        let rules_array_insert (a, b) =
            rules_array.(a) <- (b::rules_array.(a));
        in
            List.iter rules_array_insert rules;
        (rules_array, xs)
;;

let get_updates lines =
    let rec get_updates_aux lines acc =
        match lines with
        | x::xs ->
            let pages = x
                |> String.split_on_char ','
                |> List.map int_of_string
            in
                get_updates_aux xs (pages::acc)
        | [] -> acc
    in
        get_updates_aux lines []
        |> List.rev
;;

let get_rules_and_updates lines =
    let (rules, xs) = get_rules lines in
    let updates = get_updates xs in
    (rules, updates)
;;

let print_rules rules =
    for i = 0 to (Array.length rules) - 1 do
        Printf.printf "%d:" i;
        List.iter (fun x -> Printf.printf " %d" x) rules.(i);
        Printf.printf "\n"
    done
;;

let print_updates updates =
    let print_list l =
        List.iter (fun x -> Printf.printf "%d " x) l;
        Printf.printf "\n"
    in
    List.iter print_list updates;
    Printf.printf "\n"
;;

let check_update update rules =
    let rec check_rules page page_rules positions =
        match page_rules with
        | x::xs ->
            (positions.(x) == -1 || positions.(page) < positions.(x))
            && check_rules page xs positions
        | [] -> true
    in
    let rec check_update_aux update positions =
        match update with
        | page::xs ->
            check_rules page rules.(page) positions
            && check_update_aux xs positions
        | [] -> true
    in
    let positions = Array.make max_pages (-1) in
    List.iteri (fun i p -> positions.(p) <- i) update;
    check_update_aux update positions
;;

let rec count_updates updates rules =
    match updates with
    | update::xs when check_update update rules ->
        List.nth update ((List.length update)/2) + (count_updates xs rules)
    | _ -> 0
;;

let swap a i j =
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
;;

let check_update_inc update rules =
    let positions = Array.make max_pages (-1) in
    let modified = ref false in
    List.iteri (fun i p -> positions.(p) <- i) update;
    let rec correct_update page page_rules =
        match page_rules with
        | x::xs ->
            if positions.(x) != -1 && positions.(page) < positions.(x) then
                (
                Printf.printf "swapping %d and %d\n" page x;
                swap positions page x;
                modified := true;
                correct_update page xs)
            else 
                correct_update page xs
        | [] -> ()
    in
    let rec check_update_inc_aux update rules =
        match update with
        | page::xs ->
            correct_update page rules.(page);
            check_update_inc_aux xs rules
        | [] -> ()
    in
    check_update_inc_aux update rules;
    let rec get_update index =
        if index < max_pages then
            if positions.(index) == -1 then
                get_update (index + 1) 
            else
                index :: (get_update (index + 1))
        else
            []
    in
    let update = get_update 0 in
    List.iter (fun x -> Printf.printf "%d\n" x) update;
    (!modified, update)
;;

let rec count_updates_inc updates rules =
    match updates with
    | x::xs ->
        let (modified, update) = check_update_inc x rules in
        if modified then
            List.nth update ((List.length update)/2) + (count_updates_inc xs rules)
        else
            0
    | _ -> 0
;;

let aoc5 () =
    let (rules, updates) =
        get_lines "input.txt" |> get_rules_and_updates
    in
        count_updates_inc updates rules
        |> Printf.printf "%d\n"
;;
