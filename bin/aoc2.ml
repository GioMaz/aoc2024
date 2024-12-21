let are_levels_safe l =
    let rec check_dec l =
        match l with
        | [] | [_] -> true
        | x::(y::_ as rest) ->
            x > y
            && x-y >= 1
            && x-y <= 3
            && (check_dec rest)
    in
    let rec check_inc l =
        match l with
        | [] | [_] -> true
        | x::(y::_ as rest) ->
            x < y
            && y-x >= 1
            && y-x <= 3
            && (check_inc rest)
    in
        (check_dec l) || (check_inc l)
;;

(*are_levels_safe [1;2;7;8;9] |> Printf.printf "%b";;*)

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

let string_to_level s = s
    |> String.split_on_char ' '
    |> List.map int_of_string;;

let are_levels_safe_dampener l =
    let check_dec x y = x > y && x-y <= 3 in
    let check_inc x y = y > x && y-x <= 3 in
    let rec check l used check_fun = 
        match l, used with
        | [], _ | [_], _ -> true
        | x::(y::_ as rest), true ->
            check_fun x y && check rest true check_fun
        | x::(y::ys as rest), false ->
            let is_ok = check_fun x y in
            if is_ok
            then (check rest false check_fun)
            else
                (Printf.printf "Removing %d %d\n" x y;
                (check (x::ys) true check_fun))
    in
        (check l false check_dec) || (check l false check_inc)
;;

let read_level_list filename = 
    get_lines filename
        |> List.map string_to_level
        |> List.filter are_levels_safe_dampener
        |> List.length

let aoc2 () = read_level_list "input.txt" |> Printf.printf "%d\n";;
