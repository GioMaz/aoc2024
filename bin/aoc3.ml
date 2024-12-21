let get_string filename =
    let channel = open_in filename in
    let s = really_input_string channel (in_channel_length channel) in
    close_in channel;
    s
;;

let explode s = List.init (String.length s) (String.get s);;

let try_read_number x =
    let rec try_read_number_aux x first acc =
        let base = int_of_char '0' in
        match x, first with
            | x::xs as full, error ->
                let n = (int_of_char x) - base in
                if n >= 0 && n <= 9 then
                    try_read_number_aux xs false (acc * 10 + n)
                else if error then
                    (0, full)
                else
                    (acc, full)
            | [], false -> (acc, x)
            | [], true -> (0, x)
    in
        try_read_number_aux x false 0
;;

let consume x = 
    let rec consume_aux x acc =
        match x with
        | 'm'::'u'::'l'::'('::xs ->
            let (n1, xs1) = try_read_number xs in
            if List.hd xs1 == ',' then
                let xs1 = List.tl xs1 in
                let (n2, xs2) = try_read_number xs1 in
                if List.hd xs2 == ')' then
                    consume_aux (List.tl xs2) (acc + n1*n2)
                else
                    consume_aux (List.tl xs2) acc
            else
                consume_aux (List.tl xs1) acc
        | [] -> acc
        | _::xs -> consume_aux xs acc
    in
        consume_aux x 0
;;

let consume_enabling x = 
    let rec consume_aux x acc enabled =
        match x, enabled with
        | 'd'::'o'::'('::')'::xs, _ -> consume_aux xs acc true
        | 'd'::'o'::'n'::'\''::'t'::'('::')'::xs, _ -> consume_aux xs acc false
        | 'm'::'u'::'l'::'('::xs, true ->
            let (n1, xs1) = try_read_number xs in
            if List.hd xs1 == ',' then
                let xs1 = List.tl xs1 in
                let (n2, xs2) = try_read_number xs1 in
                if List.hd xs2 == ')' then
                    consume_aux (List.tl xs2) (acc + n1*n2) true
                else
                    consume_aux (List.tl xs2) acc true
            else
                consume_aux (List.tl xs1) acc true
        | [], _ -> acc
        | _::xs, _ -> consume_aux xs acc enabled
    in
        consume_aux x 0 true
;;

let aoc3 () = get_string "input.txt" |> explode |> consume_enabling |> Printf.printf "%d\n";;
