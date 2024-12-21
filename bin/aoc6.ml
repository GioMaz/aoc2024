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

let get_matrix lines =
    let string_to_char_array s =
        Array.of_seq (String.to_seq s)
    in
        lines |> List.map string_to_char_array |> Array.of_list
;;

let print_matrix matrix =
    for i = 0 to (Array.length matrix) - 1 do
        for j = 0 to (Array.length matrix.(i)) - 1 do
            Printf.printf "%c " matrix.(i).(j)
        done;
        Printf.printf "\n"
    done
;;

type dir = Up | Right | Down | Left;;

let get_init_pos matrix =
    let pos = ref None in
    for i = 0 to (Array.length matrix) - 1 do
        for j = 0 to (Array.length matrix.(i)) - 1 do
            if matrix.(i).(j) == '^' then
                pos := Some (i, j)
            else
                ()
        done
    done;
    match !pos with
    | Some x -> x
    | None -> failwith "Init pos not found"
;;

let rec solve matrix i j dir =
    if i < Array.length matrix && i >= 0
        && j < Array.length matrix.(i) && j >= 0 then
        match matrix.(i).(j) with
        | '#' -> (
            match dir with
            | Up    -> solve matrix (i+1) (j+1) Right
            | Right -> solve matrix (i+1) (j-1) Down
            | Down  -> solve matrix (i-1) (j-1) Left
            | Left  -> solve matrix (i-1) (j+1) Up
        )
        | _ -> (
            matrix.(i).(j) <- 'X';
            match dir with
            | Up    -> solve matrix (i-1) j Up
            | Right -> solve matrix i (j+1) Right
            | Down  -> solve matrix (i+1) j Down
            | Left  -> solve matrix i (j-1) Left
        )
    else
        ()
;;

let count_visited matrix =
    let (i, j) = get_init_pos matrix in
    solve matrix i j Up;
    let count = ref 0 in
    for i = 0 to (Array.length matrix) - 1 do
        for j = 0 to (Array.length matrix.(i)) - 1 do
            match matrix.(i).(j) with
            | 'X' -> count := !count + 1
            | _ -> ()
        done
    done;
    !count
;;

let aoc6 () = get_lines "input.txt" |> get_matrix |> count_visited |> Printf.printf "%d\n";;
