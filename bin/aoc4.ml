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

let get_matrix lines = Array.of_list lines;;

let print_matrix matrix =
    for i = 0 to (Array.length matrix) - 1 do
        for j = 0 to (String.length matrix.(i)) - 1 do
            print_char matrix.(i).[j]
        done;
        print_char '\n'
    done
;;

let is_xmas x m a s =
    match x, m, a, s with
    | 'X', 'M', 'A', 'S' -> true
    | _ -> false
;;

let count matrix =
    let count = ref 0 in
    let height = (Array.length matrix) in
    let width = (String.length matrix.(0)) in
    for i = 0 to height - 1 do
        for j = 0 to width - 1 do

            if i <= height - 4 && is_xmas
                matrix.(i).[j]
                matrix.(i+1).[j]
                matrix.(i+2).[j]
                matrix.(i+3).[j]
            then
                count := !count + 1;

            if i >= 3 && is_xmas
                matrix.(i).[j]
                matrix.(i-1).[j]
                matrix.(i-2).[j]
                matrix.(i-3).[j]
            then
                count := !count + 1;

            if j <= width - 4 && is_xmas
                matrix.(i).[j]
                matrix.(i).[j+1]
                matrix.(i).[j+2]
                matrix.(i).[j+3]
            then
                count := !count + 1;

            if j >= 3 && is_xmas
                matrix.(i).[j]
                matrix.(i).[j-1]
                matrix.(i).[j-2]
                matrix.(i).[j-3]
            then
                count := !count + 1;

            if i >= 3 && j >= 3 && is_xmas
                matrix.(i).[j]
                matrix.(i-1).[j-1]
                matrix.(i-2).[j-2]
                matrix.(i-3).[j-3]
            then
                count := !count + 1;

            if i >= 3 && j <= width - 4 && is_xmas
                matrix.(i).[j]      (* 5 0 *)
                matrix.(i-1).[j+1]  (* 4 1 *)
                matrix.(i-2).[j+2]  (* 3 2 *)
                matrix.(i-3).[j+3]  (* 2 3 *)
            then
                count := !count + 1;
            if i <= height - 4 && j >= 3 && is_xmas
                matrix.(i).[j]
                matrix.(i+1).[j-1]
                matrix.(i+2).[j-2]
                matrix.(i+3).[j-3]
            then
                count := !count + 1;

            if i <= height - 4 && j <= width - 4 && is_xmas
                matrix.(i).[j]
                matrix.(i+1).[j+1]
                matrix.(i+2).[j+2]
                matrix.(i+3).[j+3]
            then
                count := !count + 1;
        done
    done;
    !count
;;

let is_mas m1 m2 a s1 s2 =
    match m1, m2, a, s1, s2 with
    | 'M', 'M', 'A', 'S', 'S' -> true
    | _ -> false
;;

let count_x matrix =
    let count = ref 0 in
    let height = (Array.length matrix) in
    let width = (String.length matrix.(0)) in
    for i = 1 to height - 2 do
        for j = 1 to width - 2 do
            if is_mas
                matrix.(i-1).[j-1]
                matrix.(i-1).[j+1]
                matrix.(i).[j]
                matrix.(i+1).[j-1]
                matrix.(i+1).[j+1]
            then
                count := !count + 1;

            if is_mas
                matrix.(i-1).[j+1]
                matrix.(i+1).[j+1]
                matrix.(i).[j]
                matrix.(i+1).[j-1]
                matrix.(i-1).[j-1]
            then
                count := !count + 1;

            if is_mas
                matrix.(i+1).[j+1]
                matrix.(i+1).[j-1]
                matrix.(i).[j]
                matrix.(i-1).[j-1]
                matrix.(i-1).[j+1]
            then
                count := !count + 1;

            if is_mas
                matrix.(i+1).[j-1]
                matrix.(i-1).[j-1]
                matrix.(i).[j]
                matrix.(i-1).[j+1]
                matrix.(i+1).[j+1]
            then
                count := !count + 1;
        done
    done;
    !count
;;

let aoc4 () = get_lines "input.txt" |> get_matrix |> count_x |> Printf.printf "%d\n";;
