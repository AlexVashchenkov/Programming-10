let n = read_int();;

let rec input_list x = 
	if x = 0 then [] else (read_line()) :: (input_list (x-1));;

let rec lst n = 
	if n = 0 then [] else ((read_int()),(Array.init n (fun x -> Array.init n (fun y -> 0)))) :: (lst (n-1));;

lst 4;;
