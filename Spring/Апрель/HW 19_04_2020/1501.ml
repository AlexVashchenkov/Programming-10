let t = read_int();;

let rec split s n = 
	if n >= String.length s then [s] else
	if s.[n] = ' ' then [String.sub s 0 n;String.sub s (n+1) ((String.length s) - n - 1)] 
		       else (split s (n+1));;

let rec input_list n = 
	if n = 0 then [] else [split (read_line()) 0] @ (input_list (n-1));;

let rec array_input n = Array.init n (fun x -> (x,(input_list x)));;