let n = read_int();;

let rec split l x n = 
	if n >= String.length l then [] else
	if l.[n] = x then [(sub l 0 n);(sub l n ((length l) - n - 1))] ;;

let rec input_list n = 
	if n = 0 then [] else (split  @ (input_list (n-1));;

