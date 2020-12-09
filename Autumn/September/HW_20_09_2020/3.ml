let n = read_int();;

let rec make_prob n = 
	if n = 1. then (1. -. 1. /. 365.) else (make_prob (n-.1.)) *. (1. -. (n -. 1.) /. 365.);;

print_float (1. -. (make_prob n));;



