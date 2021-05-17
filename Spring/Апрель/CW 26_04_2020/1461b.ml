open String;;

let mn = read_line();;

let rec split l x n = 
	if n >= String.length l then [] else
	if l.[n] = x then (sub l n (split l x (n+1)))