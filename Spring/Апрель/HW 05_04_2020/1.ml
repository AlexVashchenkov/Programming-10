open String;;

let s = read_line();;

let rec find l n x = 
	if n >= length l then false else
	if l.[n] = x then true else (find l (n+1) x);;

let rec find_max s n l = 
	if n >= length s then l else
	if (find l 0 s.[n]) then (find_max s (n+1) ((index_from l s.[n])))	