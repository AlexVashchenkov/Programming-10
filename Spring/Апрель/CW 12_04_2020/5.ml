open String;;

let s = read_line();;

let rec max_str s n l = 
	if n >= length s then l else
	if l = "" then (max_str s (n+1) (make 1 s.[n])) else
	
	if s.[n] > l.[length l - 1] then (max_str s (n+1) (l ^ (make 1 s.[n]))) else l;;

let rec all_max s n = 
	if n = length s then [] else (all_max s (n+1)) @ [(max_str s n "")];;

let rec find_max l x = 
	match l with
 [] -> x
|a :: b -> if (length a) > (length x) || (a > x) then (find_max b a) else (find_max b x);;

