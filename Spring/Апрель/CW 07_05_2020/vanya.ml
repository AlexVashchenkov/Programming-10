let n = read_int();;

let rec to_2 n = 
	if n = 0 then [0] else
	if n = 1 then [1] else (to_2 (n / 2)) @ [(n mod 2)];;

let rec deg_2 n = 
	if n = 0 then 1 else 2 * (deg_2 (n-1));;

let rec fill l n = 
	if (List.length l) = n then l else (fill (0 :: l) n);;

let rec generate len n k = 
	if n = k then [] else (fill (to_2 k) len) :: (generate len n (k+1));;

generate n (deg_2 n) 0;;

(int_of_string ((string_of_int 111) ^ "0"));;