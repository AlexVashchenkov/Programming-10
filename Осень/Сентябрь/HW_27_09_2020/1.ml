let m = read_int();;
let n = read_int();;


let rec prime n k = 
	if n = 1 then false else
	if k > n / 2 then true else 
	if n mod k = 0 then false else (prime n (k+1));;

let rec count m n = 
if m > n then 0 else (if (prime m 2) then 1 + (count (m+1) n) else (count (m+1) n));;

let length m n = (max m n) - (min m n);;

print_float ((float_of_int (count m n)) /. (float_of_int (length m n)));;