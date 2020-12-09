let n = read_int();;
let last = read_int();;

let is_square n = 
	if n = (int_of_float (sqrt (float_of_int n))) * (int_of_float (sqrt (float_of_int n))) then true else false;;

let last_digit n = n mod 10;;

let rec make_prob n k = 
	if k > n then 0 else 
	if (is_square k) then (if (last_digit k) = last then 1 + l(make_prob n (k+1)) else (make_prob n (k+1))) else (make_prob n (k+1));;



print_string ("Среди " ^ (string_of_int n) ^ " чисел встретилось " ^ (string_of_int (make_prob n 0)) ^ " полных квадратов, оканчивающихся на " ^ (string_of_int last));; 