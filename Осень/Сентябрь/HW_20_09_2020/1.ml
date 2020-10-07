let n = read_int();;

let is_square n = 
	if n = (int_of_float (sqrt (float_of_int n))) * (int_of_float (sqrt (float_of_int n))) then true else false;;

let last_digit n = n mod 10;;

let rec make_prob n k = 
	if k > n then 0 else 
	if (is_square k) then (if (last_digit k) = 0 then 1 + (make_prob n (k+1)) else (make_prob n (k+1))) else (make_prob n (k+1));;

for i = 0 to n do
	print_string "(";
	print_int (make_prob i 0);
	print_string " , ";
	print_int i;
	print_string ") ; ";
done;;