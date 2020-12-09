let n = read_int();;

let first_num n = int_of_string (String.make 1 ((string_of_int n).[0]));;

let rec count n k = 
	if k > n then 0 else 
	if (first_num k) = 1 then 1 + (count n (k+1)) else (count n (k+1));;

for i = 0 to n do

	print_string"\n(";
	print_int (count i 0);
	print_string ",";
	print_int n;
	print_string ")\n";
done;;