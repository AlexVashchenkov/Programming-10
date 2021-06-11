let n = read_int();;

let rec fib n = 
	if n = 1 || n = 2 then 1 else (fib (n-1)) + (fib (n-2));;

print_int (fib n);;