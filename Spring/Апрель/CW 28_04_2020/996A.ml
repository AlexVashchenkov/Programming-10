let n = read_int();;

let rec create_list n = 
	if n >= 1 then
		(if n >= 5 then 
			(if n >= 10 then
				(if n >= 20 then 
					(if n >= 100 then [1;5;10;20;100]
					else [1;5;10;20])
				else [1;5;10])
			else [1;5])
		else [1])
	else [];;

let rec get_elem l n = 
	match l with
 [] -> failwith""
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec main n l k =                             
	if n = 0 then k else 
	let q = n - (get_elem l (List.length l - 1)) in
	(main q (create_list q) (k+1));;

print_int (main n (create_list n) 0);;
