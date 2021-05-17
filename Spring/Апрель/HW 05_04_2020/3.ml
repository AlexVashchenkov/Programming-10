open String;;

let f = open_in "3.txt";;

let rec read_file() = 
	try
		let u = input_line f in [u] @ (read_file())
	with
 		End_of_file -> [];;

let l = read_file();;

let rec check_diff s n x = 
	if n >= length s then true else
	if n = 0 then (check_diff s (n+1) s.[n]) else
	if s.[n] = x then false else (check_diff s (n+1) s.[n]);;

let rec find_max s n l max_str = 
	if n >= length s then max_str else
	
	let new_l = l ^ (make 1 s.[n]) in
	
	if (check_diff new_l 0 ' ') then 
		(if (length new_l) > (length max_str) then (find_max s (n+1) new_l new_l) else (find_max s (n+1) new_l max_str))
	else (find_max s (n+1) (make 1 s.[n]) max_str);;

let rec maximum l x = 
	match l with
 [] -> x
|a :: b -> if (length a) > (length x) then (maximum b a) else (maximum b x);;

print_string (maximum (List.map (fun x -> (find_max x 0 "" "")) l) "");;