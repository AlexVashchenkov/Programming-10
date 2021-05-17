let m = read_int();;
let n = read_int();;

let rec grid m n mi ni =
	if mi > m then [] else
	if ni > n then (grid m n (mi+1) 0) else (mi,ni) :: (grid m n mi (ni+1));;

let rec print_grid l =
	match l with
 [] -> ()
|[[]] -> print_string "\n"
|[a] -> List.iter (fun (x,y) -> print_string ("(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ ") ")) a
|a :: b -> List.iter (fun (x,y) -> print_string ("(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ ") ")) a; print_grid b;;

let rec filter l (x,y) =
	match l with
 [] -> []
|(a,b) :: q -> if a = x || y = b then (filter q (x,y)) else (a,b) :: (filter q (x,y));;

let rec delete l x =
	match l with
 [] -> []
|a :: b -> if a = x then b else a :: (delete b x);;

let rec delete_list l l2 = 
	match l2 with
 [] -> l
|a :: b -> (delete_list (delete l a) b);;

let rec delete_all l x = 
	match l with
 [] -> []
|a :: b -> if a = x then (delete_all b x) else a :: (delete_all b x);;

let rec unique l = 
	let rec remove_same l ((w,x),(y,z)) = 
			match l with
	 [] -> ((w,x),(y,z)) :: l
	|((a,b),(c,d)) :: q -> 
			if (a = w || b = x) && (c = y || d = z) 
			then (remove_same q ((w,x),(y,z))) 
			else ((a,b),(c,d)) :: (remove_same q ((w,x),(y,z)))
		
	in let rec recursion l l2 = 
		match l2 with
	    [] -> l
	   |a :: b -> (recursion (remove_same l a) (remove_same b a))
		
	in (recursion l l);;
  
let rec main m n = 
	let rec _main_ map dlist m n mi ni =
		if mi > m then [] else
		if ni > n then (_main_ map dlist m n (mi+1) ni) else
		(List.map (fun (x,y) -> if x <> mi && y <> ni then ((mi,ni),(x,y)) else ((-1,-1),(-1,-1)))  (delete_list map dlist)) :: (_main_ map dlist m n mi (ni+1)) in
		
	let output = List.map (fun x -> (delete_all x ((-1,-1),(-1,-1)))) (_main_ (grid m n 0 0) [] m n 0 0) in
	(unique (List.flatten output));;

print_int (List.length (main m n));;