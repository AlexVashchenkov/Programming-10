type ('k , 'v) tree = Leaf | Node of ('k * 'v) * ('k,'v) tree * ('k,'v) tree;;

let rec print_prefix prefix = 
	match prefix with
 [] -> ()
|[true] -> print_string ("  " ^ (String.make 1 (char_of_int 0xCC)) ^ (String.make 2 (char_of_int 0xCD)))
|[false] -> print_string ("  " ^ (String.make 1 (char_of_int 0xC8)) ^ (String.make 2 (char_of_int 0xCD)))
|true :: b -> print_string ("  " ^ ((String.make 1 (char_of_int 0xBA)) ^ "  "));print_prefix b
|false :: b -> print_string "     ";print_prefix b;;

let print_indent k v prefix = 
	print_prefix prefix;print_string ("(" ^ k ^ "," ^ v ^ ")\n");;

let print_leaf prefix = print_prefix prefix;print_string "*\n";;

let rec print_tree node prefix = 
	match node with
	 Leaf -> print_leaf prefix
|Node ((k,v),l,r) -> print_indent k (string_of_int v) prefix;
			  
			 print_tree l (prefix @ [true]);
			
			 print_tree r (prefix @ [false]);;

let rec number l x = 
	match l with 
 [] -> failwith""
|a :: b -> if a = x then 0 else 1 + (number l x);;

(*let bamboo (m,n) = 
	
	let rec make_bamboo (m2,n2) (m,n) q = 
		match (m2,n2) with
	(a,b) -> if Leaf
	|(m,_) -> Node (((String.make 1 (char_of_int (97 + q))),q),Leaf,(make_bamboo (m2+1,n2+1) (m,n) (q+1)))
	|(_,n) -> Node (((String.make 1 (char_of_int (97 + q))),q),(make_bamboo (m2+1,n2+1) (m,n) (q+1)),Leaf)
	|(_,_) -> Node (((String.make 1 (char_of_int (97 + q))),q),(make_bamboo (m2+1,n2+1) (m,n) (q+1)),(make_bamboo (m2+1,n2+1) (m,n) (q+1)))
in (make_bamboo (0,0) (m,n) 0);;*)

let bamboo (m,n) = 
	let rec make_bamboo (m2,n2) (m,n) q = 
		if m2 = m && n2 = n then Leaf else
		if m2 = m then Node (((String.make 1 (char_of_int (97 + q))),q),Leaf,Leaf) else
		if n2 = n then Node (((String.make 1 (char_of_int (97 + q))),q),Leaf,(make_bamboo ((m2+1),n2) (m,n) (q+1))) else
		Node (((String.make 1 (char_of_int (97 + q))),q),(make_bamboo ((m2+1),(n2+1)) (m,n) (q+1)),(make_bamboo ((m2+1),(n2+1)) (m,n) (q+1)))
in (make_bamboo (0,0) (m,n) 0);;
(*let rec bamboo (m,n) q =
	if *)
 
print_tree (bamboo (20,10)) [];;

