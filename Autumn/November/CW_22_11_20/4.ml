type ('k , 'v) tree = Leaf | Node of ('k * 'v) * ('k,'v) tree * ('k,'v) tree;;

let rec print_prefix prefix = 
	match prefix with
 [] -> print_string ""
|[true] -> print_string "  +--"
|[false] -> print_string "  \\--"
|true :: b -> print_string "  |  ";print_prefix b
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

let bamboo l = 
	
	let rec make_bamboo l l2 n = 
		match l with
	 [] -> (Node (((String.make 1 (char_of_int (97 + n))),n),Leaf,Leaf))
	|a :: b -> if a = 0 then (Node (((String.make 1 (char_of_int (97 + n))),n),(make_bamboo b l2 (n+1)),Leaf))
		    else (Node (((String.make 1 (char_of_int (97 + n))),n),Leaf,(make_bamboo b l2 (n+1))))
in (make_bamboo l l 0);;

let rec make_list n = if n = 0 then [] else 0 :: (make_list (n-1));;
 
bamboo (make_list 4);;

