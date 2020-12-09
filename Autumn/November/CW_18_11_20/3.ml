type ('k , 'v) tree = Leaf | Node of ('k * 'v) * ('k,'v) tree * ('k,'v) tree;;

let rec less_than_key tr k = 
	match tr with 
 Leaf -> []
|Node ((k2,v2),l,r) -> if k2 <= k then [(k2,v2)] @ (less_than_key r k) @ (less_than_key l k) else (less_than_key r k) @ (less_than_key l k);;

let rec large_than_key tr k = 
	match tr with 
 Leaf -> []
|Node ((k2,v2),l,r) -> if k2 > k then [(k2,v2)] @ (large_than_key r k) @ (large_than_key l k) else (large_than_key r k) @ (large_than_key l k);;

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

let tr = (Node (("abvd",1),
			 Node (("c",2),
					Node (("a",4), Leaf, Leaf), 
					Node (("b",5),Leaf, Leaf)), 
			 Node (("e",3), Node (("f",6), Leaf, Leaf), 
					Node (("g",7),Leaf, Leaf))));;


print_tree tr []