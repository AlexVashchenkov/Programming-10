type 'v tree = Leaf | RNode of 'v * 'v tree * 'v tree | BNode of 'v * 'v tree * 'v tree;;

let rec print_prefix prefix = 
	match prefix with
 [] -> ()
|[true] -> print_string ("  " ^ (String.make 1 (char_of_int 0xCC)) ^ (String.make 4 (char_of_int 0xCD)))
|[false] -> print_string ("  " ^ (String.make 1 (char_of_int 0xC8)) ^ (String.make 4 (char_of_int 0xCD)))
|true :: b -> print_string ("  " ^ ((String.make 1 (char_of_int 0xBA)) ^ "    "));print_prefix b
|false :: b -> print_string "       ";print_prefix b;;

let print_indent k v prefix = 
	print_prefix prefix;Printf.printf "(%s,%3d)\n" k v;;

let print_leaf prefix = print_prefix prefix;print_string "*\n";;

let rec print_tree node prefix = 
	match node with
	 Leaf -> print_leaf prefix
|RNode (v,l,r) -> print_indent "R" v prefix;
			  
			 print_tree l (prefix @ [true]);
			
			 print_tree r (prefix @ [false])
|BNode (v,l,r) -> print_indent "B" v prefix;
			  
			 print_tree l (prefix @ [true]);
			
			 print_tree r (prefix @ [false]);;

let rec full_tree n m = if n = 0 then Leaf else 
			if n mod 2 = 0 then RNode (m,(full_tree (n-1) (2*m)),(full_tree (n-1) (2*m+1)))
			else BNode (m,(full_tree (n-1) (2*m)),(full_tree (n-1) (2*m+1)));;

print_tree (full_tree 5 1) [];;