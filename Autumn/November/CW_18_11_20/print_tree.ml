
type tree = Leaf | Node of tree * tree * int;;

let rec print_prefix prefix = 
	match prefix with
 [] -> ()
|[true] -> print_string ((String.make 1 (char_of_int 0xCC)) ^ (String.make 2 (char_of_int 0xCD)))
|[false] -> print_string ((String.make 1 (char_of_int 0xC8)) ^ (String.make 2 (char_of_int 0xCD)))
|true :: b -> print_string (((String.make 1 (char_of_int 0xBA)) ^ "  "));print_prefix b
|false :: b -> print_string "   ";print_prefix b;;

let print_indent s prefix = 
	print_prefix prefix;print_string (s ^ "\n");;

let rec print_tree node prefix = 
	match node with
	 Leaf -> print_indent "" prefix
	|Node (l,r,n) -> print_indent (string_of_int n) prefix;
			  
			 print_tree l (prefix @ [true]);
			
			 print_tree r (prefix @ [false]);;

let rec full_tree n m = if n = 0 then Leaf else Node ((full_tree (n-1) (2*m)),(full_tree (n-1) (2*m+1)),m);;

print_tree (full_tree 3 1) [];;