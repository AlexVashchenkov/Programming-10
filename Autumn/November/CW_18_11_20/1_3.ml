type ('k , 'v) tree = Leaf | Node of ('k * 'v) * ('k,'v) tree * ('k,'v) tree;;

let rec find tr k = 
	match tr with
 Leaf -> ""
|Node ((k2,v2),l,r) -> if k = k2 then (string_of_int v2) else (find l k) ^ (find r k);;

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

let tr = (Node (("d",1),
			 Node (("c",2),
					Node (("a",4), Leaf, Leaf), 
					Node (("b",5),Leaf, Leaf)), 
			 Node (("e",3), Node (("f",6), Leaf, Leaf), 
					Node (("g",7),Leaf, Leaf))));;

print_string (find tr "d");;