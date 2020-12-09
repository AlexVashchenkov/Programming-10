type ('k , 'v) tree = Leaf | Node of ('k * 'v) * ('k,'v) tree * ('k,'v) tree;;

let rec add tr k v = 
	match tr with
 Leaf -> Node ((k,v),Leaf, Leaf)
|Node (((k2,v2) as kv),l,r) ->   if k = k2 then (Node ((k,v),l,r)) else 
			(if k < k2 then (Node (kv,(add l k v),r)) else
					 (Node (kv,l,(add r k v))));; 

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
print_tree (add tr "d" 8) [];;

