type ('k , 'v) tree = Leaf | Node of ('k * 'v) * ('k,'v) tree * ('k,'v) tree;;

let rec add_elem tr (k,v) = 
	match tr with
 Leaf -> Node ((k,v),Leaf, Leaf)
|Node ((k2,v2),l,r) ->   if k = k2 then Node ((k2,v),l,r) else 
			(if k < k2 then (Node ((k2,v2),(add_elem l (k,v)),r)) 
				else (Node ((k2,v2),l,(add_elem r (k,v)))));;

let rec merge_trees tr1 tr2 = 
	match tr2 with
 Leaf -> tr1
|Node ((k,v),l,r) -> (add_elem (merge_trees (merge_trees tr1 l) r) (k,v));;

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

let tr1 = (Node (("m",1),
			 Node (("f",2),
					Node (("c",4), Leaf, Leaf), 
					Node (("i",5),Leaf, Leaf)), 
			 Node (("s",3), Node (("p",6), Leaf, Leaf), 
					Node (("v",7),Leaf, Leaf))));;

let tr2 = (Node (("r",1), Node (("o",4), Leaf, Leaf), 
			  Node (("w",5),Leaf, Leaf)));; 

print_tree (merge_trees tr1 tr2) [];;