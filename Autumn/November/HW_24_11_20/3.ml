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

let min_max_depth tr =  
	let rec max_depth tr n = 
		match tr with
	 Leaf -> n
	|Node ((k,v),l,r) -> (max (max_depth l (n+1)) (max_depth r (n+1)))
in 	
	let rec min_depth tr n = 
		match tr with
	 Leaf -> n
	|Node ((k,v),l,r) -> (min (min_depth l (n+1)) (min_depth r (n+1)))
in ((min_depth tr 0),(max_depth tr 0));;

let is_balanced tr = if (abs ((fst (min_max_depth tr)) - (snd (min_max_depth tr)))) <= 1 then true else false;;

let tr1 = (Node (("m",1),
			 Node (("f",2),
					Node (("c",4), Leaf, Leaf), 
					Node (("i",5),Leaf, Leaf)), 
			 Node (("s",3), Node (("p",6), Leaf, Leaf), 
					Node (("v",7),Leaf, Leaf))));;

let tr2 = (Node (("r",1), Node (("o",4), Leaf, Leaf), 
			  Leaf));; 

 


