type ('k , 'v) tree = Leaf | Node of ('k * 'v) * ('k,'v) tree * ('k,'v) tree;;

let is_bamboo tr = 
	let rec check tr lr = 
		match tr with
	 Leaf -> true
	|Node ((k,v),l,Leaf) -> if lr = 1 then (check l 1) else false
	|Node ((k,v),Leaf,r) -> if lr = -1 then (check r (-1)) else false
	|Node ((k,v),l,r) ->     if l = Leaf then (check tr (-1)) else 
				(if r = Leaf then (check tr 1) else false)
in (check tr (-1)) || (check tr 1);; 

let tr1 = (Node (("m",1),
			 Node (("f",2),
					Node (("c",4), Leaf, Leaf), 
					Node (("i",5),Leaf, Leaf)), 
			 Node (("s",3), Node (("p",6), Leaf, Leaf), 
					Node (("v",7),Leaf, Leaf))));;

let tr2 = (Node (("r",1), Node (("o",4), Leaf, Leaf), 
			  Node (("w",5),Leaf, Leaf)));; 

let bamboo = (Node (("a",1), Node (("b",2), Node (("c",3), Leaf, Leaf), Leaf), Leaf));;

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
