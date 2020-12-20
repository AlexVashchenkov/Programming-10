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

let rec add t (k,v) = 
	match t with
 Leaf -> Node ((k,v), Leaf, Leaf)
|Node ((k2,v2),l,r) -> if k = k2 then Node ((k2,v),l,r) else
	               if v < v2 then Node ((k2,v2),(add l (k,v)),r) else Node ((k2,v2),l,(add r (k,v)));;


let rec split_by_var t n (l1,l2) =
	match t with
 Leaf -> (l1,l2)
|Node ((k,v),l,r) -> if v <= n then (split_by_var r n (split_by_var l n ((k,v) :: l1,l2))) else (split_by_var r n (split_by_var l n (l1,(k,v) :: l2)));;

let tree =  (Node (("a",8),
			    (Node (("b",3),
					   (Node (("d",1),
							  Leaf,
							  Leaf))
					   ,
					   (Node (("e", 6),
							   (Node (("g", 4), Leaf, Leaf)), 
							   (Node (("h", 7), Leaf, Leaf))))))
			    ,
			    (Node (("c",10),
					   Leaf, 
					   (Node (("f", 14),
							    (Node (("i", 13), 
									     Leaf,
									     Leaf)
							    ), Leaf))))));;

let rec make_tree l t = 
	match l with
 [] -> t
|(k,v) :: b -> (make_tree b (add t (k,v)));;



print_tree (make_tree (fst (split_by_var tree 7 ([],[]))) Leaf) [];;	 
print_string "\n";;
print_tree (make_tree (snd (split_by_var tree 7 ([],[]))) Leaf) [];;
	