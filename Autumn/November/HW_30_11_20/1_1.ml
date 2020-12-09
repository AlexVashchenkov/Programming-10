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

let rec add tree x = 
	match tree with
 Leaf -> RNode (x,Leaf, Leaf)
|BNode (v, l, r) -> if x < v then BNode (v, (add l x), r) else BNode (v, l, (add r x))
|RNode (v, l, r) -> if x < v then RNode (v, (add l x), r) else RNode (v, l, (add r x));;

let case_5 tree = 
	match tree with
|BNode (g, RNode (p, RNode (n, t1, t2), t3), BNode (u, t4, t5)) -> (BNode (p, RNode (n, t1, t2),RNode (g, t3, BNode (u, t4, t5))));;

let case_4 tree = 
	match tree with 
|BNode (g, RNode (p, t1, RNode (n, t2, t3)), BNode (u, t4, t5)) -> (case_5 (BNode (g, RNode (n, RNode (p, t1, t2), t3), BNode (u, t4, t5))));;

let case_3 tree = 
	match tree with
|BNode (g, RNode (p, RNode (n, t1, t2), t3), RNode (u, t4, t5)) -> (RNode (g, BNode (p, RNode (n , t1, t2), t3), BNode (u, t4, t5)))
|_ -> case_4 tree;;
 
let case_2 tree = 
	match tree with
 BNode (p, BNode (n, t1, t2), t3) -> tree
|BNode (p, t1, BNode (n, t2, t3)) -> tree
|_ -> case_3 tree;; 
   
let case_1 tree = 
	match tree with 
 BNode (n, t1, t2) -> tree 
|RNode (n, t1, t2) -> BNode (n, t1, t2)
|_ -> case_2 tree;;

(*let tree = BNode (100, RNode (50, BNode (25, Leaf, Leaf), BNode (75, Leaf, Leaf)), RNode (150, BNode (125, Leaf, Leaf), BNode (175, Leaf, Leaf)));;*)

let tree = BNode (100, RNode (50, BNode (25, Leaf, Leaf), BNode (75, Leaf, Leaf)), RNode (150, Leaf, BNode (175, Leaf, Leaf)));;

case_1 (add tree 125);;
	
