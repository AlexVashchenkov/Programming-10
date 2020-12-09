type 'a tree = Leaf | Node of ('a tree) * ('a tree) * 'a;;

let rec empty n = 
	if n = 0 then "" else "  " ^ (empty (n-1));; 

let rec lines n = 
	if n = 0 then "" else 
	if n = 1 then "| " else (lines (n-1)) ^ "| ";;

let rec string_of_tree l n = 
	match l with
 Leaf -> print_string "*\n"
|Node (tr1, tr2, x) ->  print_string ((string_of_int x) ^ "\n" ^ (lines n) ^ "\n+-");string_of_tree tr1 (n+1);
		       
			print_string ((empty (n-1)) ^ (lines (n-1)) ^ "\\-");string_of_tree tr2 (n+1);print_string ((lines n) ^ "\n");;

let tr = Node (Node (Node (Leaf,Leaf,4), Node (Leaf,Leaf,5),2), Node (Node (Leaf,Leaf,6), Node (Leaf,Leaf,7),3), 1);;

string_of_tree tr 1;;
