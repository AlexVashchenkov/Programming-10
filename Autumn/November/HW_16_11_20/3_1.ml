type tree = Tree of tree list;;

let rec empty n = 
	if n = 0 then "" else "  " ^ (empty (n-1));; 
let rec print_tree l n = 
	match l with
|[] -> print_string "*"
(*|[a] -> 	(match a with
 	 Tree [] -> print_string "[] "
	|Tree x -> (print_string "[ ";print_tree x;print_string "]"))*)

|a :: b -> print_string ("*\n"); match a with
 	 (*Tree [] -> (print_string "[] ";print_tree b) *)
	|Tree x -> print_string ((empty n) ^ "+-");print_tree x (n+1);print_string ("\n" ^ (empty n) ^ "\\-");print_tree b n;;

print_tree [Tree [Tree [];Tree []]] 0;;
