type tr = Star | Comma of tr * tr;;

type tree = Tree of tree list;;

let rec empty n = 
	if n = 0 then "" else " " ^ (empty (n-1));;

let rec print_tree l n = 
	match l with
|[] -> print_string "*"

|a :: b -> 
	match a with
 Tree [] -> (print_string ((empty n) ^ "+-\n");print_tree b (n+1))
|Tree x -> (print_string ((empty n) ^ "*\n");print_tree x n);print_tree b (n-1);;

print_tree [Tree [];Tree [Tree [];Tree []]] 0;;
