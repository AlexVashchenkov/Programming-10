(*type tr = Star | Comma of tr * tr;;*)

type tree = Tree of tree list;;
(*
let rec print_tree l = 
	match l with
|[] -> print_string ""
(*|[a] -> 	(match a with
 	 Tree [] -> print_string "[] "
	|Tree x -> (print_string "[ ";print_tree x;print_string "]"))*)

|a :: b -> 	match a with
 	 (*Tree [] -> (print_string "[] ";print_tree b) *)
	|Tree x -> (print_string "[";print_tree x;print_string "]");print_tree b;;

print_tree [Tree [Tree []; Tree [Tree []; Tree []]; Tree []; Tree[]]];;*)
(*
type tr = Star | Comma of tr * tr;;

type tree = Tree of tree list;;

let rec print_tree l = 
	match l with
|[] -> print_string ""
|a :: b -> 	match a with
 	|Tree x -> (print_string "[";print_tree x;print_string "]");print_tree b;;

print_tree [Tree [Tree []; Tree [Tree []; Tree []]; Tree []; Tree[]]];;
*)
let rec print_tree l n = 
	match l with
|[] -> print_string "*"
|a :: b -> (print_string ("*\n" ^ (String.make n ' ') ^ "+-"));	match a with
 	|Tree x -> print_tree x (n+2);print_string "\n";(print_string ((String.make (n) ' ') ^ "\\-"));print_tree b (n+2);;

print_tree [Tree [Tree [];Tree []]] 0;;












