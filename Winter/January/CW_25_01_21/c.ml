type expr = Plus of expr * expr | Minus of expr * expr | Brac of expr | Var of string | Int of int;;

let rec print_expr s = 
	match s with
 Plus (s1,s2) -> print_expr s1; print_string " + "; print_expr s2
|Minus (s1,s2) -> print_expr s1; print_string " - "; print_expr s2
|Brac s1 -> print_string "("; print_expr s1; print_string ")"
|Var str -> print_string str
|Int x -> print_int x
|_ -> failwith "";;

print_expr (