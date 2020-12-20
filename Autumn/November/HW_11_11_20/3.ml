(*красивая печать скобочной записи ввиде структуры*)
type tree = Tree of tree list;;

let s = "(()(()))(())";;

let rec parse n = 
	if n >= (String.length s) then ([],n) else
	match s.[n] with
 |'(' -> let (tl,n) = parse (n+1) in
	 if s.[n] = ')' then let (tl2,n) = parse (n+1) in
	 (Tree tl :: tl2,n) else failwith""
 |')' -> ([],n)
 |_ -> failwith"";;

fst (parse 0);;

let rec print_structure l n = 
	match l with
 [] -> ()
|[a] -> print_string "\\-";print_structure a (n+1) 
|a :: b -> ;print_string "\n|\n";print_

