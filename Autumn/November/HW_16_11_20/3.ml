(*красивая печать скобочной записи ввиде структуры*)
type tree = Tree of tree list;;

let s = "(()(())((())))";;

let rec parse n = 
	if n >= (String.length s) then ([],n) else
	match s.[n] with
 |'(' -> let (tl,n) = parse (n+1) in
	 if s.[n] = ')' then let (tl2,n) = parse (n+1) in
	 (Tree tl :: tl2,n) else failwith""
 |')' -> ([],n)
 |_ -> failwith"";;

let rec make_str n = 
	if n = 0 then "" else "| " ^ (make_str (n-1));;                                                    

let rec empty_str n = 
	if n = 0 then "" else 
	if n = 1 then "| " else "  " ^ (make_str (n-1));;                                        

let rec empty n = 
	if n = 0 then "" else "  " ^ (make_str (n-1));;                                        

(*let rec print_structure l n = 
	match l with
 [] -> ()
|[a] -> (match a with
	 Tree [] -> print_string ((empty n) ^ "\n" ^ (empty (n-1)) ^ "\\-*\n")  
        |(Tree c) -> print_string ((empty n) ^ "\n" ^ (empty (n-1)) ^ "\\-+\n");print_structure c (n+1))
|a :: b -> match a with
 	 Tree [] -> print_string ((make_str n) ^ "\n" ^ (empty_str (n-1)) ^ "+-*\n");print_structure b n  
|_ -> print_string ((make_str n) ^ "\n" ^ "\n" ^ (empty_str (n-1)) ^ "+-*\n");(print_structure [a] (n+1));print_string "\n";print_structure b n;;
*)

let rec print_structure l s = 
	match l with
 [] -> ()
|[a] -> (match a with
	 Tree [] -> print_string (s ^ "\n" ^ s ^ "\n");print_string "\\-*\n"
	|Tree c -> print_string (s ^ "\n" ^ s ^ "\n");print_string "\\-*\n";print_structure c ("  " ^ s))
|a :: b -> match a with
	 Tree [] -> print_string (s ^ "\n" ^ s ^ "\n");print_string "|-*\n"
	|Tree c -> print_string (s ^ "\n" ^ s ^ "\n");print_string "|-*\n";print_structure c (s ^ "| \n");print_string (s ^ "\n" ^ s ^ "\n");print_structure b s;;






print_structure (fst (parse 0)) "| ";;  

