type tree = Tree of tree list;;

(*let parse s = 
	let rec parse_comma p = 
		match s.[p] with
	 '*' -> (Star, p+1)
	|'(' -> let (left,p_) = parse_comma (p+1) in
		if s.[p_] <> ',' then (Printf.printf "',' expected on symbol %d \n" p_);
		let (right,p__) = parse_comma (p_+1) in
		if s.[p__] <> ')' then (Printf.printf "')' expected on symbol %d \n" p__);
		(Comma (left,right), p__ + 1)
	|_ -> (failwith "Unexpected symbol")
	
	in (let (t,p) = (parse_comma 0) in (if p <> String.length s then failwith "Extra symbols" else t));;*)
let s = "(()(()()))()";;

let rec parse n = 
	if n >= (String.length s) then ([],n) else
	match s.[n] with
 |'(' -> let (tl,n) = parse (n+1) in
	 if s.[n] = ')' then let (tl2,n) = parse (n+1) in
	 (Tree tl :: tl2,n) else failwith""
 |')' -> ([],n)
 |_ -> failwith"";;

let rec print_list l = 
	match l with
 [] -> ()
|a :: b -> match a with
 [] -> print_string "[]"
|Tree l2 -> print_string "Tree [ ";print_list l2;print_string " ]";print_list b;;

print_list (fst (parse 0));;





