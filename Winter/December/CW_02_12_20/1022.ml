let rec input_list n =
	if n = 0 then [] else [(read_line())] @ (input_list (n-1));;

let rec string_to_list s n = 
	if n >= String.length s then [] else
	if s.[n] = '0' && n = ((String.length s) - 1) then [] else 
	if s.[n] <> '0' && n = ((String.length s) - 1) then failwith"" else 
	[int_of_string (String.make 1 s.[n])] @ (string_to_list s (n+2));;

(*let n = read_int();;

let l = List.map (fun x -> string_to_list x 0) (input_list n);;*)

let l = ["0";"4 5 1 0";"1 0";"5 3 0";"3 0"];;

let lst = List.map (fun x -> string_to_list x 0) l;;


let rec get_elem l n = 
	match l with
 [] -> failwith""
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec create_matr l = 
	Array.init (List.length l) (fun x -> 
				if (get_elem l x) = [] 
					then Array.init (List.length l) (fun y -> false) 
				else Array.init (List.length l) (fun y -> if (List.mem (y+1) (get_elem l x)) 
									  then true 
									  else false));;

let rec print_matr m n = 
	if n >= (Array.length m) then () else (Array.iter (fun x -> if x = false then print_string "_ " else print_string "1 ") m.(n);print_string "\n";print_matr m (n+1));;

print_matr (create_matr lst) 0;;