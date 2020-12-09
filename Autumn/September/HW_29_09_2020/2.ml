(*Матожидание длины текста в словах*)
(*"С:\\OCaml\\10th grade with git\\Autumn\\September\\HW_29_09_20\\"*)
let a = Sys.readdir "texts";;

let split pat s = 
	let pat_len = String.length pat in
	
	let prepare str pat = 
		let pos = Array.to_list (Array.init (String.length s - pat_len + 1) (fun x -> x)) in 
		List.filter (fun p -> String.sub s p pat_len = pat) pos in

	let rec spl s prev pos = match pos with
		hd :: tl -> String.sub s prev (hd-prev) :: spl s (hd+pat_len) tl
	       |[] -> [String.sub s prev (String.length s - prev)] in
	
	spl s 0 (prepare s pat);;

let rec read_file f = 
	try
		let u = input_line f in [u] @ (read_file f)
	with
		End_of_file -> [];;

let rec main a n = 
	if n = (Array.length a) then [] else
	let f = open_in ("texts\\" ^ a.(n)) in 
	let lst = (List.map (fun x -> split " " x) (read_file f)) in 
	(close_in f;lst :: (main a (n + 1)));;

(*Array.iter (fun x -> print_string (x ^ " ")) (Sys.readdir "texts");;*)
(*List.iter ((fun x -> print_string "[ ";
		     List.iter (fun y -> print_string "[ "; 
					 List.iter (fun z -> print_string z;print_string " ") 
		     y; print_string " ] ") 
	  x;print_string " ]\n")) ((main (Sys.readdir "texts") 0));;*)
(*List.iter ((fun x -> print_string x;print_string " ")) (List.flatten (List.flatten ((main (Sys.readdir "texts") 0))));;*)
let list_of_texts = (*List.iter ((fun x -> print_string x;print_string " "))*) ((main (Sys.readdir "texts") 0));;

let list_of_lengths = List.map (fun x -> List.length (List.flatten x)) list_of_texts;;

let rec matog l l2 = 
	match l with
 [] -> 0.
|a :: b -> (float_of_int (List.length (List.flatten a))) /. (float_of_int (List.length l2)) +. (matog b l2);;

List.iter ((fun x -> print_string "[ ";
		     List.iter (fun y -> print_string "[ "; 
					 List.iter (fun z -> print_string z;print_string " ") 
		     y; print_string " ] ") 
	  x;print_string " ]\n")) (list_of_texts);;
print_string "\n\n";;
List.iter (fun x -> print_int x;print_string " ") list_of_lengths;;
print_string "\n\n";;
print_float (matog list_of_texts list_of_lengths);;

(*let my_hash = Hashtbl.create 1000000;;

let rec make_lengths lst hsh = 
	match lst with
 [] -> hsh
|a :: b -> if Hashtbl.mem hsh a then (make_lengths b (Hashtbl.add hsh a ((Hashtbl.find hsh a) + 1))) else (make_lengths b (Hashtbl.add hsh a 1));;

let rec max_hash_v hsh lst x =
	match lst with 
 [] -> x
|a :: b -> if Hashtbl.find hsh a > Hashtbl.find hsh x then (max_hash_v hsh b a) else (max_hash_v hsh b x);;
	
let rec get_elem l n = 
	match l with
 [] -> failwith"get_elem"
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

print_string (max_hash_v (make_lengths list_of_words (Hashtbl.create 1000000)) list_of_words (get_elem list_of_words 0));;
*) 





