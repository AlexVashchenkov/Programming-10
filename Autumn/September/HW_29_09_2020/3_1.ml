let f = open_in "3.txt";;

let split pat s = 
	let pat_len = String.length pat in
	
	let prepare str pat = 
		let pos = Array.to_list (Array.init (String.length s - pat_len + 1) (fun x -> x)) in 
		List.filter (fun p -> String.sub s p pat_len = pat) pos in

	let rec spl s prev pos = match pos with
		hd :: tl -> String.sub s prev (hd-prev) :: spl s (hd+pat_len) tl
	       |[] -> [String.sub s prev (String.length s - prev)] in
	
	spl s 0 (prepare s pat);;

let rec read_file() = 
	try
		let u = input_line f in [u] @ (read_file())
	with
		End_of_file -> [];;

let words = List.flatten (List.map (fun x -> split " " x) (read_file()));;

let rec diff_words l l2 = 
	match l with
 [] -> l2
|a :: b -> if List.mem a l2 then (diff_words b l2) else (diff_words b (a :: l2));;

let lengths = [];;

let rec mem l x = 
	match l with
 [] -> false
|a :: b -> if a = x then true else (mem b x);;

let rec count l x = 
	match l with 
 [] -> 0
|a :: b -> if a = x then 1 + (count b x) else (count b x);;

let rec matog l l2 = 
	match l with
 [] -> 0.
|a :: b -> ((float_of_int (count l2 a)) *. (float_of_int (String.length a))) /. (float_of_int (List.length l2)) +. (matog b l2);;

List.iter (fun x -> print_string (x ^ " ")) words;;
print_string "\n";;
List.iter (fun x -> print_string (x ^ " ")) (diff_words words []);;
print_string "\n";; 
print_float (matog (diff_words words []) words);;
