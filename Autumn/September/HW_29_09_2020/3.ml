let f = open_in "1.txt";;

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

let lengths = [];;

let rec mem l x = 
	match l with
 [] -> false
|a :: b -> if a = x then true else (mem b x);;

let rec count l x = 
	match l with 
 [] -> 0
|a :: b -> if a = x then 1 + (count b x) else (count b x);;

let rec main words l = 
	match words with
 [] -> l
|a :: b -> (*if (mem l ((String.length a),a)) then (main b l) else*) (main b (((String.length a),a) :: l));;

let rec matog l lng = 
	match l with
 [] -> 0.
|(a,b) :: q -> (*((float_of_int (count words b)) *.*) ((float_of_int a) /. (float_of_int (List.length lng))) +. (matog q lng);;	

(*List.iter (fun (x,y) -> print_string "(";print_int x;print_string (" , " ^ y ^ ") ")) (main words lengths);;
print_string "\n";;*)

print_float (matog (main words lengths) (main words lengths));;