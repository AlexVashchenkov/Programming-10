open List;;

let rec read_file_impl f = 
	try
		let u = input_line f in [u] @ (read_file_impl f)
	with
		End_of_file -> [];;

let read_file name = 
        let f = open_in name in
        let contents = read_file_impl f in
        close_in f;
        contents;;

let rec split_str s n x = 
	if n = String.length s then [x] else
	if ((int_of_char s.[n]) >= 128 && (int_of_char s.[n]) <= 175) || ((int_of_char s.[n]) >= 224 && (int_of_char s.[n]) <= 239) then (split_str s (n+1) (x ^ (String.make 1 s.[n]))) else x :: (split_str s (n+1) "");;

let l = List.flatten (List.map (fun x -> split_str x 0 "") (read_file "1.txt"));;

let rec print_list_str l = 
	match l with
 [] -> ()
|a :: b -> print_string a; print_string " ; "; print_list_str b;;

print_list_str l;;