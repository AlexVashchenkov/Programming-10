open List;;
let f = open_in "1.txt";;

let rec read_file() =
	try
		let u = (input_line f) in [u] @ (read_file())
	with	
End_of_file -> [];;

let lst = read_file();;

let rec all_words s n x = 
	if n >= String.length s then [] else
	match s.[n] with
 ' '|','|'.' -> (all_words s (n+1) "") @ [x]
|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' -> (all_words s (n+1) (x ^ (String.make 1 s.[n])))
|_ -> (all_words s (n+1) "");;

let length_list = map (fun x -> List.map (fun y -> String.length y) (all_words x 0 "")) lst;;

List.iter (fun x -> print_string x;print_string "\n") lst;;
List.iter (fun x -> List.iter (fun y -> print_string y;print_string "") x;print_string "\n") (List.map (fun x -> (all_words x 0 "")) lst);;

if (length (flatten length_list)) = 0 then print_int 0 else print_float ((float_of_int (fold_left (+) 0 (flatten length_list))) /. (float_of_int (length (flatten length_list))));;