open List;;
let f = open_in "1.txt";;

let rec read_file() =
	try
		let u = (input_line f) in [u] @ (read_file())
	with	
End_of_file -> [];;

let lst = read_file();;

print_int (length lst);;

