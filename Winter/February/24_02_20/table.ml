let f = open_in "����������.html";;

let rec read_file() = 
	try
		let u = input_line f in [u] @ (read_file())
	with
		End_of_file -> [];;

