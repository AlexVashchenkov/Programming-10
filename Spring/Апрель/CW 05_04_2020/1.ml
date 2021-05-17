let rows = read_int();;
let columns = read_int();;

let rec column n = 
	if n = 0 then [] else (column (n-1)) @ [n];;

let rec row m = 
	if m = 0 then [] else (row (m-1)) @ [m];;

let rec make_list = List.flatten (List.map (fun x -> List.map (fun y -> (x,y)) (column n)) (row m));;

List.iter (fun (x,y) -> print_string "(";print_int x;print_string " , ";print_int y;print_string "); ") make_list;;

let rec 