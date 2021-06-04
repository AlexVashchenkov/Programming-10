let a = Array.init 3 (fun x -> read_line());;

let rec main a = 
	if a.(0) = "rock" && a.(1) = "scissors" && a.(2) = "scissors" then print_string "F" else
	
	if a.(0) = "paper" && a.(1) = "rock" && a.(2) = "rock" then print_string "F" else
	
	if a.(0) = "scissors" && a.(1) = "paper" && a.(2) = "paper" then print_string "F" else
	
	if a.(1) = "rock" && a.(0) = "scissors" && a.(2) = "scissors" then print_string "M" else

	if a.(1) = "paper" && a.(0) = "rock" && a.(2) = "rock" then print_string "M" else
	
	if a.(1) = "scissors" && a.(0) = "paper" && a.(2) = "paper" then print_string "M" else
	
	if a.(2) = "rock" && a.(0) = "scissors" && a.(1) = "scissors" then print_string "S" else

	if a.(2) = "paper" && a.(0) = "rock" && a.(1) = "rock" then print_string "S" else

	if a.(2) = "scissors" && a.(0) = "paper" && a.(1) = "paper" then print_string "S" else print_string "?";;

main a;;	