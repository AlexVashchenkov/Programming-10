let s = "0123456789";;
let s = "0000";;

let re = Str.regexp "^0*\\([1-9][0-9]*\\|0\\)$";;

if Str.string_match re s 0 then 
	Printf.printf "Matched: %s\n" (Str.matched_group 1 s)
else
	print_string "match failed\n";;
