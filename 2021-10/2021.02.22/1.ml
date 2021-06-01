let s = read_line();;

let re = Str.regexp "^\\([2-7][0\\|2\\|4\\|6\\|8]\\)$";;

if Str.string_match re s 0 then 
	Printf.printf "Matched: %s\n" (Str.matched_group 1 s)
else
	print_string "match failed\n";;
