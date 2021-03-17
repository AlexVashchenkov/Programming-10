let s = "0123456789";;
let s = "0000";;
                                                                
let s = "20";;
let s = "65";;

let re = Str.regexp "^\\([2-6][0\\|2\\|4\\|6\\|8]\\|7[0\\|2\\|4]\\)$";;

if Str.string_match re s 0 then 
	Printf.printf "Matched: %s\n" (Str.matched_group 1 s)
else
	print_string "match failed\n";;
