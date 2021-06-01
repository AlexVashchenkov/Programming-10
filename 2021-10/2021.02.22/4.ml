let s = ".7";;

(*let re = Str.regexp "^\\(\\([1-9][0-9]+\\|0\\)?\\).\\(\\([0-9]+[1-9]\\)?\\)$";; *)

let re = Str.regexp "^\\(\\([1-9][0-9]*\\|0\\).[0-9]*[1-9]\\)$";;

if Str.string_match re s 0 then 
	Printf.printf "Matched: %s\n" (Str.matched_group 1 s)
else
	print_string "match failed\n";;
