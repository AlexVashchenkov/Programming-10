let s = "http://lnmo.ru/news/frontpage/manual.txt";;

(*адрес.разделённый.точками:порт*)
(*let re = Str.regexp "^\\( \\([a-z]+://\\)? \\(\\([a-z].\\)+.[a-z]+\\) \\(:[1-9][0-9]*\\)? \\(\\(/[a-z]+\\)*?\\(.[a-z]+\\)\\)?\\)$";;*)

let re = Str.regexp "^\\(\\(\\([a-z]+\\)://\\)?\\(\\([a-z].\\)+.[a-z]+\\)\\(:[1-9][0-9]*\\)?\\(\\(/[a-z]+\\)*?\\)\\(.[a-z]+\\)?\\)$";;

if Str.string_match re s 0 then 
	Printf.printf "Matched: %s\n" (Str.matched_group 1 s)
else
	print_string "match failed\n";;
