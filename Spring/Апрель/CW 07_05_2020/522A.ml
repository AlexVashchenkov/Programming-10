open String;;
open Char;;

let n = read_int();;

let rec input_list n = 
	if n = 0 then [] else (input_list (n-1)) @ [read_line()];;

let pair s = 
	let rec split s n x buff = 
		if n >= length s then [buff] else
		if s.[n] = x 	
		then buff :: (split s (n+1) x "") 
		else (split s (n+1) x (buff ^ (make 1 s.[n])))
	in (if 	

let rec lowercase s n = 
	if n >= length s then "" else 
	if (int_of_char s.[n]) <= 122 && (int_of_char s.[n]) >= 97 
	then (make 1 s.[n]) ^ (lowercase s (n+1)) 
	else (make 1 (chr ((int_of_char s.[n]) + 32))) ^ (lowercase s (n+1));;




