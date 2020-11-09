open List;;

let p = 11;;
let k = 1023;;

let rec split_char s n = 
	if n = String.length s then [] else (split_char s (n+1)) @ [int_of_char s.[n]];;

(*let hash l = if String.length s = 1 then s.[0] mod k else (if
	match l with
[] 	
 a::[] -> a mod k
|a :: b -> 
)*)

let rec hash s n r = if String.length s = 1 then ((int_of_char s.[0]) mod k) else (if n + 1 = (String.length s) then r else 
		     if n = 0 
			then (hash s (n+1) (((int_of_char s.[n])*p + (int_of_char s.[n+1])) mod k)) 
			else (hash s (n+1) ((r + int_of_char s.[n+1]) mod k)));;
