open List;;

let p = 11;;
let k = 1023;;

let rec split_char s n = 
	if n = String.length s then [] else (split_char s (n+1)) @ [int_of_char s.[n]];;

let rec hash s n r = if String.length s = 1 then ((int_of_char s.[0]) mod k) else (if n + 1 = (String.length s) then r else 
		     if n = 0 
			then (hash s (n+1) (((int_of_char s.[n])*p + (int_of_char s.[n+1])) mod k)) 
			else (hash s (n+1) ((r + int_of_char s.[n+1]) mod k)));;


let rec find l x = 
	match l with
 [] -> false
|a :: b -> if a = x then true else (find b x);;

let rec add a s = 
	if (find a.(hash s 0 0) s) = true then a else (a.(hash s 0 0) <- (a.(hash s 0 0) @ [s]);a);;

let a = Array.init (k-1) (fun x -> if x = (hash "a" 0 0) then ["a"] else []);;


	