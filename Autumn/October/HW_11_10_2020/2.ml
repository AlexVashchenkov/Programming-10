type ('a,'b) t = ('a * 'b) list;;

let p = 11;;
let k = 1023;;

let rec innit n f = 
	let rec initt n k f = 
		if k = n then [] else (f k) :: (innit n (k+1) f)
	in (innit n 0 f);;

let rec create f n = init n (fun x -> f x);;

let rec split_char s n = 
	if n = String.length s then [] else (split_char s (n+1)) @ [int_of_char s.[n]];;

let rec hash s n r = if String.length s = 1 then ((int_of_char s.[0]) mod k) else (if n + 1 = (String.length s) then r else 
		     if n = 0 
			then (hash s (n+1) (((int_of_char s.[n])*p + (int_of_char s.[n+1])) mod k)) 
			else (hash s (n+1) ((r + int_of_char s.[n+1]) mod k)));;

let rec find l n = 
	match l with
 [] -> failwith""
|a :: b -> if a = n then n else (find b n);;

let rec add hashtbl s = 
	
	

