let n = 6;;

let rec input_list l = 
	if l = 0 then [] else (input_list (l-1)) @ [read_int()];;

let l = [1;1;0;1;1;1];;

let cake = Array.make n (-1);;
let cream = Array.make n 0;;


let rec main n ni l cake cream = 
	if ni = n then (cake,cream) else
	match l with
 [] -> (cake,cream) 
|a :: b -> (main n (ni+1) b (cake.(ni) <- a;cake) 
			 (cream.(ni) <- ni - a;cream));;

main n 0 l cake cream;;
	