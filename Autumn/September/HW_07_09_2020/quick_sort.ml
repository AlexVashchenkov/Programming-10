let l = [1;2;0;1;2;1;3;3;4;2];;

let rec low l x = 
	match l with
 [] -> []
|a :: b -> if a < x then a :: (low b x) else (low b x);;

let rec middle l x = 
	match l with
 [] -> []
|a :: b -> if a = x then a :: (middle b x) else (middle b x);;

let rec high l x = 
	match l with
 [] -> []
|a :: b -> if a > x then a :: (high b x) else (high b x);;
 
let mid l = List.nth l ((List.length l)/2);;

let rec quick_sort a = 
	if List.length a <= 1 then a else (quick_sort (low a (mid a))) @ (middle a (mid a))
 @ (quick_sort (high a (mid a)));;


	 