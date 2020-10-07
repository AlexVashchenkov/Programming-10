open List;;

let f = open_in "1.txt";;

let rec read_file() = 
	try
		let u = input_line f in [u] @ (read_file())
	with
		End_of_file -> [];;

let rec split_str s n x = 
	if n = String.length s then [x] else
	if (int_of_char s.[n]) >= 65 then (split_str s (n+1) (x ^ (String.make 1 s.[n]))) else x :: (split_str s (n+1) "");;

let l = List.flatten (List.map (fun x -> split_str x 0 "") (read_file()));;

(*quick sort*)
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
(*end of quick sort*)
(*quick_sort for pairs*)
let rec low2 l x = 
	match l with
 [] -> []
|(a,b) :: q -> if b < x then (a,b) :: (low2 q x) else (low2 q x);;

let rec middle2 l x = 
	match l with
 [] -> []
|(a,b) :: q -> if b = x then (a,b) :: (middle2 q x) else (middle2 q x);;

let rec high2 l x = 
	match l with
 [] -> []
|(a,b) :: q -> if b > x then (a,b) :: (high2 q x) else (high2 q x);;
 
let mid2 l = (snd (List.nth l ((List.length l)/2)));;

let rec quick_sort2 a = 
if List.length a <= 1 then a else (quick_sort2 (low2 a (mid2 a))) @ (middle2 a (mid2 a))
 @ (quick_sort2 (high2 a (mid2 a)));;

(*end*)
let rec find_next s n i = 
	if s.[n] = ' ' then (find_next s (n+1) (i+1)) else i;;

let rec count l i = 
	match l with
 [] -> []
|a :: [] -> [(a,i+1)]
|h1 :: (h2 :: tl) -> if h1 = ""  then (count (h2 :: tl) i) else (if h1 = h2 then (count (h2 :: tl) (i+1)) else (h1,i+1) :: (count (h2 :: tl) 0));;

let rec print_list2 l = 
	match l with
 [] -> ()
|(a,b) :: q -> print_string "(";print_string a; print_string ","; print_int b;print_string") "; print_list2 q;;

let rec str_list l = 
	match l with
 [] -> ()
|a :: b -> print_string ("\"" ^ a ^ "\" "); str_list b;;

let rec sub_list l n = 
	match l with
 [] -> []
|a :: b -> if n = 0 then [] else a :: (sub_list b (n-1));;

print_list2 (sub_list (List.rev (quick_sort2 (count (quick_sort l) 0))) 10);;
