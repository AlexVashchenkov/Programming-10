(*let l = ["hamburger"; "ham"; "ham"; "bur"; "bur"; "bur"; "bur"; "ger"; "burgerham"; "hamger"; "gerburger"; "uuuuuu"];;*)

let l = [3;5;3;6;3;2;3;4;6;7];;

(*Обычный сорт*)

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

(*Сорт для пар*)

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

let rec quick_sort_pair a = 
	if List.length a <= 1 then a else (quick_sort_pair (low2 a (mid2 a))) @ (middle2 a (mid2 a))
 @ (quick_sort_pair (high2 a (mid2 a)));;



let rec count l i = 
	match l with
 [] -> []
|a :: [] -> [(a,i+1)]
|h1 :: h2 :: tl -> if h1 = h2 then (count (h2 :: tl) (i+1)) else (h1,i+1) :: (count (h2 :: tl) 0);;

print_int (fst (List.hd (List.rev (quick_sort_pair (count (quick_sort l) 0)))));;
print_int (snd (List.hd (List.rev (quick_sort_pair (count (quick_sort l) 0)))));;
