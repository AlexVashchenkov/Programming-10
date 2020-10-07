let l1 = ["li"; "chzen"; "huan"; "666"; "666"; "ni"; "666"; "228"; "666"; "li"; "li"; "li"; "li"; "li"; "li"];;

let l = [6;3;5;8;1;4;2;7];;

let rec compare x1 x2 = 
	if x1 < x2 then 0 else 1;;

let rec step l = 
	match l with
 [] -> []
|a :: [] -> [a]
|h1 :: h2 :: tl -> if (compare h1 h2) = 0 then h1 :: (step (h2 :: tl)) else h2 :: (step (h1 :: tl));;

let rec sort l = 
	if l = (step l) then l else (sort (step l));;

sort l;;