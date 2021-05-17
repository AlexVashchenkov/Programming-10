let n = read_int();;

let m = read_int();;

let rec number n i =
	if i >= (String.length n) then [] else (String.make 1 n.[i]) :: (number n (i+1));;

let rec conc l = match l with
[] -> ""
|a :: b -> a ^ (conc b);;

let operation n = (string_of_int ((int_of_string 
	(conc (List.map (fun x -> (string_of_int ((int_of_string x) + 1))) (number n 0)))) mod 1000000007));;

let rec main n m = if m = 0 then n else (main (operation n) (m-1));;

print_int (String.length (main (int_of_string n) m));;