let rec number n i =
	if i >= (String.length n) then [] else (String.make 1 n.[i]) :: (number n (i+1));;

let rec conc l = match l with
[] -> ""
|a :: b -> a ^ (conc b);;

let operation n = (string_of_int ((int_of_string 
	(conc (List.map (fun x -> (string_of_int ((int_of_string x) + 1))) (number n 0)))) mod 1000000007));;

let rec main n m = if m = 0 then n else (main (operation n) (m-1));;

let n = read_int();;

let rec split s n = 
	if n >= String.length s then failwith"" else
	if s.[n] = ' ' then [String.sub s 0 n;String.sub s (n+1) ((String.length s) - n - 1)] else (split s (n+1));;

let rec input n = 
	if n = 0 then [] else [split (read_line()) 0] @ (input (n-1));;

List.iter (fun x -> print_int (String.length (main x m))) (input n);;