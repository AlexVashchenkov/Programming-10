let n = read_int();;

let rec main n l str = 	
	if n = 0 then [str] else
	match l with
 [1] -> (main (n-1) [0] (str @ [1]))
|[0] -> (main (n-1) [0;1] (str @ [0]))
|[0;1] -> (main (n-1) [0;1] (str @ [0])) @ (main (n-1) [0] (str @ [1]));;

let rec count n l = 	
	if n = 0 then 1 else
	match l with
 [1] -> (count (n-1) [0])
|[0] -> (count (n-1) [0;1])
|[0;1] -> (count (n-1) [0;1]) + (count (n-1) [0]);;

(*main 5 [0;1] [];;
count 5 [0;1];;

print_int (count n [0;1]);;
print_string "\n";;
List.iter (fun x -> List.iter (fun y -> print_int y) x; print_string " ") (main n [0;1] []);;

for i=1 to n do
print_int (count i [0;1]);print_string " ";
done;;*)

print_int (count n [0;1]);;