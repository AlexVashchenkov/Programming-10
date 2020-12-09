type ('k , 'v) tree = Leaf | Node of ('k * 'v) * ('k,'v) tree * ('k,'v) tree;;

let l = [("a",1);("b",2);("c",3);("d",4);("e",5);("f",1);("g",1)];;

let rec less_than_key tr k = 
	match tr with 
 Leaf -> []
|Node ((k2,v2),l,r) -> if k2 <= k then [(k2,v2)] @ (less_than_key r k) @ (less_than_key l k) else (less_than_key r k) @ (less_than_key l k);;

let rec large_than_key tr k = 
	match tr with 
 Leaf -> []
|Node ((k2,v2),l,r) -> if k2 > k then [(k2,v2)] @ (large_than_key r k) @ (large_than_key l k) else (large_than_key r k) @ (large_than_key l k);;


	