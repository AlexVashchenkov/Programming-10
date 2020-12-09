type 'a tree = Leaf | Node of ('a tree) * ('a tree) * 'a;;

let rec mem tr x = 
	match tr with
 Node (Leaf, Leaf, y) -> if y = x then true else false
|Node (tr1, Leaf, y) -> if y = x then true else (mem tr1 x)
|Node (Leaf, tr2, y) -> if y = x then true else (mem tr2 x)
|Node (tr1, tr2, y) -> if y = x then true else (mem tr1 x) || (mem tr2 x);;

let tr = Node (Node (Node (Leaf,Leaf,4), Node (Leaf,Leaf,5),2), Node (Node (Leaf,Leaf,6), Node (Leaf,Leaf,7),3), 1);;

 