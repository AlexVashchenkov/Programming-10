type 'a tree = Leaf | Node of ('a tree) * ('a tree) * 'a;;

let rec add tr x = 
	match tr with
 Node (Leaf, Leaf, y) -> Node (Node (Leaf, Leaf, x),Leaf, y)
|Node (tr1, Leaf, y) -> (Node ((add tr1 x), Leaf, y))
|Node (Leaf, tr2, y) -> (Node (Node (Leaf, Leaf, x), tr2, y))
|Node (tr1, tr2, y) -> (Node ((add tr1 x), tr2, y));;                                                          

let tr = Node (Node (Node (Leaf,Leaf,4), Node (Leaf,Leaf,5),2), Node (Node (Leaf,Leaf,6), Node (Leaf,Leaf,7),3), 1);;

 