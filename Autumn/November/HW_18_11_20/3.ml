type 'a tree = Leaf | Node of ('a tree) * ('a tree) * 'a;;

let rec find_min t=
		match t with
 Leaf->failwith""
|Node (Node(l1,l2,m),r,n) -> find_min (Node(l1,l2,m))
|Node (Leaf,r,n) -> n;; 

let rec delete t x=  
	match t with
 Node (l,r,n) -> if x = n 
	then (  if r = Leaf 
		then l 
		else (let y = (find_min r) in (Node (l,(delete r y), y)))) 
	else (  if x < n 
		then (Node (delete l x, r, n)) 
		else (Node (l, delete r x, n))) 
|Leaf -> failwith"";;

delete (Node (Node ((Node (Leaf,Leaf,25),Node (Leaf,Leaf,75),50)),(Node (Node (Leaf,Leaf,125),Node (Leaf,Leaf,175),150)),100)) 125;;