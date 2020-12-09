type ('k , 'v) tree = Leaf | Node of ('k * 'v) * ('k,'v) tree * ('k,'v) tree;;

let turn t = 
	match t with
 Leaf - >failwith""
|Node (a,t1,Node (b,t2,t3)) -> Node (b,Node (a,t1,t2),t3);;

turn (Node (