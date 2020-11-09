type ('a,'b) t = ('a * 'b) list;;

let rec create f n = [];;

let rec add l n = n :: l;;

let rec mem l n = 
	match l with
 [] -> false
|a :: b -> if a = n then true else (mem b n);;

let rec find l n = 
	match l with
 [] -> failwith""
|a :: b -> if a = n then n else (find b n);;

let rec delete l n = 
	match l with
 [] -> []
|a :: b -> if a = n then (delete b n) else a :: (delete b n);;

let rec iter l f =
	match l with
 [] -> ()
|a :: b -> (f a);iter b f;;

let rec fold f al b = 
	match al with
 [] -> b
|ah :: at -> (f ah (fold f at b));;

