open String;;
let split pat s = 
	let pat_len = String.length pat in
	
	let prepare str pat = 
		let pos = Array.to_list (Array.init (String.length s - pat_len + 1) (fun x -> x)) in 
		List.filter (fun p -> String.sub s p pat_len = pat) pos in

	let rec spl s prev pos = match pos with
		hd :: tl -> String.sub s prev (hd-prev) :: spl s (hd+pat_len) tl
	       |[] -> [String.sub s prev (String.length s - prev)] in
	
	spl s 0 (prepare s pat);;

let message = "* ***   * *** * *   *   *** * * ***         * *   * * *         * ***         * * *   ***   * * ***   *** * *   *   *** *   ***";;


type morse = Star | ThreeStars;;

let morse_rus = [
    ([Star; ThreeStars],"�");
    ([ThreeStars; Star; Star; Star],"�");
    ([Star; ThreeStars; ThreeStars],"�");
    ([ThreeStars; ThreeStars; Star],"�");
    ([ThreeStars; Star; Star],"�");
    ([Star],"�");
    ([Star; Star; Star; ThreeStars],"�");
    ([ThreeStars; ThreeStars; Star; Star],"�");
    ([Star; Star],"�");
    ([Star; ThreeStars; ThreeStars; ThreeStars],"�");
    ([ThreeStars; Star; ThreeStars],"�");
    ([Star; ThreeStars; Star; Star],"�");
    ([ThreeStars; ThreeStars],"�");
    ([ThreeStars; Star],"�");
    ([ThreeStars; ThreeStars; ThreeStars],"�");
    ([Star; ThreeStars; ThreeStars; Star],"�");
    ([Star; ThreeStars; Star],"�");
    ([Star; Star; Star],"�");
    ([ThreeStars],"�");
    ([Star; Star; ThreeStars],"�");
    ([Star; Star; ThreeStars; Star],"�");
    ([Star; Star; Star; Star],"�");
    ([ThreeStars; Star; ThreeStars; Star],"�");
    ([ThreeStars; ThreeStars; ThreeStars; Star],"�");
    ([ThreeStars; ThreeStars; ThreeStars; ThreeStars],"�");
    ([ThreeStars; ThreeStars; Star; ThreeStars],"�");
    ([ThreeStars; ThreeStars; Star; ThreeStars; ThreeStars],"�");
    ([ThreeStars; Star; ThreeStars; ThreeStars],"�");
    ([ThreeStars; Star; Star; ThreeStars],"�");
    ([Star; Star; ThreeStars; Star; Star],"�");
    ([Star; Star; ThreeStars; ThreeStars],"�");
    ([Star; ThreeStars; Star; ThreeStars],"�")] ;;

let rec type_to_morse lst = 
	match lst with 
 [] -> ""
|Star :: b -> "* " ^ (type_to_morse b) 
|ThreeStars :: b -> "*** " ^ (type_to_morse b);;

let rec morse_to_type l = 
	match l with
 [] -> []
|"*" :: b -> Star :: (morse_to_type b)
|"***" :: b -> ThreeStars :: (morse_to_type b);;

let new_rus = List.map (fun (x,y) -> ((String.sub (type_to_morse x) 0 ((String.length (type_to_morse x)) - 1)),y)) morse_rus;;

let morse_eng = ["A"; "B"; "W"; "G"; "D"; "E"; "V"; "Z"; "I"; "J"; "K"; "L";
"M"; "N"; "O"; "P"; "R"; "S"; "T"; "U"; "F"; "H"; "C"; "_"; "_"; "Q"; "_"; "Y"; "X"; "_"; "_"; "_"] ;;

(*List.iter (fun x -> List.iter (fun y -> print_string y;print_string "\n") x; print_string "\n")*)
let lst = (List.map (fun x -> split "   " x) (split "         " message));;

let rec get_elem l n = 
	match l with 
 [] -> failwith "get_elem"
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec morse_to_text s n = 
	if n = List.length morse_eng then failwith"" else
if fst (get_elem morse_rus n) = (morse_to_type s) then (get_elem morse_eng n) else (morse_to_text s (n+1));;

let output = (List.map (fun x -> List.map (fun y -> morse_to_text y 0) x) (List.map (fun x -> List.map (fun y -> split " " y) x) lst));;

List.iter (fun x -> List.iter (fun y -> print_string y) x; print_string " ") output;;