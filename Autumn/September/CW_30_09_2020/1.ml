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

let new_rus = List.map (fun (x,y) -> ((String.sub (type_to_morse x) 0 ((String.length (type_to_morse x)) - 1)),y)) morse_rus;;

let morse_eng = ["A"; "B"; "W"; "G"; "D"; "E"; "V"; "Z"; "I"; "J"; "K"; "L";
"M"; "N"; "O"; "P"; "R"; "S"; "T"; "U"; "F"; "H"; "C"; "_"; "_"; "Q"; "_"; "Y"; "X"; "_"; "_"; "_"] ;;

List.map (fun x -> split "   " x) (split "         " message);;
	