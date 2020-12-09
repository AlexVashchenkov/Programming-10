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

let morse_eng = ["A"; "B"; "W"; "G"; "D"; "E"; "V"; "Z"; "I"; "J"; "K"; "L";
"M"; "N"; "O"; "P"; "R"; "S"; "T"; "U"; "F"; "H"; "C"; "_"; "_"; "Q"; "_"; "Y"; "X"; "_"; "_"; "_"] ;;

let split str pat = 
	let pos = Array.to_list (Array.init (String.length s - String.length pat + 1) (fun x -> x)) in 
	List.filter (fun p -> String.sub s p (String.length pat) = pat) pos;;

let letter_list = split_3 (split_9 str "         ") "   ";;


let to_morse_type message n = 
	match (String.sub message.[n]) with 
 
let rec to_str 
