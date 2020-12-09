type morse = Star | ThreeStars;;

let morse_rus = [
    ([Star; ThreeStars],"À");
    ([ThreeStars; Star; Star; Star],"Á");
    ([Star; ThreeStars; ThreeStars],"Â");
    ([ThreeStars; ThreeStars; Star],"Ã");
    ([ThreeStars; Star; Star],"Ä");
    ([Star],"Å");
    ([Star; Star; Star; ThreeStars],"Æ");
    ([ThreeStars; ThreeStars; Star; Star],"Ç");
    ([Star; Star],"È");
    ([Star; ThreeStars; ThreeStars; ThreeStars],"É");
    ([ThreeStars; Star; ThreeStars],"Ê");
    ([Star; ThreeStars; Star; Star],"Ë");
    ([ThreeStars; ThreeStars],"Ì");
    ([ThreeStars; Star],"Í");
    ([ThreeStars; ThreeStars; ThreeStars],"Î");
    ([Star; ThreeStars; ThreeStars; Star],"Ï");
    ([Star; ThreeStars; Star],"Ð");
    ([Star; Star; Star],"Ñ");
    ([ThreeStars],"Ò");
    ([Star; Star; ThreeStars],"Ó");
    ([Star; Star; ThreeStars; Star],"Ô");
    ([Star; Star; Star; Star],"Õ");
    ([ThreeStars; Star; ThreeStars; Star],"Ö");
    ([ThreeStars; ThreeStars; ThreeStars; Star],"×");
    ([ThreeStars; ThreeStars; ThreeStars; ThreeStars],"Ø");
    ([ThreeStars; ThreeStars; Star; ThreeStars],"Ù");
    ([ThreeStars; ThreeStars; Star; ThreeStars; ThreeStars],"Ú");
    ([ThreeStars; Star; ThreeStars; ThreeStars],"Û");
    ([ThreeStars; Star; Star; ThreeStars],"Ü");
    ([Star; Star; ThreeStars; Star; Star],"Ý");
    ([Star; Star; ThreeStars; ThreeStars],"Þ");
    ([Star; ThreeStars; Star; ThreeStars],"ß")] ;;

let morse_eng = ["A"; "B"; "W"; "G"; "D"; "E"; "V"; "Z"; "I"; "J"; "K"; "L";
"M"; "N"; "O"; "P"; "R"; "S"; "T"; "U"; "F"; "H"; "C"; "_"; "_"; "Q"; "_"; "Y"; "X"; "_"; "_"; "_"] ;;

let split str pat = 
	let pos = Array.to_list (Array.init (String.length s - String.length pat + 1) (fun x -> x)) in 
	List.filter (fun p -> String.sub s p (String.length pat) = pat) pos;;

let letter_list = split_3 (split_9 str "         ") "   ";;


let to_morse_type message n = 
	match (String.sub message.[n]) with 
 
let rec to_str 
