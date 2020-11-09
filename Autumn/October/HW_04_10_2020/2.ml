(*let message = "* ***   * *** * *   *   *** * * ***         * *   * * *         * ***         * * *   ***   * * ***   *** * *   *   *** *   ***";;*)

let message = "ALEX IS A STUDENT";;


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

let rec get_elem l n = 
	match l with 
 [] -> failwith "get_elem"
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec letter_to_morse s n = 
	if s = (get_elem morse_eng) then (type_to_morse (fst (get_elem morse_rus n))) else (letter_to_morse s (n+1));;


let rec string_to_list s n = 
	if n = String.length s then [] else (string_to_list s (n+1)) @ [(String.make 1 (Char.uppercase s.[n]))];;

let rec message_to_morse s n = 
	match s.[n] with
 ' ' -> (message_to_morse s (n+1)) ^ "         "
|_ -> (message_to_morse s (n+1)) ^ (letter_to_morse (String.make s.[n+1] 1) 0) ^ "   ";;

message_to_morse message 0;;