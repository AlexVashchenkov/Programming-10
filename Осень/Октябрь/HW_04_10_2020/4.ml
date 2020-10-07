let empty (_ : string) : int = 0;;

let add d k v = fun k' -> if k = k' then v else d k';;

let dict = add empty "lexa" 5;;

dict "lexa";;

dict "lepeha";;

let dict = add dict "lev" 4;;
