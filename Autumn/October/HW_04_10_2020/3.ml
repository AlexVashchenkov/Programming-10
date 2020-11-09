let rec string_to_number s n = 
	if n = String.length s - 1 then (int_of_char s.[0]) else (int_of_char s.[(String.length s) - 1 - n]) + 256 * (string_to_number s (n+1));;

