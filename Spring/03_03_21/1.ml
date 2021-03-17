type expr = Empty | Char of char | Concat of expr * expr | RepOne of expr | RepZero of expr | Alt of expr * expr;;

let pos = ref 0;;

let s = "(1|2|3|4|5|)";;

let rec parse_s() = 
	if !pos >= (String.length s) then failwith"Out of bounds s" else
	let t = parse_t() in  print_int !pos;
	if s.[!pos] = '|' then (pos :=! pos + 1; Alt (t, parse_s()))
	else t
    and parse_t() = 
	if !pos >= (String.length s) then failwith"Out of bounds t" else
	match s.[!pos] with
 '(' -> pos :=! pos + 1; let s_ = parse_s() in if s.[!pos] <> ')' then failwith "Failwith"; pos :=! pos + 1; Concat (s_, parse_t())
|')'|'|' -> Empty
|
|_ -> let c = s.[!pos] in pos :=! pos + 1; Char c;;

parse_s();;

	

