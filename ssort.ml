open Graphics
open Sortlib
let rec fm i = if i = 999 then i else let ni = fm (i+1) in if (pause 10000;  t.(i) < t.(ni)) then i else ni
let rec ssort di =
	if di = 1000 then () else
	let min_i = fm di in
	swap di min_i;
	ssort (di +1)
let () = set_window_title "Selection sort"; ssort 0; off()
