open Graphics
open Sortlib
let rec isort i =
	if i = 1000 then () else
	let rec it j =
		if (pause 10000; j > 0 && t.(j-1) > t.(j)) then (swap j (j-1); it (j-1)) else () in
	it i; isort (i+1)
let () = set_window_title "Insertion sort"; isort 0; off()
