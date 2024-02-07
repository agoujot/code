open Graphics
open Sortlib
let rec bsort i j =
	if j = 0 then () else
	if i = j then (bsort 0 (j-1)) else
	(if t.(i) > t.(i+1) then swap i (i+1); bsort (i+1) j)
let () = set_window_title "Bubble sort"; bsort 0 999; off()
