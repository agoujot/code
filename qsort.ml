open Graphics
open Sortlib
let rec qsort i j =
	if i >= j  then () else
	let rec split l r =
		if l >= r then (swap (r+1) j; if (pause 500000; t.(l) > t.(r+1)) then (swap l (r+1); l) else r+1) else
		let l_ = if (pause 500000; t.(l) < t.(j)) then l+1 else l in
		let r_ = if (pause 500000; t.(r) > t.(j)) then r-1 else r in
		if l_ = l && r_ = r then 
			(swap l r;
			split (l+1) (r-1))
		else split l_ r_ in
	let m = split i (j-1) in
	qsort i (m-1); 
	qsort (m+1) j
let () = set_window_title "Quick sort"; qsort 0 999; off()
