open Graphics
open Sortlib
let t = ini(); b 0
let rec isort i =
	if i = 1000 then () else
	let v = t.(i) in
	let rec it j =
		if (pause 10000; j > 0 && t.(j-1) > v) then (draw j t.(j-1); t.(j) <- t.(j-1); it (j-1)) else j in
	let i_ = it i in draw i_ v; t.(i_) <- v; isort (i+1)
let () = isort 0; ignore(read_key())
