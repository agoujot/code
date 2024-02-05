open Graphics
open Sortlib
let t = ini(); b 0
let rec tri i j =
	if j = 0 then () else
	if i = j then (tri 0 (j-1)) else
	((if t.(i) > t.(i+1) then
	let tmp = t.(i) in
	(t.(i) <- t.(i+1); 
	t.(i+1) <- tmp;
	draw i t.(i);
	draw (i+1) t.(i+1))); tri (i+1) j)
let () = tri 0 999; ignore (read_key())
