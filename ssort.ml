open Graphics
open Sortlib
let t = ini(); b 0
let rec fm i = if i = 999 then i else let ni = fm (i+1) in if (pause 10000;  t.(i) < t.(ni)) then i else ni
let rec tri di =
	if di = 1000 then () else
	let min_i = fm di in
	let tmp = t.(min_i) in
	t.(min_i) <- t.(di); 
	t.(di) <- tmp;
	draw di t.(di);
	draw min_i t.(min_i);
	tri (di +1)
let () = tri 0; ignore (read_key())
