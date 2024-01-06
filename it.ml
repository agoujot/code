open Graphics
open CAT
let () = ini(); go
(fun l v -> let rec c l co = match l with | [] -> 0 | i::s -> if i = co then 1+c s co else c s co in
	let v1 = c l white and v2 = c l blue in
	if (v1 = v2 && 2<=v1 && v1 <= 4) || (v2 <= v1 && 1<=v2 && v1<=4)
	then white
	else if v1 <= v2 && 1 <= v1 && v2 <= 4
	then blue
	else if v1 + v2 != 3 && v1+v2 != 4
	then black
	else v)
[(1,1);(1,0);(1,-1);(0,1);(0,-1);(-1,1);(-1,0);(-1,-1)]
100
[|black; blue; white|]
(fun () -> let r = Random.int 3 in if r = 1 then white else if r = 2 then blue else black)
