open Graphics
let display a =	
	let rec it i = 
		if i = Array.length a then () else (moveto i 0; set_color white; lineto i a.(i); set_color black; lineto i 1000; it (i+1))
	in it 0
let rec tri t di =
	if di = Array.length t then t else
	let rec it i =
		if i = Array.length t - 1 then i else
		let ni = it (i+1) in
		if t.(i) < t.(ni) then i else ni in
	let min_i = it di in
	let tmp = t.(di) in
	t.(di) <- t.(min_i); t.(min_i) <- tmp;
	display t; tri t (di +1)
let rec b i = 
	if i = 1000
	then [||]
	else Array.append [|Random.int 1000|] (b (i+1))
let () = open_graph ""; resize_window 1000 1000; set_color black; fill_rect 0 0 1000 1000; Random.self_init(); ignore (tri (b 0) 0); ignore (read_key())
