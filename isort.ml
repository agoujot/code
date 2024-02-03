open Graphics
let draw i h = set_color white; moveto i 0; lineto i h; set_color black; lineto i 1000; Unix.sleepf 0.00001
let rec b i = 
	if i = 1000 then [||] else let r = Random.int 1000 in (set_color white; moveto i 0; lineto i r; set_color black; lineto i 1000; Array.append [|r|] (b (i+1)))
let rec isort t i =
	if i = Array.length t then t else
	let v = t.(i) in
	let rec it j =
		if j > 0 && t.(j-1) > v then (draw j t.(j-1); t.(j) <- t.(j-1); it (j-1)) else j in
	let i_ = it i in draw i_ v; t.(i_) <- v; isort t (i+1)
let () = Random.self_init(); open_graph ""; set_color black; resize_window 1000 1000; fill_rect 0 0 1000 1000; ignore (isort (b 0) 0); ignore(read_key())
