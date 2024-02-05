open Graphics
let () = open_graph ""; resize_window 1000 1000; Random.self_init()
let rec b i = 
	if i = 1000
	then [||]
	else let r = Random.int 1000 in (moveto i 0; set_color (rgb 0 (r*255/1000) (255-r*255/1000)); lineto i r; set_color black; lineto i 1000; Array.append [|r|] (b (i+1)))
let t = b 0
let draw i = moveto i 0; set_color (rgb 0 (t.(i)*255/1000) (255-t.(i)*255/1000)); lineto i t.(i); set_color black; lineto i 1000
let rec fm i = if i = 999 then i else let ni = fm (i+1) in if (Unix.sleepf 0.; t.(i) < t.(ni)) then i else ni
let rec tri di =
	if di = 1000 then () else
	let min_i = fm di in
	let tmp = t.(min_i) in
	t.(min_i) <- t.(di); 
	t.(di) <- tmp;
	draw di;
	draw min_i;
	tri (di +1)
let () = tri 0; ignore (read_key())
