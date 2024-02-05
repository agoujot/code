open Graphics
let () = open_graph ""; resize_window 1000 1000; Random.self_init()
let rec b i = 
	if i = 1000
	then [||]
	else let r = Random.int 1000 in (moveto i 0; set_color white; lineto i r; set_color black; lineto i 1000; Array.append [|r|] (b (i+1)))
let t = b 0
let draw i = moveto i 0; set_color (rgb 0 (t.(i)*255/1000) (255-t.(i)*255/1000)); lineto i t.(i); set_color black; lineto i 1000
let rec tri i j =
	if j = 0 then () else
	if i = j then (tri 0 (j-1)) else
	((if (Unix.sleepf 0.; t.(i) > t.(i+1)) then
	let tmp = t.(i) in
	(t.(i) <- t.(i+1); 
	t.(i+1) <- tmp;
	draw i;
	draw (i+1))); tri (i+1) j)
let () = tri 0 999; ignore (read_key())
