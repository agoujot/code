#load "graphics.cma"
open Graphics
let h c = int_of_float((sqrt(3.)/.2.) *. (float_of_int c))
let () = open_graph ""; set_color black; resize_window 1025 1025; fill_rect 0 0 1024 1024; moveto 1024 0; set_color cyan; lineto 0 0; lineto 512 (h 1024); lineto 1024 0
let rec pin x y c =
	if c = 1
	then plot x y
	else begin
	let dc = c/2 in
	let qc = dc/2 in
	moveto (x-dc) y; lineto (x-dc-qc) (y+(h dc)); lineto (x-qc) (y+(h dc)); lineto (x-dc) y; (pin x y dc; pin (x-dc) y dc; pin (x-qc) (y+(h dc)) dc) end
let () = pin 1024 0 1024; print_float(Sys.time()); ignore(read_key())
