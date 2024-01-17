open Graphics
let () = open_graph ""; resize_window 1000 1000; set_color black; fill_rect 0 0 1000 1000
let rec read ic = 
	try let l = List.map int_of_string (String.split_on_char ' ' (input_line ic)) in l::read ic with End_of_file -> close_in ic; []
let rec di last = 
	let o = read (open_in "ordres.txt") in
	let rec draw l = 
		match l with 
		| [x;y;w;r;g;b]::s -> set_color (rgb r g b); fill_rect x y w w; draw s
		| _ -> () in
	(if o <> last then (set_color black; fill_rect 0 0 1000 1000; draw o)); Unix.sleepf 0.1; di last
let () = di []
