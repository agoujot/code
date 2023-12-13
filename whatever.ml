#load "graphics.cma"
#load "unix.cma"
open Graphics
let zones = print_endline "zones"; let m = read_line() in if m = "" then 3 else int_of_string m
let diff = print_endline "diff"; let m = read_line() in if m = "" then 3 else int_of_string m
let typ = print_endline "typ"; let m = read_line() in if m = "" then false else true
let rand = print_endline "rand";let m = read_line() in let n = if m = "" then 50 else int_of_string m in if typ then n*6 else n
let avg = print_endline "avg"; let m = read_line() in if m = "" then false else true
let var () = if typ then (let x = Random.float 1. in let rec it i = if i >= rand then 0 else if 1./.(float_of_int i) < x then i else it (i+1) in if Random.bool() then (it 2) else - (it 2)) else (Random.int (2*rand+1) - rand)
let rig = print_endline "rig"; let m = read_line() in if m = "" then 0 else int_of_string m
let col = print_endline "col"; if zones <> 3 then false else let m = read_line() in if m = "" then true else false
let () = Random.self_init(); open_graph ""; resize_window 999 999; set_color black; fill_rect 0 0 998 998
let rec b i =
	if i = 999
	then [||]
	else Array.append [|Random.int 1000|] (b (i + 1))
let wrap k = if k < 0 then 999+k else if k >= 999 then k-999 else k
let rec di r =
	let r_ = Array.copy r in
	let rec it i =
		if i < 999
		then ((r_.(i) <- if avg then (r.(wrap(i-zones))+r.(wrap(i+zones)))/2 else ((r.(i))+var()+(r.(wrap(i+zones))-(r.(i)))/diff+(r.(wrap(i-zones))-(r.(i)))/(diff+rig))); 
		it (i+1)) in
	let rec show i =
		if i < 999
		then (set_color (if col then (let m = i mod 3 in if m = 0 then red else if m = 1 then green else blue) else white); 
		fill_rect i 0 1 (max (r_.(i)) 0); set_color  black; fill_rect i (max (r_.(i)) 0) 1 1000;show (i+1)) in
	it 0; show 0; di r_
let () = di (b 0)
