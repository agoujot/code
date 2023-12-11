#load "graphics.cma"
#load "unix.cma"
open Graphics
let rec printa a=
	let rec it i =
		if i < Array.length a
		then (print_int a.(i); print_string "; "; it (i+1))
	in it 0; print_newline()
let rec b i = if i = 1000 then [||] else Array.append [|Random.int 1000|] (b (i+1))
let () = open_graph ""; resize_window 1000 1000; set_color black; fill_rect 0 0 1000 1000; Random.self_init()
let draw x h = set_color white; fill_rect x 0 1 h; set_color black; fill_rect x h 1 1000
let rec sort t i=
	if Array.length t = 1
	then t
	else
		let thing = ((Array.length t)/2) in
		let m = Array.sub t 0 thing
		in let n = Array.sub t thing (Array.length t - thing)
		in let rec fuse a b j = 
			if a = [||]
			then b
			else 
			if b = [||] 
			then a
			else
				if a.(0) < b.(0)
				then (let x = Array.append [|a.(0)|] (fuse (Array.sub a 1 ((Array.length a) - 1)) b (j+1)) in (*draw (j-(Array.length a) - (Array.length b)) (t.(j));*) draw j (a.(0)); x)
				else (let x = Array.append [|b.(0)|] (fuse a (Array.sub b 1 ((Array.length b) - 1)) (j+1)) in (*draw (j-(Array.length a) - (Array.length b)) (t.(j));*) draw j (b.(0)); x)
		in fuse (sort m i) (sort n (i+thing)) i
let b_ =  b 0
let _ = (let rec show i = if i < Array.length b_ then (draw i (b_.(i)); show (i+1)) in show 0); sort b_ 0
let () = print_char(read_key())
