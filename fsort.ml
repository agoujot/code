open Graphics
let draw i h = moveto i 0; set_color white; lineto i h; set_color black; lineto i 1000
let rec b i = if i = 1000 then [||] else let r = Random.int 1000 in (draw i r; Array.append [|r|] (b (i+1)))
let () = open_graph ""; resize_window 1000 1000; set_color black; fill_rect 0 0 1000 1000; Random.self_init()
let rec sort t i=
	if Array.length t = 1
	then t
	else
		let mid = ((Array.length t)/2) in
		let m = Array.sub t 0 mid
		in let n = Array.sub t mid (Array.length t - mid)
		in let rec fuse a b j = 
			if a = [||]
			then b
			else 
			if b = [||] 
			then a
			else
				if a.(0) < b.(0)
				then (let x = Array.append [|a.(0)|] (fuse (Array.sub a 1 ((Array.length a) - 1)) b (j+1)) in draw j (a.(0)); x)
				else (let x = Array.append [|b.(0)|] (fuse a (Array.sub b 1 ((Array.length b) - 1)) (j+1)) in draw j (b.(0)); x)
		in fuse (sort m i) (sort n (i+mid)) i
let _ = let b_ = b 0 in let rec show i = if i < Array.length b_ then (draw i (b_.(i)); show (i+1)) in show 0; sort b_ 0
let () = ignore (read_key())
