open Graphics
let pause t = let rec dec x = if x = 0 then () else dec (x-1) in dec t
let ini() =  open_graph ""; resize_window 1000 1000; Random.self_init()
let col h = let c = h*255/1000 in rgb 0 (255-c) c
let draw i h = moveto i 0; set_color (col h); lineto i h; set_color black; lineto i 1000
let drawa j a = let rec it i = if i = Array.length a then () else (draw (j+i) a.(i); it (i+1)) in it 0
let rec b i = if i = 1000 then [||] else let r = Random.int 1000 in (draw i r; Array.append [|r|] (b (i+1)))
let off () = ignore(read_key())
let t = ini(); b 0
let dra i = draw i t.(i)
let swap i j = let tmp = t.(i) in t.(i) <- t.(j); t.(j) <- tmp; dra i; dra j
