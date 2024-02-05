open Graphics
let pause t = let rec wa x = if x = 0 then () else wa (x-1) in wa t
let ini() =  open_graph ""; resize_window 1000 1000; Random.self_init()
let col h = let c = h*255/1000 in rgb 0 (255-c) c
let draw i h = moveto i 0; set_color (col h); lineto i h; set_color black; lineto i 1000
let rec b i = if i = 1000 then [||] else let r = Random.int 1000 in (draw i r; Array.append [|r|] (b (i+1)))
