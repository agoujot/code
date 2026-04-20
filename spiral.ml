open Graphics
let m = 1000
let () = 
	open_graph "";
	resize_window (m+1) (m+1)
let dist x y = int_of_float(sqrt((fst x -. fst y)**2. +. (snd x -. snd y)**2.))
let rec dist_max = function
	| [] -> 0
	| h::t -> List.fold_left (fun a b -> if a > b then a else b) (dist_max t) (List.map (fun x -> dist h x) t)
let lambda = 0.05
let tau = 0.1
let p u v = ((1.-.lambda) *. u +. lambda *. v)
let po x y = (p (fst x) (fst y), p (snd x) (snd y))
let iof (u, v) = (int_of_float u, int_of_float v)
let fio (u, v) = (float_of_int u, float_of_int v)
let trans = Fun.id
let conv x = trans (float_of_int x *. lambda)
let max_dist = dist (0., 0.) (float_of_int m, float_of_int m)
let col x = int_of_float ( conv x *. 255. /. conv max_dist)
let sm = m/3
let small x = if x > sm then 0 else int_of_float ( conv (sm - x) *. 255. /. conv sm)
let rec loop l =
	let d = dist_max l in
	if d < m/100 then ignore(read_key()) else begin
	let poly = l @ [List.hd l] in
	set_color (rgb (small d) (col d) (255 - col d - small d) );
	draw_poly_line (Array.of_list (List.map iof poly));
	let l_ = (let rec it = function
		| x::y::t -> po x y :: it (y::t)
		| _ -> [] in it poly)
	in
	Unix.sleepf (tau *. lambda);
	loop l_
	end
let rec minl = function
	| [x] -> x
	| h::t -> min h (minl t)
	| _ -> assert false
let rec maxl = function
	| [x] -> x
	| h::t -> max h (maxl t)
	| _ -> assert false
let cleanup l =
	let x, y = List.split l
	in
	(* on ramène sur le bord en bas & à gauche *)
	let x_min = minl x
	and y_min = minl y
	in
	let x_ = List.map (fun a -> a-x_min) x
	and y_ = List.map (fun a -> a-y_min) y
	in
	(* puis en haut et à droit e*)
	let x_max = maxl x_
	and y_max = maxl y_
	in
	let x__ = List.map (fun a -> a*m/x_max) x_
	and y__ = List.map (fun a -> a*m/y_max) y_
	in
	List.combine x__ y__
let start l =
	set_color black;
	fill_rect 0 0 (m+1) (m+1);
	set_color white;
	l |> cleanup |> List.map fio |> loop
let star () = start [
	(0, 5);
	(4, 4);
	(5, 0);
	(6, 4);
	(10, 5);
	(6, 6);
	(5, 10);
	(4, 6)
]
let square () = start [ (0, 0); (0, 1); (1, 1); (1, 0) ]
let fake_triangle () = start [ (1, 2); (2, 0); (1, 0); (0, 0) ]
let cat () = start [ (0, 0); (0, 2); (1, 1); (2, 2); (2, 0) ]
let weird () = start [ (0, 4); (4, 0); (6, 10); (10, 6) ]
let () =
	Random.self_init();
	while true do
		let n = 20 in
		let l = Array.init n (fun i -> (Random.int m, Random.int m))
			|> Array.to_list in
		start l
	done
