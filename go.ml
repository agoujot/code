open Graphics
(** size of a square in px *)
let z = 45
(** size of window in px *)
let si = z*20
(** board color *)
let bc = rgb 220 179 92
(** stone radius *)
let sr = 14
let () = open_graph ""; resize_window si si; set_color bc; fill_rect 0 0 si si; set_window_title "Go"
(* drawing the grid *)
let () = 
	let rec li i =
		if i = 19 then () else(
		moveto z ((i+1)*z);
		lineto (19*z) ((i+1)*z);
		li (i+1)) in
	let rec co j = 
		if j = 19 then () else (
		moveto ((j+1)*z) z;
		lineto ((j+1)*z) (19*z);
		co (j+1)) in
	set_color black; li 0; co 0
(** the board *)
let g = Array.make_matrix 19 19 0
(** add a stone to the board and display it *)
let add i j c =
	g.(i).(j) <- c;
	set_color (if c = 2 then black else white);
	fill_circle ((i+1)*z) ((j+1)*z) sr
(** remove a stone from the board and redraw the lines behind it *)
let remove i j =
	g.(i).(j) <- 0;
	set_color bc;
	fill_circle ((i+1)*z) ((j+1)*z) sr;
	set_color black;
	let c k = min (max k 0) (z*19) in
	moveto (c((i+1)*z-sr)) (c((j+1)*z));
	lineto (c((i+1)*z+sr)) (c((j+1)*z));
	moveto (c((i+1)*z)) (c((j+1)*z-sr));
	lineto (c((i+1)*z)) (c((j+1)*z+sr))
(** change i j c d add or removes depending on c's value, displaying if d *)
let change i j c d = if d then (if c = 0 then remove i j else add i j c) else g.(i).(j) <- c
(** the variations in i & j of orthogonal neighbours *)
let v = [(0, 1); (0, -1); (1, 0); (-1, 0)]
(** nc x y applies v to x y and filters out those outside the board *)
let nc x y = List.map (fun (i, j) -> (x+i, y+j)) @@ List.filter (fun (i, j) -> let x_ = x+i and y_ = y+j in x_ >= 0 && y_ <= 18 && x_ <= 18 && y_>=0) v
(** n x y is the same as nc x y but giving the values of neighbours instead of their coordinates *)
let n x y = List.map (fun (i, j) -> g.(i).(j)) (nc x y)
(** deep copy of an array *)
let dcopy a = Array.map Array.copy a
(** reverse a turn *)
let r t = if t = 1 then 2 else 1
(** compresses a grid to a number *)
let compress aa =
	let l = List.concat @@ Array.to_list @@ Array.map Array.to_list aa in
	let rec it = function
		| x::s -> x+3*it s
		| [] -> 0 in
	it l
(** check et ot vt rt d replaces all the ot stones et can eat by rt, counting vt as liberties, and displaying if d *)
let rec check et ot vt rt d =
		let rec it i j =
			if i = 19 then false else
			if j = 19 then it (i+1) 0 else
			if g.(i).(j) <> ot then it i (j+1) else (
			let ns = n i j in
			if List.exists (fun c -> c = vt || c = ot+3) ns
			then (g.(i).(j) <- ot+3; ignore(it i (j+1)); true)
			else it i (j+1)
			)
		in
		if it 0 0 then check et ot vt rt d else
		Array.iteri (fun i a -> Array.iteri (fun j c -> if c = ot then change i j rt d else if c = ot+3 then g.(i).(j) <- ot) a) g
(** di t p h Does it with turn t, p if the last player passed and the history h of past boards*)
let rec di t p h =
	if not (button_down()) then 
		if key_pressed() then 
			match read_key() with
			| ' ' -> if p then () else di (r t) true h
			| 'x' -> close_graph()
			| _ -> di t p h
		else di t p h
	else (
	let mx = (fst(mouse_pos())+z/2)/z-1
	and my = (snd(mouse_pos())+z/2)/z-1 in
	if mx < 0 || mx > 18 || my < 0 || my > 18 then di t p h else
	if g.(mx).(my) <> 0 then di t p h else (
	let g_ = dcopy g in
	add mx my t;
	let ot = r t in
	check t ot 0 0 true;
	check ot t 0 0 true;
	let s = compress g in 
	if List.mem s h then Array.iteri (fun i a -> Array.iteri (fun j c -> if g.(i).(j) <> c then change i j c true) a) g_;
	if g.(mx).(my) = 0 then di t p h else
	(Unix.sleepf 0.2;
	di ot false (s::h))
	))
(* score() calculates who won *)
let score () = 
	check 2 0 1 2 false;
	check 1 0 2 1 false;
	let l = List.concat @@ Array.to_list @@ Array.map Array.to_list g in
	let rec it = function
		| x::s -> let n = it s in (if x = 2 then n+1 else if x = 1 then n-1 else n)
		| [] -> -6 in
	it l
let () = di 2 false []; (* play the game *) try (
let res = score() in
set_color (if res > 0 then black else white);
fill_rect (si/2-150) (si/2-20) 300 40;
set_color (if res > 0 then white else black);
moveto (si/2-150+10) (si/2-20+10);
(if res > 0 then "Black" else "White")^" wins with a "^(abs res |> string_of_int )^" point margin." |> draw_string;
ignore(read_key()) (* and then do the endgame stuff *)
) with _ -> () (* if the screen was closed before it finished *)
