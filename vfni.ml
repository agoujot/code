open Graphics
let w = 1000
let bg = black
let fg = white
let sp = red
let () = open_graph ""; resize_window w w; set_color bg; fill_rect 0 0 w w; set_color fg
let n = 200
let z = w/n
let grid = Array.make_matrix n n false
let whit = Array.make_matrix n n false
let stillfree = Hashtbl.create (n*n)
let () =
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			Hashtbl.add stillfree (i, j) ()
		done
	done
let k = 1
let fill i j = fill_rect (i*z+k) (j*z+k) (z-k) (z-k)
let unfree (i, j) =
	grid.(i).(j) <- true;
	Hashtbl.remove stillfree (i, j)
let free_up (i, j) =
	grid.(i).(j) <- false;
	(if whit.(i).(j) then (set_color bg;
	fill_rect (i*z) (j*z) z z));
	Hashtbl.add stillfree (i, j) ();
	whit.(i).(j) <- false
let free b = List.filter (fun (i, j) ->
	0 <= i && 0 <= j && i < n && j < n &&
	(
		(b && not grid.(i).(j))
		||
		(not b && grid.(i).(j) && not whit.(i).(j))
	)
	)
let free_neumann_neighbours (i, j) =
	[(i+1, j); (i-1, j); (i, j+1); (i, j-1)]
	|> free true
let next_to_unfree (i, j) = 
	[(i+1, j); (i-1, j); (i, j+1); (i, j-1)]
	|> List.exists (fun (x, y) -> 0 <= x && 0 <= y && x < n && y < n && grid.(x).(y))
let free_moore_neighbours (i, j) =
	[(i+1, j); (i-1, j); (i, j+1); (i, j-1); (i+1, j+1); (i+1, j-1); (i-1, j+1); (i-1, j-1)]
	|> free true
let unfree_moore_neighbours (i, j) =
	[(i+1, j); (i-1, j); (i, j+1); (i, j-1); (i+1, j+1); (i+1, j-1); (i-1, j+1); (i-1, j-1)]
	|> free false
let shuffle d =
	let nd = List.map (fun c -> (Random.bits (), c)) d in
	let sond = List.sort compare nd in
	List.map snd sond
let get_long () =
	match Random.int 20 with
	| 0 -> 1
	| x when x < 3 -> 2
	| x when x < 6 -> 3
	| x when x < 11 -> 4
	| x when x < 14 -> 5
	| x when x < 16 -> 6
	| x when x < 18 -> 7
	| x when x < 20 -> 8
	| _ -> assert false
let contains_square l =
	List.exists (fun (i, j) ->
		List.mem (i+1, j) l &&
		List.mem (i, j+1) l &&
		List.mem (i+1, j+1) l
	) l
let rec uniq l =
	match l with
	| [] -> []
	| h::t -> if List.mem h t then uniq t else h::uniq t
let leaves_black_square l =
	l
	|> List.map unfree_moore_neighbours
	|> List.concat
	|> (@) l
	|> uniq
	|> contains_square
let dessiner_figure x y bs =
	let rec put_pixel i j next n don =
		unfree (i, j);
		whit.(i).(j) <- true;
		let nex =
			next
			|> List.filter (fun (i, j) -> not grid.(i).(j))
			|> (@) (free_neumann_neighbours (i, j))
			|> shuffle
			|> uniq
		in
		let don_ = (i, j)::don in
		if n <= 1 then don_ else
		match nex with
		| [] -> don_
		| (a, b)::t -> put_pixel a b nex (n-1) don_
	in
	let l = get_long() in
	let don = put_pixel x y [] l [] in
	if l > 4 && contains_square don
	then (
		List.iter free_up don;
		true
	)
	else begin
		let n = don
		|> List.map free_moore_neighbours
		|> List.concat
		|> uniq
		in
		List.iter unfree n;
		if bs && leaves_black_square n
		then (
			List.iter free_up (don @ n);
			true
		) else (
			set_color fg;
			List.iter (fun (i, j) -> fill i j) don;
			false
		)
	end
let rec free_next_interdit () =
	Hashtbl.to_seq_keys stillfree
	|> List.of_seq
	|> List.filter next_to_unfree
	|> shuffle
	|> (fun l -> List.nth_opt l 0)
let rec di k =
	if k > 50 then () else
	let n = free_next_interdit() in
	match n with
	| None -> ()
	| Some (x, y) -> let v = dessiner_figure x y (k < 20) in di (if v then k+1 else 0)
let () =
	Random.self_init ();
	unfree (n/2, n/2);
	try di 0; ignore(wait_next_event [Button_down]); with Graphic_failure _ -> close_graph()
