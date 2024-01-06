open Graphics
let ini () = Random.self_init(); open_graph ""; resize_window 1000 1020; set_color black; fill_rect 0 0 1000 1020 (* initializes the graphics screen & the RNG *)
let rec write l x y = (* to write a list of strings one line each with the top one starting at x y *)
	match l with
	| [] -> ()
	| h::t -> moveto x y; set_color white; draw_string h; write t x (y-20)
let info() = set_color black; fill_rect 0 0 1000 1020; write [
"Hello and welcome to my Cellular Automata Terminal (CAT).";
"To use, first do ini(), then do go with the following arguments:";
" - a function that given a list of the colors of the neighbours of a cell and the colour of that cell returns the color that cell should now take.";
" - a list of tuples telling which neighbours the above function should be given, with the tuples being relative coordinates to the cell.";
" - the size in cells of the square grid.";
" - an array of colors listing all colours existing in your system.";
" - a function for (re)initialization giving from unit a color. should use random.";
"You will have the status in the top left.";
"When you click, CAT will stop and go to pause.";
"When in pause, you can press keys to do things:";
" - p (Play) : does exactly what it says on the tin.";
" - o (One) : goes one frame further and then back to pause.";
" - e (Edit) : lets you manually change the state of cells by clicking on them. it changes to the next one in the color array given earlier";
"              (which means the order in that array counts). to exit editing mode, press o for Out.";
" - n (New) : generates a new random grid using the element function above.";
" - d (Delete) : sets all the grid to black (it is therefore recommended that you have the state 'no life' and that it be black).";
" - i (Info) : show this.";
" - x (EXit) : closes CAT.";
"Click to exit this and go back to pause."] 10 980; let rec wait () = if button_down() then () else wait() in wait() (* display info about this *)
let rec b x e si = (* building an array array of size si filled with e() (to allow for different random results). should be called with x=0*)
	let rec l y =
		if y = si
		then [||]
		else Array.append [|e()|] (l (y+1))
    in if x = si
	then [||]
	else Array.append [|l 0|] (b (x+1) e si)
let rec dcopy a i = if i = Array.length a then [||] else Array.append [|Array.copy a.(i)|] (dcopy a (i+1)) (* deepcopy of an array array, called with i=0 *)
let rec di g p f n si col d = (* di for do it, main function that does pretty much everything. Arguments are: g for grid, an arrray array of cells (represented with Graphic.color s), p for parameter, a character defining the beheviou on that run, for f, n, si, and d just look up in info, and col is made out of the array of colors, its a function that just goes around the array *)
	let g_ = dcopy g 0 in (* new array in which modifications will be made *)
	let z = 1000/si in (* the widh of a cell in pixels *)
	let rec w k = if k >= si then k - si else if k < 0 then si + k else k in (* w for wrap around *)
	let draw x y c = set_color c; fill_rect (x*z) (y*z) (z-1) (z-1) in (* draws a cell, aka a rectangle. c is the color *)
	let bl s = set_color black; fill_rect 0 1000 1000 20; write [s] 10 1005 in (* for bottom left even though now it is in the top left, eh, erases the text at the top and replaces it, for status *)
	let rec cha i j e = (* cha for change, when you want to replace every cell with e(). *)
		if i < 100
		then (
			if j = 100
			then cha (i+1) 0 e
			else let v = e() in g_.(i).(j) <- v; draw i j v; cha i (j+1) e) in
	let rec it i j = (* it for iterate, as it does the main loop *)
		if i = si
		then ()
		else
			if j = si
			then it (i+1) 0
			else 
				let rec ch l = (* ch for check, no ideas, applies the relatives coordinates in n to the coordinates of the current cell to get the coordinates of the cells to check.*)
					match l with
					| [] -> []
					| h::t -> g.(w (i+fst(h))).(w (j+snd(h))):: ch t in
				((if p = 's' (* for stop *) then draw i j (g_.(i).(j)) (* you only show *) else (g_.(i).(j) <- f (ch n) (g.(i).(j)); (if g.(i).(j) <> g_.(i).(j) then draw i j (g_.(i).(j))))); it i (j+1)) in
	it 0 0; if not (button_down()) && p = 'p' (* for play*) then di g_ p f n si col d (*the standard path of just continuing*) else
	let rec wa() = (* for wait *)
		bl " - pause - ";
		match read_key() with
		| 'p' -> bl " - running - "; di g_ 'p' f n si col d
		| 'o' -> di g_ 'o' f n si col d (* meaning you only do it once *)
		| 'e' -> bl " - editing - ";
			let rec ed() = (* for editing *)
			if key_pressed() 
			then 
				(if read_key() = 'o' 
				then (Unix.sleepf 0.2; wa()) 
				else ed()) 
			else 
				(if (button_down())
				then (let mx = fst(mouse_pos())/z and my = snd(mouse_pos())/z (* coordinates of the cell you clicked*) in g_.(mx).(my) <- col (g_.(mx).(my)); draw mx my (g_.(mx).(my)); Unix.sleepf 0.2 (* else you would always change it multiple times*); ed())
				else ed()) in ed()
		| 'x' -> close_graph(); print_endline "Closed with X."
		| 'd' -> cha 0 0 (fun () -> black); wa()
		| 'n' -> cha 0 0 d; wa()
		| 'i' -> info(); let rec show i j = if i = si then wa() else if j = si then show (i+1) 0 else (draw i j (g_.(i).(j)); show i (j+1)) (*need to redisplay after showing text*) in show 0 0
		| _ -> print_endline "Unknown command. Press i for info."; wa()
	in wa()
let go f n si co d = info(); di (b 0 d si) 's' (* for stop, doesnt even do first iteration*) f n si (fun c -> let rec it i = if co.(i) = c then i else it (i+1) in co.(((it 0) + 1) mod (Array.length co))) (* makes the fucntion out of the array *) d
