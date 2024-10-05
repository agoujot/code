open Graphics
exception Incoherent_colors
(** Library for useful functions to run 2D cellular automata, including interface and commands. requires Graphics module. *)
let () = Random.self_init(); open_graph ""; resize_window 1000 1020; set_color black; fill_rect 0 0 1000 1020; set_window_title "Cellular Automata Terminal" (* initializes the graphics screen & the RNG *)
let () = set_font "fixed"
(** write l x y writes a list of strings one line each with the top one starting at x y *)
let rec write l x y =
	match l with
	| [] -> ()
	| h::t -> set_color black; fill_rect x y (6*String.length h) 19 (* to overlay the text on top of the CA, more pretty *); moveto x y; set_color white; draw_string h; write t x (y-20)
(** Waits until user presses a key *)
let rec wait() = if key_pressed() then () else wait()
(** Displays information about CAT, argument is a list of strings you might want to display at the bottom *)
let info rs = write ([
	"Hello and welcome to my Cellular Automata Terminal (CAT).";
	"";
	"To write a new CA, do go with the following arguments:";
	" - a function that given a list of the colors of the neighbours of a cell and the colour of that cell returns the color that cell should now take.";
	" - a list of tuples telling which neighbours the above function should be given, with the tuples being relative coordinates to the cell.";
	" - the size in cells of the square grid.";
	" - an array of colors listing all colours existing in your system.";
	" - a function for (re)initialization giving a color from an array of colors and unit. just put equ if you are fine with equal probabilities.";
	" - a list containing lines of text you might want to display, will appear at the end.";
	"(note: exit stops the CA but does not close the window; this is to give you the opportunity to close it or do something else)";
	"";
	"You will have the status in the top left.";
	"When CAT is running and you click, it will stop and go to pause.";
	"When in pause, you can press keys to do things:";
	" - spacebar (play/pause)   : Does Exactly What It Says On The Tin.";
	" - o (One)    : goes one frame further and then back to pause.";
	" - b (Backwards) : play but in reverse, shows you the state of before. going backwards to a frame that has been edited will show you it with the edits";
	" - l (Last)   : goes one frame backwards and back to pause.";
	" - e (Edit)   : lets you manually change the state of cells by clicking on them. it changes to the next one in the color array given earlier";
	"              (which means the order in that array counts). to exit editing mode, press o for Out.";
	" - n (New)    : generates a new random grid using the element function above.";
	" - d (Delete) : sets all the grid to black (it is therefore recommended that you have the state 'no life' and that it be black).";
	" - i (Info)   : shows this. exit by pressing any key";
	" - x (EXit)   : Does Exactly What It Says On The Tin. (Kills the CA, but does not in itself close the window. Can be used to restart another.)"]@rs) 10 980
(** tob64 n compresses n in base 64, assuming it's smaller than 64**2 *)
let rec tob64 n =
	if n < 10 then string_of_int n else
	if n < 36 then String.make 1 @@ char_of_int @@ n-10+65 else
	if n < 62 then String.make 1 @@ char_of_int @@ n-36+97 else
	if n = 62 then "-" else
	if n = 63 then "." else
	"'"^tob64 (n/64)^tob64 (n mod 64)
(** frb64 s gets a number out of a base 64 string *)
let rec frb64 s =
	if s.[0] = '\'' then let n, _ = frb64 @@ String.sub s 1 1 and m, _ = frb64 @@ String.sub s 2 1 in (n*64+m, 3) else
	((if '0' <= s.[0] && s.[0] <= '9' then int_of_char s.[0]-48 else
	if 'A' <= s.[0] && s.[0] <= 'Z' then int_of_char s.[0]-65+10 else
	if 'a' <= s.[0] && s.[0] <= 'z' then int_of_char s.[0]-97+36 else
	if '-' = s.[0] then 62 else
	63), 1)
(** Moore neighbourhood of range given *)
let moore r = 
	let rec it i j = 
		if i = r+1 then [[];[];[];[]] 
		else if j = r+1 then it (i+1) 0
		else List.mapi (fun k v ->
			match k with
			| 0 -> (i, j)::v
			| 1 -> (-j, i)::v
			| 2 -> (-i, -j)::v
			| 3 -> (j, -i)::v
			| _ -> assert false)
			(it i (j+1)) in
	List.concat (it 1 0)
(** Neumann neighbourhood of range given *)
let neumann r = let rec it i j = if i = (r+1) then [] else if j = (r-(abs i)+1) then it (i+1) (-r+(abs (i+1))) else if i = 0 && j == 0 then it i (j+1) else (i, j)::it i (j+1) in it (-r) 0
(** Cross neighbourhood of range given *)
let cross r = 
	let rec it i = 
		if i = 0 then [[];[];[];[]] else
		List.mapi (fun j v ->
			match j with
			| 0 -> (-i, 0)::v
			| 1 -> (i, 0)::v
			| 2 -> (0, -i)::v
			| 3 -> (0, i)::v
			| _ -> assert false)
		(it (i-1)) in
	List.concat (it r)
(** Saltire (diagonal cross) neighbourhood of range given *)
let saltire r = (let rec it k = if k = 0 then [] else (-k, -k)::(-k, k)::(k, -k)::(k, k)::it (k-1) in it r)
(** equ a returns a function that chooses a random element from a *)
let equ a = (fun () -> let r = Random.int (Array.length a) in a.(r))
(** count l c counts the number of occurrences of c in l *)
let rec count l c = match l with 
	| [] -> 0
	| i::s -> if i = c then 1 + count s c else count s c
(** bs bi su returns a function corresponding to the Bbi/Ssu notation see {{:https://conwaylife.com/wiki/Life-like_cellular_automaton#Notation}on lifewiki} *)
let bs bi su = 
	let test s = (* returns function that tests whether a count matches s *)
		let parts = String.split_on_char ',' s in
		(fun c -> List.exists 
		(fun ss -> if String.contains ss '-' (* set *)
			then (let bo = Array.of_list @@ List.map int_of_string @@ String.split_on_char '-' ss in bo.(0) <= c && c <= bo.(1))
			else int_of_string ss = c) (* single value *)
		parts) in
	let bi_ = test bi and su_ = test su in
	(fun l v ->
		let c = count l white in
		if v = black then
			if bi_ c then white else black
		else
			if su_ c then white else black
	)
(** b e si builds an array array of size si filled with results of e() (to allow for different random results) *)
let rec b e si = 
	let rec itx x = 
		let rec ity y =
			if y = si
			then [||]
			else Array.append [|e()|] (ity (y+1))
	    in if x = si
		then [||]
		else Array.append [|ity 0|] (itx (x+1))
	in itx 0
(** Deepcopy of an array array *)
let rec dcopy a = let rec it i = if i = Array.length a then [||] else Array.append [|Array.copy a.(i)|] (it (i+1)) in it 0
(** For bottom left (even though now it is in the top left), erases the top and displays the text given. for status *)
let bl s = set_color black; fill_rect 0 1000 1000 20; write [s] 10 1000
(** inp s reads text from the homemade input field, waiting for a line return (call with "") *)
let rec inp s = 
	if key_pressed() then 
		(let c = read_key() in 
		if c = char_of_int 13 then s else
		if c = char_of_int 8 then (let ns = if s = "" then s else (String.sub s 0 (String.length s - 1)) in Unix.sleepf 0.1; bl ns; inp ns) else
		(Unix.sleepf 0.1; bl (s^(String.make 1 c)); inp (s^(String.make 1 c))))
	else inp s 
(** di for do it, as it does most things. Arguments are : an array array of cells (Graphics.color's), a character for parameters (p to play, o to do one frame, b to go backwards, l to go to the last frame, and s to stop), a function that returns a new color from a list of colors and the old color, a list of the relative coordinates of neighbours, an array of the colors of the CA, a function returning a color from unit, a list of strings to display for information, a list of compressed changes, and the grid of before. you are not supposed to touch this, call go. *)
let rec di g p f n col e rs h _g =
	let g_ = dcopy g in (* new array in which modifications will be made *)
	let si = Array.length g in
	let z = 1000/si in (* the widh of a cell in pixels *)
	let rec w k = if k >= si then k - si else if k < 0 then si + k else k in (* w for wrap around *)
	let draw x y c = set_color c; fill_rect (x*z) (y*z) (z-1) (z-1) in (* draws a cell, aka a rectangle. c is the color *)
	let fi co = (* Find the Index for a color in col *)
		let rec fi' i = 
			if i = Array.length col
			then raise Incoherent_colors
			else
			if col.(i) = co
			then i
			else fi' (i+1) in 
		fi' 0 in
	let reverse aa c =
		let l = 
			let rec def l =
				match l with
				| v1::v2::v3::t -> 
					(if v1 = "" then -1 else int_of_string v1)::
					(if v2 = "" then 0 else int_of_string v2)::
					(if v3 = "" then 1 else int_of_string v3)::
					def t
				| _ -> []
			in
			def @@ List.tl @@ String.split_on_char ',' c 
		in
		let rec rev' l i j ls = 
			match l with
			| k::y::x::s -> 
				(let r = if k <> -1 then k else ls in 
				aa.(i+x).(j+y) <- col.(r); rev' s (i+x) (j+y) r)
			| _ -> () in
		rev' l 0 0 0; aa
		in
	let rec it i j c x y cc = (* it for iterate, as it does the main loop. c is a string describing changes, (x, y) is the position of the last change, and cc the color of that change *)
		if i = si
		then c
		else
			if j = si
			then it (i+1) 0 c x y cc
			else 
				let rec ch l = (* ch for check, no ideas, applies the relatives coordinates in n to the coordinates of the current cell to get the colors of the cells to check.*)
					match l with
					| [] -> []
					| hd::tl -> g.(w (i+fst(hd))).(w (j+snd(hd))):: ch tl in	
				(if p = 's' (* for stop *) 
				then (draw i j (g_.(i).(j)); it i (j+1) c x y cc) 
				else if p = 'b' || p = 'l' 
				then (if g.(i).(j) <> _g.(i).(j) then draw i j (g.(i).(j)); it i (j+1) c x y cc) else
				(let co = f (ch n) (g.(i).(j)) in
				g_.(i).(j) <- co;
				let changed = g.(i).(j) <> co in
				(if changed then draw i j co);
				it i (j+1) 
					(if changed then c^","
						^(if g.(i).(j) <> cc then string_of_int (fi g.(i).(j)) else "")^","
						^(if j <> y then string_of_int (j-y) else "")^","
						^(if i <> x+1 then string_of_int (i-x) else "")
					else c)
					(if changed then i else x)
					(if changed then j else y)
					(if changed then g.(i).(j) else cc))
				) in
	let change = it 0 0 "" 0 0 col.(0) in 
	let rec wa() = (* for wait *)
		match read_key() with
		| ' ' -> (match p with 
			| 'p' -> bl " - running - ";di g_ 'p' f n col e rs (change::h) g
			| 'b' -> if h <> [] then (bl " - running - "; di (reverse g_ (if change = "" then List.hd h else change)) 'b' f n col e rs (if change = "" then List.tl h else h) (if change = "" then g else dcopy g_)) else (bl "ERROR: NO VISIBLE HISTORY"; wa())
			| _ -> () )
		| 'p' -> bl " - running - "; di g_ 'p' f n col e rs (change::h) g
		| 'o' -> bl " - pause - "; di g_ 'o' f n col e rs (change::h) g
		| 'e' -> bl " - editing - ";
			let rec ed() = (* for editing *)
			if key_pressed() 
			then 
				(if read_key() = 'o' 
				then (Unix.sleepf 0.2; bl " - pause - "; wa()) 
				else ed()) 
			else 
				(if (button_down())
				then (let mx = fst(mouse_pos())/z and my = snd(mouse_pos())/z (* coordinates of the cell you clicked in g_ *) in 
				let co c = col.(((fi c) + 1) mod (Array.length col)) in
				g_.(mx).(my) <- co (g_.(mx).(my)); draw mx my (g_.(mx).(my)); Unix.sleepf 0.2 (* else you would always change it multiple times *); ed())
				else ed()) in ed()
		| 'x' -> ()
		| 'd' -> di (b (fun _ -> black) si) 's' f n col e rs [] g
		| 'n' -> di (b e si) 's' f n col e rs [] g
		| 'i' -> info (rs); wait(); let rec show i j = if i = si then wa() else if j = si then show (i+1) 0 else (draw i j (g_.(i).(j)); show i (j+1)) (*need to redisplay after showing text*) in show 0 0
		| 'b' -> if h <> [] then (bl " - running - "; di (reverse g_ (if change = "" then List.hd h else change)) 'b' (* use h to read the old ones *) f n col e rs (if change = "" then List.tl h else h) (if change = "" then g else dcopy g_)) else (bl "ERROR: NO VISIBLE HISTORY"; wa())
		| 'l' -> if h <> [] then (bl " - pause - "; di (reverse g_ (if change = "" then List.hd h else change)) 'l' f n col e rs (if change = "" then List.tl h else h) (if change = "" then g else dcopy g_)) else (bl "ERROR: NO VISIBLE HISTORY"; wa())
		| 's' ->
			let compress a =
				Array.map (Array.map fi) a |>
				Array.map
				(fun aa -> let rec it l la ti =
						match l with
						| [] -> if la > -1 then tob64 la^tob64 ti else ""
						| x::s -> if x = la then it s la (ti+1) else (if la > -1 then tob64 la^tob64 ti else "")^it s x 1 in
				it (Array.to_list aa) (-1) (-1)) |>
				Array.to_list |>
				(fun l -> let rec it l la ti =
					match l with
					| [] -> if la = "" then "" else la^(if ti > 1 then "$"^tob64 (ti-1) else "")
					| x::s -> if x = la then it s la (ti+1) else (if la = "" then "" else la^(if ti > 1 then "$"^tob64 (ti-1) else ""))^it s x 1 in
				it l "" (-1)) |>
				(fun x -> print_endline x; x)
			in
			let oc = open_out @@ "AC-"^(string_of_int @@ int_of_float @@ Unix.time())^".txt" in
			output_string oc @@ compress g;
			close_out oc;
			wa();
		| 'r' ->
			bl "Enter the file name without the .txt, or - to get the latest file created by CAT.";
			let fn = let fn_ = inp "" in 
				if fn_ = "-" then 
				(Sys.readdir "." |> Array.to_list |> List.filter (fun s -> Str.string_match (Str.regexp {|^AC-[0-9]+\.txt|}) s 0) |> List.sort (fun a b -> if a < b then 1 else if a > b then -1 else 0) |> List.hd)
				else fn_^".txt" in
			let decompress s =
				let rec it i li a =
					if Array.length li = si then (it i [||] (Array.append a [|li|])) else
					if i >= String.length s then a else
					if s.[i] = '$' then
						let n, j = frb64 (String.sub s (i+1) (String.length s-i-1)) in 
						it (i+1+j) [||] (Array.append a (Array.make n a.(Array.length a - 1)))
					else
					let n, j = frb64 (String.sub s i (String.length s-i)) in
					let m, k = frb64 (String.sub s (i+j) (String.length s -i-j)) in
					it (i+j+k) (Array.append li (Array.make m n)) a
					in
				it 0 [||] [||] |>
				Array.map (fun l -> Array.map (fun i -> col.(i)) l)
			in
			let ic = open_in fn in
			let s = input_line ic in
			close_in ic;
			di (decompress s) 's' f n col e rs [] g_
		| _ -> bl "ERROR: UNKNOWN COMMAND (PRESS i FOR INFO)"; wa() in
	if (key_pressed() && read_key() == ' ') || p = 's' then (bl " - pause - " ; wa()) else
	if p = 'p' then di g_ p f n col e rs (change::h) g (* standard path of continuing *) else
	if p = 'b' then (if h = [] then (bl "ERROR: NO VISIBLE HISTORY"; wa()) else di (reverse g (List.hd h)) 'b' f n col e rs (List.tl h) g_) else wa()
(** go f n si col e rs starts the CA using colors col, the neighbourhood n and the function f giving from the old state and the neighbours the new state in a square grid of size si that is initialized with results of (e col)(), also showing rs in info. *)
let go f n si col e rs = bl " - pause - "; di (b (e col) si) 's' f n col (e col) rs [] [||]
