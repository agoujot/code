(*#use "topfind";;
#require "graphics";;
#require "unix";;*)
open Graphics
(** size in pixels of a square *)
let z = 90
(** size in pixels of the window *)
let si = z * 8
(** piece p i j c b shows the piece p of color c at indexes i, j, 2x as big if b *)
let piece p i j c b = 
	set_color c;
	let k = if b then 2 else 1 in
	set_line_width (k*3);
	let m x y = moveto ((i*z+x)*k) ((j*z+(z-y))*k)
	and l x y = lineto ((i*z+x)*k) ((j*z+(z-y))*k)
	and c x1 y1 x2 y2 x3 y3 = 
		curveto 
		((i*z+x1)*k, (j*z+(z-y1))*k)
		((i*z+x2)*k, (j*z+(z-y2))*k)
		((i*z+x3)*k, (j*z+(z-y3))*k)
	and r x y r = draw_circle ((i*z+x)*k) ((j*z+(z-y))*k) (r*k)
	in match p with
	| 'K' | 'k' -> m 45 23; l 45 12; m 40 16; l 50 16; m 45 50; c 45 50 54 35 51 29; c 51 29 49 24 45 24; c 41 24 39 29 39 29; c 36 35 45 50 45 50; m 23 74; c 34 81 54 81 65 74; l 65 60; c 65 60 83 51 77 39; c 69 26 50 32 45 47; l 45 54; l 45 47; c 38 32 19 26 13 39; c 7 51 23 59 23 59; l 23 74; m 45 50; m 23 60; c 34 54 54 54 65 60; m 23 67; c 34 61 54 61 65 67; m 23 74; c 34 68 54 68 65 74
	| 'Q' | 'q' -> r 12 22 5; r 28 16 5; r 45 14 5; r 62 16 5; r 78 22 5; m 18 52; c 35 49 60 49 72 52; l 76 28; l 62 50; l 62 22; l 51 49; l 45 19; l 39 49; l 28 21; l 28 50; l 14 28; l 18 52; m 18 52; c 18 56 21 56 23 60; c 25 63 25 62 24 67; c 21 69 21 72 21 72; c 18 75 22 77 22 77; c 35 79 55 79 68 77; c 68 77 71 75 68 72; c 68 72 69 69 66 67; c 65 62 65 63 67 60; c 69 56 72 56 72 52; c 55 49 35 49 18 52; m 23 60; c 30 58 60 58 67 60; m 24 67; c 36 65 54 65 66 67
	| 'B'| 'b' -> r 45 15 5; m 18 72; c 24 70 38 72 45 68; c 51 72 65 70 72 72; c 72 72 75 73 78 76; c 76 77 74 77 72 77; c 65 75 51 77 45 75; c 38 77 24 75 18 77; c 15 77 13 77 12 76; c 14 72 18 72 18 72; m 30 64; c 35 69 55 69 60 64; c 61 61 60 60 60 60; c 60 55 55 52 55 52; c 66 49 67 29 45 21; c 23 29 24 49 35 52; c 35 52 30 55 30 60; c 30 60 29 61 30 64; m 35 52; l 55 52; m 30 60; l 60 60; m 45 31; l 45 41; m 40 36; l 50 36
	| 'N' | 'n' ->  r 30 32 2; r 18 52 1; m 44 20; c 65 22 77 36 76 78; l 30 78; c 30 60 50 65 46 36; m 48 36; c 48 41 36 50 32 54; c 26 58 26 62 22 62; c 19 60 24 55 22 56; c 20 56 22 58 20 60; c 18 60 11 62 12 52; c 12 48 24 28 24 28; c 24 28 27 24 28 21; c 26 19 27 17 27 15; c 29 13 33 20 33 20; l 37 20; c 37 20 38 16 42 14; c 44 14 44 20 44 20
	| 'R' | 'r' -> m 18 78; l 72 78; l 72 72; l 18 72; l 18 78;
m 24 72; l 24 64; l 66 64; l 66 72; l 24 72; m 22 28; l 22 18; l 30 18; l 30 22; l 40 22; l 40 18; l 50 18; l 50 22; l 60 22; l 60 18; l 68 18; l 68 28; m 68 28; l 62 34; l 28 34; l 22 28; m 62 34; l 62 59; l 28 59; l 28 34; m 62 59; l 65 64; l 25 64; l 28 59; m 22 28; l 68 28
	| 'P' | 'p' -> m 44 18; c 39 18 36 21 36 26; c 36 27 36 29 37 30; c 33 33 31 37 31 42; c 31 46 32 49 35 52; c 29 54 21 63 21 79; l 67 79; c 67 63 58 54 52 52; c 55 49 57 46 57 42; c 57 37 54 33 50 30; c 51 29 52 27 52 26; c 52 21 48 18 44 18
	| _ -> ()
let rec explode s = if s = "" then [||] else Array.append [|s.[0]|] (explode(String.sub s 1 (String.length s-1)))
(* dobot t decides whether a bot should play player t *)
let dobot =
	open_graph (" "^string_of_int si^"x"^string_of_int (si+if Sys.win32 then 17 else 0)); resize_window si si; set_window_title "Chess";
	set_color black; fill_rect 0 0 1000 1000; set_color white; set_line_width 4;
	moveto (3*z/2) (15*z/2); rlineto ~-z 0; rlineto 0 ~-z; rlineto z 0; (* C *)
	rmoveto (z/2) z; rlineto 0 ~-z ; rmoveto z z; rlineto 0 ~-z; rmoveto 0 (z/2); rlineto ~-z 0; (* H *)
	rmoveto (5*z/2) (z/2); rlineto ~-z 0; rlineto 0 (-z/2); rlineto z 0; rmoveto ~-z 0; rlineto 0 (-z/2); rlineto z 0; (* E *)
	rmoveto (z/2) 0; rlineto z 0; rlineto 0 (z/2); rlineto ~-z 0; rlineto 0 (z/2); rlineto z 0; (* S *)
	rmoveto (z/2) ~-z; rlineto z 0; rlineto 0 (z/2); rlineto ~-z 0; rlineto 0 (z/2); rlineto z 0; (* S *)
	moveto (3*z/4) (14*z/2); draw_string "(by AG)";
	moveto (z/2) (5*z); draw_string "Type n for a normal game, y to play white against the bot, Y to play black against the bot,";
	moveto (z/2) (19*z/4); draw_string "and b to make the bot play against itself.";
	let rec wai () =
		let e = wait_next_event [ Key_pressed ] in
		if List.mem e.key ['y'; 'Y'; 'b'; 'n'] then e.key else wai() in
	let k = wai() in
	match k with
	| 'y' -> (fun t -> t = 0) (* black is bot *)
	| 'Y' -> (fun t -> t = 1) (* white is bot *)
	| 'b' -> (fun _ -> true) (* both are bots *)
	| 'n' | _ -> (fun _ -> false) (* none are bots *)
(** the board *)
let g = Array.map explode @@ [|
	"RNBQKBNR";
	"PPPPPPPP";
	"        ";
	"        ";
	"        ";
	"        ";
	"pppppppp";
	"rnbqkbnr";
|]
(** 0 for pieces that haven't moved, 1 for pawns that did a double jump this turn, 2 for those that did so the turn before and can be enpassant'd, 3 for everyone else *)
let moved = Array.make_matrix 8 8 0
(** kingco t gets the coordinates of t's king. used to be an array, but it bugged, and I got tired of it *)
let kingco t =
	let p = if t = 1 then 'K' else 'k' in
	let rec it i j =
		if i = 8 then (-1, -1) else
		if j = 8 then it (i+1) 0 else
		if g.(i).(j) = p then (i, j) else
		it i (j+1) in
	it 0 0
(** values of pieces, as given by hans berliner. The king's value reflects that of a check, not of a checkmate (that case is treated separately) *)
let v = function
	| 'p' | 'P' -> 10
	| 'n' | 'N' -> 32
	| 'b' | 'B' -> 33
	| 'r' | 'R' -> 51
	| 'q' | 'Q' -> 88
	| 'k' | 'K' -> 5
	| ' ' | _ -> 0
(** deltas of allowed knight moves *)
let nv = [(-2,-1);(-2,1);(-1,-2);(-1,2);(1,-2);(1,2);(2,-1);(2,1)]
(** draw i j draws the board square (i, j) (and any piece on it) *)
let draw i_ j_ =
	if i_ < 0 then () else
	let c = g.(i_).(j_)
	and i = j_
	and j = i_ in
	moveto (i*z) (j*z);
	set_color (if (i+j) mod 2 = 1 then rgb 227 193 111 else rgb 184 139 74);
	fill_rect (i*z) (j*z) (z-1) (z-1);
	if c = ' ' then () else (
	piece c i j (if 'A' <= c && c <= 'Z' then white else black) false)
(** redraw the whole board *)
let drawall () = let rec it i j = if i = 8 then () else if j = 8 then it (i+1) 0 else (draw i j; it i (j+1)) in it 0 0
(** draws a colored square around a board square, to indicate it *)
let square c i j = set_color c; draw_rect (z*j+3) (z*i+3) (z-7) (z-7)
(** get a number from a piece character *)
let piecechar = function | 'p' -> 1 | 'P' -> 2 | 'r' -> 3 | 'R' -> 4 | 'b' -> 5 | 'B' -> 6 | 'q' -> 7 | 'Q' -> 8 | 'k' -> 9 | 'K' -> 10 | 'n' -> 11 | 'N' -> 12 | _ -> 0
(** compress a grid into a number *)
let compress a =
	let rec it = function
		| x::s -> x+13*(it s)
		| [] -> 0 in
	Array.to_list a |>
	Array.concat |>
	Array.to_list |>
	List.map piecechar |>
	it
(** cc c t checks if c belongs to player t *)
let cc c t = (if t = 1 then 'A' else 'a') <= c && c <= (if t = 1 then 'Z' else 'z')
(** mbpp b x s gives s with x maybe prepended if b *)
let mbpp b x s = if b then x::s else s
(** false tuplet, (-1, -1) *)
let ft = (-1,-1)
(* a ||| b gives b if a is ft *)
let (|||) a b = if a = ft then b else a
(** check t i j ex gives a square controlled by player t that is threatening square i j and is not in list ex, or ft if there are none *)
let check t i j ex =
	let ne x y = not (List.mem (x, y) ex) in
	let line p fl =
		let rec it x y fx fy =
			if x < 0 || y < 0 || x > 7 || y > 7 then ft else
			if g.(x).(y) = p && ne x y then (x, y) else
			if g.(x).(y) = ' ' then it (x+fx) (y+fy) fx fy else ft in
		List.fold_right (|||) (List.map (fun (fx, fy) -> it (i+fx) (j+fy) fx fy) fl) ft in
	if cc g.(i).(j) t then ft else
	(* pawns *) (
		let p = if t = 1 then 'P' else 'p' in
		let f = if t = 1 then 1 else -1 in
		if g.(i).(j) = ' ' then (
			if 0 > i-f || i-f >= 8 then ft else
			if g.(i-f).(j) = p && ne (i-f) j then (i-f, j) else (* forward 1 *)
			if 0 <= i-2*f && i-2*f < 8 && g.(i-2*f).(j) = p && moved.(i-2*f).(j) = 0 && ne (i-2*f) j then (i-2*f, j) else (* forward 2 *)
			if g.(i-f).(j) <> (if t = 1 then 'p' else 'P') || moved.(i-f).(j) <> 2 then ft else
			if (j-1 >= 0 && g.(i-f).(j-1) = p) && ne (i-f) (j-1) then (i-f, j-1) else (* destination square of an en passant *)
			if (j+1 < 8 && g.(i-f).(j+1) = p) && ne (i-f) (j+1) then (i-f, j+1) else (* other en passant side *)
			ft
		) else (
			if 0 <= i-f && i-f < 8 && g.(i-f).(j) = ' ' && j+1 < 8 && g.(i).(j+1)  = p && moved.(i).(j) = 2 && ne (i-f) (j+1) then (i-f, j+1) else (* getting en passant'd *)
			if 0 <= i-f && i-f < 8 && g.(i-f).(j) = ' ' && j-1 >= 0 && g.(i).(j-1) = p && moved.(i).(j) = 2 && ne (i-f) (j-1) then (i-f, j-1) else (* on the other side *)
			if 0 > i-f || i-f >= 8 then ft else
			if j+1 < 8 && g.(i-f).(j+1) = p && ne (i-f) (j+1) then (i-f, j+1) else (* diagonal eating *)
			if j-1 >= 0 && g.(i-f).(j-1) = p && ne (i-f) (j-1) then (i-f, j-1) else (* other diagonal *)
			ft
		)
	) |||
	(* knights *) (
		let p = if t = 1 then 'N' else 'n' in
		let rec it = function
		| (x, y)::s -> if 0 <= i+x && i+x < 8 && 0 <= j+y && j+y < 8 && g.(i+x).(j+y) = p && ne (i+x) (j+y) then (i+x, j+y) else it s
		| [] -> ft in
		it nv
	) |||
	(* rooks *) ( line (if t = 1 then 'R' else 'r') [(-1,0);(1,0);(0,-1);(0,1)] ) |||
	(* bishops *) ( line (if t = 1 then 'B' else 'b') [(-1,-1);(-1,1);(1,-1);(1,1)] ) |||
	(* queens *) ( line (if t = 1 then 'Q' else 'q') [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1)] ) |||
	(* kings *) (
		let p = if t = 1 then 'K' else 'k' in
		let rec it x y =
			if x = 2 then ft else
			if y = 2 then it (x+1) ~-1 else
			if (x = 0 && y = 0) || 0 > x+i || 0 > j+y || 8 <= x+i || 8 <= j+y then it x (y+1) else
			if g.(i+x).(j+y) = p && ne (i+x) (j+y) then (i+x, j+y) else it x (y+1) in
		it ~-1 ~-1
	)
(** safe t i j says if square i j is safe from player t *)
let safe t i j = if i < 0 then false else ft = check t i j []
(** wait .2 secs *)
let p() = Unix.sleepf 0.2
(** do the impure effects of a move, and return (canceling function, displaying function) *)
let effect t a b c d =
	let s = g.(a).(b) and e = g.(c).(d) in
	g.(c).(d) <- s; g.(a).(b) <- ' '; (* move it *)
	let pa = s = 'p' || s = 'P' in (* moving a pawn *)
	let ep = pa && b <> d && e = ' ' in (* en passant *)
	let passd = if ep then g.(a).(d) else ' ' in
	if ep then (g.(a).(d) <- ' '; moved.(a).(d) <- 3);
	let df = pa && abs (c-a) = 2 in (* double forward *)
	if df then moved.(a).(b) <- 1;
	let kin = s = 'k' || s = 'K' in (* moving a king *)
	let ca = kin && abs (b-d) = 2 in (* castling *)
	let f = if b < d then (d-1, 7) else (d+1, 0) in
	if ca then (g.(a).(fst f) <- g.(a).(snd f); g.(a).(snd f) <- ' ');
	((fun () -> ( (* undo the above *)
		g.(a).(b) <- g.(c).(d); g.(c).(d) <- e;
		if ep then (g.(a).(d) <- passd; moved.(a).(d) <- 2);
		if df then moved.(a).(b) <- 0;
		if ca then (g.(a).(snd f) <- g.(a).(fst f); g.(a).(fst f) <- ' ')
	)), (fun () -> ( (* display the above *)
		if ep then draw a d; (* erase what was enpassant'd *)
		if ca then draw a (fst f); draw a (snd f); (* the tower's move in castling *)
	)))
(** te t a b c d f tests the effect of move a, b -> c, d (by player t), returning f() *)
let te t a b c d f =
	let bad, good = effect t a b c d in
	let va = f() in
	bad();
	va
(** cango t i j gives where the piece in square i j could go (t's turn) *)
let cango t i j =
	let ot = (t+1) mod 2 in
	let line a b fx fy =
		let rec it c d =
			if c < 0 || d < 0 || c >= 8 || d >= 8 || cc g.(c).(d) t then [] else
			(a, b, c, d)::(if g.(c).(d) <> ' ' then [] else it (c+fx) (d+fy)) in
		it (a+fx) (b+fy) in
	let c = g.(i).(j) in
	(if cc c t then (
	match c with
	| 'p' | 'P' ->
		let f = if t = 1 then 1 else -1 in
		mbpp (0 <= i+f && i+f < 8 && g.(i+f).(j) = ' ') (i, j, i+f, j) @@ (* forward *)
		mbpp (0 <= i+2*f && i+2*f < 8 && g.(i+f).(j) = ' ' && g.(i+2*f).(j) = ' ' && moved.(i).(j) = 0) (i, j, i+2*f, j) @@ (* double forward *)
		mbpp (0 <= i+f && i+f < 8 && j-1 >= 0 && cc g.(i+f).(j-1) ot) (i, j, i+f, j-1) @@ (* standard diagonal eating *)
		mbpp (0 <= i+f && i+f < 8 && j+1 < 8 && cc g.(i+f).(j+1) ot) (i, j, i+f, j+1) @@ (* on the other side *)
		mbpp (0 <= i+f && i+f < 8 && j-1 >= 0 && cc g.(i).(j-1) ot && g.(i+f).(j-1) = ' ' && moved.(i).(j-1) = 2) (i, j, i+f, j-1) @@ (* en passant *)
		mbpp (0 <= i+f && i+f < 8 && j+1 < 8 && cc g.(i).(j+1) ot && g.(i+f).(j+1) = ' ' && moved.(i).(j+1) = 2) (i, j, i+f, j+1) [] (* on the other side *)
	| 'n' | 'N' ->
		nv |>
		List.map (fun (x, y) -> (i, j, i+x, j+y)) |>
		List.filter (fun (i, j, x, y) -> 0 <= x && x < 8 && 0 <= y && y < 8 && not (cc g.(x).(y) t))
	| 'r' | 'R' ->
		line i j 0 1 @ line i j 0 ~-1 @ line i j 1 0 @ line i j ~-1 0
	| 'b' | 'B' ->
		line i j 1 1 @ line i j 1 ~-1 @ line i j ~-1 1 @ line i j ~-1 ~-1
	| 'q' | 'Q' ->
		line i j 0 1 @ line i j 0 ~-1 @ line i j 1 0 @ line i j ~-1 0 @
		line i j 1 1 @ line i j 1 ~-1 @ line i j ~-1 1 @ line i j ~-1 ~-1
	| 'k' | 'K' ->
		let rec it x y =
			if x = 2 then [] else
			if y = 2 then it (x+1) ~-1 else
			if (x = 0 && y = 0) || x+i < 0 || y+j < 0 || x+i >= 8 || y+j >= 8 || cc g.(x+i).(y+j) t then it x (y+1) else
			(i, j, i+x, j+y)::it x (y+1) in
		it ~-1 ~-1
	| _ -> []
	) else []) |>
	List.filter (fun (a, b, c, d) ->
		te t a b c d (fun () ->
			let k = kingco t in
			safe ot (fst k) (snd k)
		)
	)
(** movelist t gets the moves that t could do *)
let movelist t =
	let rec it i j =
		if i = 8 then [] else
		if j = 8 then it (i+1) 0 else
		cango t i j @ it i (j+1) in
	it 0 0 
(** promote t c d promotes the pawn in c d, that belongs to t *)
let promote t c d =
	if dobot t then g.(c).(d) <- (if t = 1 then 'Q' else 'q') else (
	let co = if t = 1 then white else black in
	let dc = if Sys.win32 then 3 else 4 in (* draw column *)
	if Sys.unix then resize_window (si+2*z) si; (* add two columns *)
	drawall();
	set_color (if (c+d) mod 2 = 1 then rgb 227 193 111 else rgb 184 139 74); (* fill with promoting square's background *)
	fill_rect (8*z) 0 (2*z) (8*z);
	piece 'r' dc 0 co true; piece 'n' dc 1 co true; piece 'b' dc 2 co true; piece 'q' dc 3 co true; (* draw the options *)
	let rec wai() = 
		let ev = wait_next_event [ Button_down ] in let i_, j_ = ev.mouse_x, ev.mouse_y in
		let i, j = i_/z/2, j_/z/2 in
		if i <> dc || j < 0 || j >= 4 then wai() else (
			resize_window si si;
			drawall();
			g.(c).(d) <- (match (j, t) with
			| (0, 1) -> 'R' | (0, 0) -> 'r'
			| (1, 1) -> 'N' | (1, 0) -> 'n'
			| (2, 1) -> 'B' | (2, 0) -> 'b'
			| (3, 1) -> 'Q' | (3, 0) -> 'q'
			| _ -> ' ')
		) in
	wai()
	) 
(** does the impure effects of endgame. argument is the player that won, or 2 for a draw *)
let endgame n =
	set_window_title "Chess (Game over. Press any key to exit)";
	set_line_width 10;
	let score t =
		set_color (if t = 1 then white else black);
		let jz = if t = 1 then 2*z else 6*z
		and z2 = 2*z in
		if t = n then (moveto jz z2; lineto jz (6*z); lineto (jz-z) (5*z))
		else (draw_ellipse jz (4*z) z z2) in
	score 1; 
	set_color @@ rgb 128 128 128; moveto (7*z/2) (4*z); lineto (9*z/2) (4*z); (* make the dash *)
	score 0;
	ignore(read_key());
	close_graph()
(** auto t decides which move the bot should do *)
let auto t =
	let ot = (t+1) mod 2 in
	let vthreat () = (* total value of t's threatened pieces *)
		let rec it i j = if i = 8 then (0, 0) else if j = 8 then it (i+1) 0 else (
			let c = g.(i).(j) in
			let k, l = it i (j+1) in
			(if c = ' ' then (k, l) else
			if cc c t then (k+(v c)*(if not (safe ot i j) then 0 else 1), l) else
			(k, l+(v c)*(if not (safe t i j) then 0 else 1))
			)
		) in
		(it 0 0, let ki, kj = kingco ot in if safe t ki kj then 0 else if movelist ot = [] then 10000 else 0) in
	let ((art, arto), _) = vthreat () in
	let best at l = List.filter ( (<>) None ) l |>
		List.map (fun x -> match x with | None -> assert false | Some y -> y) |>
		List.fast_sort (
			(fun (_, v, ((vt, vto), ch)) (_, v_, ((vt_, vto_), ch_)) ->
			let rt, rto, rt_, rto_ = vt-art,vto-arto,vt_-art,vto_-arto in
			let ct, ct_ = v-rt+(rto/5)+ch, v_-rt_+(rto_/5)+ch_ in
			(ct-ct_)*(if at = t then 1 else -1))
		) |> List.hd in
	let mapmove f at =
		let ml = movelist at in
		if ml = [] then None else
		Some (List.map f ml |> best at) in
	(*mapmove (fun (a1, b1, c1, d1) -> te t a1 b1 c1 d1 (fun () -> (*all this commented stuff is the other two moves, didn't work out*)
		mapmove (fun (a2, b2, c2, d2) -> te ot a2 b2 c2 d2 (fun () ->*)
			mapmove (fun (a3, b3, c3, d3) -> (
				Some ((a3, b3, c3, d3), v g.(c3).(d3), te t a3 b3 c3 d3 vthreat)
			)) t
(*		)) ot
	)) t*) |>
	(fun x -> match x with | None -> assert false | Some (y, _, _) -> y)
(** wai ev waits for mouse event ev and returns the coordinates if they're in the window *)
let rec wai evc = let ev = wait_next_event [ evc ] in let x, y = (ev.mouse_y/z, ev.mouse_x/z) in if x < 0 || x >= 8 || y < 0 || y >= 8 then wai evc else (x, y)
(** legal t a b c d l h chekcs if t can do a,b -> c,d, and if so, does the move, also checks for endgame *)
let legal t a b c d l h =
	let s = g.(a).(b) and e = g.(c).(d) (* start and end squares *)
		and ot = (t+1) mod 2 in (* other turn *)
	(
		(a <> -1 && (* internal checks, for start *)
		not (cc e t) && (* not eating oneself *)
		(match s with
		| 'P' | 'p' -> (* pawns *)
			let f = if t = 1 then 1 else -1 (* the way forward *) in
			(b = d && c = a+f && e = ' ') || (* forward 1 *)
			(abs (b-d) = 1 && c = a+f && (cc e ot || (* diagonal eating *)
				(e = ' ' && cc g.(a).(d) ot && moved.(a).(d) = 2))) || (* en passant *)
			(b = d && c = a+f*2 && e = ' ' && g.(a+f).(b) = ' ' && moved.(a).(b) = 0) (** forward 2 *)
		| 'R' | 'r' -> (* rooks *)
			(a = c && (* horizontal *)
				let f = if b < d then 1 else -1 in 
				let rec it i = if i = d then true else g.(a).(i) = ' ' && it (i+f) in
				it (b+f)) ||
			(b = d && (* vertical *)
				let f = if a < c then 1 else -1 in
				let rec it i = if i = c then true else g.(i).(b) = ' ' && it (i+f) in
				it (a+f))
		| 'N' | 'n' -> (* knights *)
			let vx = c-a and vy = d-b in abs (vx*vy) = 2
		| 'B' | 'b' -> (* bishops *)
			abs (c-a) = abs(d-b) &&
				let f = ((if a < c then 1 else -1), (if b < d then 1 else -1)) in
				let rec it i j = if i = c then true else g.(i).(j) = ' ' && it (i+fst f) (j+snd f) in
				it (a+fst f) (b+snd f)
		| 'Q' | 'q' -> (* queens *)
			(a = c && (* horizontal *)
				let f = if b < d then 1 else -1 in 
				let rec it i = if i = d then true else g.(a).(i) = ' ' && it (i+f) in
				it (b+f)) ||
			(b = d && (* vertical *)
				let f = if a < c then 1 else -1 in
				let rec it i = if i = c then true else g.(i).(b) = ' ' && it (i+f) in
				it (a+f)) ||
			(abs (c-a) = abs(d-b) && (* diagonal *)
				let f = ((if a < c then 1 else -1), (if b < d then 1 else -1)) in
				let rec it i j = if i = c then true else g.(i).(j) = ' ' && it (i+fst f) (j+snd f) in
				it (a+fst f) (b+snd f))
		| 'K' | 'k' -> (* kings *)
			((abs(c-a) <= 1 && abs(d-b) <= 1) (* standard movement *)
			|| (a = c && abs (d-b) = 2 && (* castling *)
				let f = if b < d then 1 else -1 in
				let rec it i = if i = d+1 then true else g.(a).(i) = ' ' && safe ot a i && it (i+f) in
				it (b+f) &&
				moved.(a).(b) = 0 &&
				let i = if b < d then 7 else 0 in
				(g.(a).(i) = 'R' || g.(a).(i) = 'r')
				&& moved.(a).(i) = 0))
		| _ -> false) (* not going to happen - hopefully *)
	) || (
		square red a b; square red c d; p();
		draw a b; draw c d;
		false
	)
	) && (
		let bad, good = effect t a b c d in (* do all the impure effects of the move *)
		let k = kingco t in
		let ci, cj = check ot (fst k) (snd k) [] in
		if ci <> -1 then ( (* undo all if in check *)
			bad ();
			square red a b; square red c d; p();
			draw a b; draw c d;
			false
		) else ( (* else show our changes *)
			square green a b; square green c d; p();
			if (s = 'p' || s = 'P') && c = (if t = 1 then 7 else 0) then ( promote t c d );
			good();
			draw a b; draw c d;
			moved.(c).(d) <- if moved.(a).(b) = 0 then 3 else moved.(a).(b); (* log who moved & how *)
			moved.(a).(b) <- 3;
		if movelist ot = [] then endgame (let ki, kj = kingco ot in if safe t ki kj then 2 else t) (* checkmate/stalemate *) else
		if Array.for_all (Array.for_all (fun e -> e = 'K' || e = 'k' || e = ' ')) g (* only kings left *)
		then endgame 2 else
		if movelist ot = [] then endgame 2 else (* stalemate *)
		if l = 50 then endgame 2 else (* fifty move *)
		if List.mem (compress g, t, true) h then endgame 2; (* triple repetition *)
		true
		)
	)
(** di for Do It, does main loop. Arguments: the turn, the (compressed) history of the game, the number of moves since a pawn was moved or a piece was taken, possibly preloaded coordinates of the start of the move (or ft), and the last move, which should be squared in blue *)
let rec di t h l xy1 td =
	set_window_title @@ "Chess ("^(if t = 0 then "Black" else "White")^"'s turn)";
	List.iter (fun tup -> square blue (fst tup) (snd tup)) td;
	let botmove = if dobot t then auto t else (-1, -1, -1, -1) in
	let mx1, my1 = if dobot t then (
		let a, b, _, _ = botmove in (a, b)
	) else (
		if xy1 <> ft then xy1 else
		let ev = wait_next_event [ Button_down ] in (ev.mouse_y/z, ev.mouse_x/z)
	) in
	if not (cc g.(mx1).(my1) t) then (
		square red mx1 my1; p(); 
		draw mx1 my1; 
		di t h l ft td
	) else (
		square green mx1 my1;
		let xy2s = cango t mx1 my1 |> List.map (fun (a, b, c, d) -> (c, d)) in
		List.iter (fun (c, d) -> square (rgb 128 128 128) c d) xy2s;
		let hcg () = List.iter (fun (c, d) -> draw c d) xy2s in
		p();
		let mx2, my2 = if dobot t then (
			let _, _, c, d = botmove in (c, d)
		) else (
			let x2, y2 = wai Button_up in
			if x2 = mx1 && y2 = my1 then
			wai Button_down
			else x2, y2
		) in
		if cc g.(mx2).(my2) t then (draw mx1 my1;hcg();di t h l (mx2, my2) td) else
		let ot = (t+1) mod 2 in
		let a, b, c, d = mx1, my1, mx2, my2 in
		let s = g.(a).(b) and e = g.(c).(d) in
		let nt = if (legal t a b c d l h) then ot else t in
		List.iter (fun tup -> draw (fst tup) (snd tup)) td;
		if nt <> t then (
			let rec it i j = if i = 8 then () else if j = 8 then it (i+1) 0 else (moved.(i).(j) <- (match moved.(i).(j) with | 1 -> 2 | 2 -> 3 | x -> x); it i (j+1)) in it 0 0); (* update moved *)
		hcg();
		di nt
			(if nt <> t then (let comp = compress g in if List.mem (comp, t, false) h then (comp, t, true)::h else (comp, t, false)::h) else h)
			(if nt <> t && s <> 'P' && s <> 'p' && e = ' ' then l+1 else 0)
			ft
			(if nt <> t then [(a, b);(c, d)] else td)
		)
let () = drawall(); Unix.sleep 1; di 1 [] 0 ft []
