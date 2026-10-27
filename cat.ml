(** Cellular Automata Terminal: simulator for outer-totalistic 2D automata, including interface and commands. Requires Graphics module, xclip, and generally expects to be on linux. *)
open Graphics

(** list of lines to display for help *)
let help_first_half = [
	"Hello and welcome to my Cellular Automata Terminal (CAT).";
	"";
	"You will have the status in the top left.";
	"When CAT is running and you press space, it will stop and go to pause.";
	"When in pause, you can press keys to do things:";
	" - spacebar     : play/pause depending on status.";
	" - f (Forward)  : simulate new generations";
	" - n (Next)     : goes one frame further and then back to pause.";
	" - b (Backward) : play but in reverse, shows you the state of before.";
	" - p (Previous) : goes one frame backwards and back to pause.";
	" - e (Edit)     : lets you manually change the state of cells by clicking on them.";
	"                 You can undo the last edit with p (Previous).";
	"                 To exit editing mode, press o (Out).";
	" - r (Randomise): generates a new random grid.";
	" - x (Cross)    : generates a new random grid symmetric in both directions";
	" - d (Delete)   : sets all the grid to black.";
	" - h, j, k, l   : left, down, up, right scroll (from the vim shortcuts)";
	" - z, u (Zoom)  : makes cells biger (smaller for u)";
	"h, j, k, l, and z, u are also available when running or editing.";
	" - m (Move)     : try to follow an evolving area in the center (typically a ship).";
	"                 Stops whenever a cell on the edge of the screen is alive.";
	" - g (Grid)     : toggle grid lines";
	" - w (Wrap)     : toggle periodic boundary conditions (ie torus). This will reset the grid.";
	"                 Scroll and zoom are unavailable in torus mode.";
	" - s (Show)     : toggle display of cells. (No display is faster.)";
	" - c (Cap)      : set max fps. 0 for unlimited. Default is 60.";
	" - t (Ticks)    : set the number of ticks the simulator advances in each frame.";
	" - (drag-)click : drag-click over an area to select it.";
	"                 Selection is automatically trimmed to live cells if there are any.";
	"                 You can then:";
	"                 a (All)       select everything";
	"                 r (Randomize) its cells";
	"                 d (Delete)    ie set all cells to black (also copies)";
	"                 h, j, k, l    move the selection (overwriting what was where it's going)";
	"                 t (Turn)      rotate the selection by 90 degrees clockwise";
	"                 c (Copy)      the selection to clipboard as RLE";
	"                 s (Save)      the selection into an RLE file (also copies)";
	"                 You can exit selection at any point with o (Out)."
]
let help_second_half = [
	" - v (Paste)    : insert RLE from clipboard with the left corner at mouse position";
	" - o (Open)     : load grid from an RLE file";
	" - a (Add)      : insert RLE from file with lower left at mouse position";
	" - i (Info)     : shows this. Go back to pause by pressing any key";
	" - q (Quit)     : Does Exactly What It Says On The Tin. (Goes back to the CA choice screen.)";
	"                 Put Q again while choosing CA to close CAT.";
]

(** a cell's state is identified to a positive integer *)
type state = int

(** a quadrant is a square matrix of states (here stored as a 1D array) (immutable ideally but not on 5.4 yet) *)
type quadrant = state array

(** we subdivide the state space into square quadrants of identical sizes,
	and mark each active or inactive *)
type grid = {
	q: ((int * int, quadrant * bool) Hashtbl.t); (** quadrant coordinates in the plane to (matrix, activeness). *)
	si: int; (** each quadrant's width/height in cells *)
	tor: (int * int) option; (** [None] if using unbounded grid, [Some (w, h)] if using bounded grid, where [w, h] are the height and width in quadrants *)
}

(** format used to record changes to the grid, where the snds are the absolute cell change location,
	and the fsts are the {i previous} state. *)
type change_list = (state * (int * int)) list

(** a history of changes. the fsts are the number of steps to be counted back *)
type history = (int * change_list) list

(** a neighbourhood is a list of parts, each of which is a list of relative coordinates.
	for most use cases, only one part will be needed *)
type neighbourhood = (int * int) list list

(** a transition function takes the list of the states of each neighbour, the current state, and returns the new state *)
type transition_func = state list list -> state -> state

(** record of parameters needed to describe an automaton *)
type automaton = {
	f: transition_func;
	n: neighbourhood;
	c: color array
}

(** record type for the internal state of the simulator, to make calls less annoying *)
type params = {
	g: grid;
	f: transition_func;
	n: neighbourhood;
	col: color array;
	rule: string; (** for rle *)
	h: history;
	u: int; (** u, v are the cell offsets of the lower left corner *)
	v: int;
	z: int; (** the size of a cell in pixels *)
	gl: bool; (** whether to draw the grid lines *)
	sp: int; (** speed cap in fps *)
	fo: bool; (** whether to try and follow the living blob *)
	t: int; (** simulation time (since origin) *)
	sh: bool; (** whether to draw cells *)
	inc: int; (** increment, number of steps we advance by *)
	b: bool; (** going backwards? *)
}

(** {1 Grids} *)

(** implement actual euclidian integer division, where the rest is positive and we have:
	[p = (divi p q)*q + modu p q]. *)
let divi p q =
	if p >= 0 then p/q
	else ((p-(q-1))/q)

(** see {!divi} *)
let modu p q = p-q*(divi p q)

(** [grid_new si tor] creates a new {!grid} with no quadrants, quad size set to [si] and toroidal settings [tor] *)
let grid_new si tor = {q = Hashtbl.create 1; tor = tor; si = si}

(** [grid_copy g] returns a {!grid} with the internal hashtable copied *)
let grid_copy g =
	let h = Hashtbl.copy g.q in
	{g with q = h}

(** [grid_wrap g (a, b)] wraps around the quadrants numbers if out of window and [g.tor] is not [None] *)
let grid_wrap g (a, b) = match g.tor with
	| None -> a, b
	| Some (da, db) -> modu a da, modu b db

(** [grid_get g i j] gets the value of cell [i, j] in [g] *)
let grid_get g i j =
	match Hashtbl.find_opt g.q (grid_wrap g (divi i g.si, divi j g.si)) with
	| None -> 0
	| Some (q, _) -> q.((modu i g.si)*g.si+modu j g.si)

(** [grid_quad g a b] gets the matrix of quadrant [a, b] in [g] *)
let grid_quad g a b =
	match Hashtbl.find_opt g.q (grid_wrap g (a, b)) with
	| None -> Array.make (g.si*g.si) 0
	| Some (q, _) -> q

(** [grid_is_active g a b] gets the activity boolean *)
let grid_is_active g a b =
	match Hashtbl.find_opt g.q (grid_wrap g (a, b)) with
	| None -> false
	| Some (_, b) -> b

(** [grid_set g i j k] sets [i, j]'s state to [k] in [g]. *)
let grid_set g i j k =
	(* we need to copy else quadrants are shared between generations *)
	let a = divi i g.si 
	and b = divi j g.si
	and x = modu i g.si
	and y = modu j g.si in
	let m = Array.copy (grid_quad g a b) in
	m.(x*g.si+y) <- k;
	Hashtbl.replace g.q (grid_wrap g (a, b)) (m, true)

(** [grid_remove quad g a b] removes the quad [a, b] of [g] *)
let grid_remove_quad g a b =
	Hashtbl.remove g.q (grid_wrap g (a, b))

(** [grid_mark_active g (a, b) bo] sets the activity of quad [a, b] in [g] to [bo] *)
let grid_mark_active g (a, b) bo =
	Hashtbl.replace g.q (grid_wrap g (a, b)) (grid_quad g a b, bo)

(** [grid_activate_adjacent g (a, b)] marks the quads within a moore distance of 1 of [a, b] active in g *)
let grid_activate_adjacent g (a, b) =
	for da = -1 to 1 do
		for db = -1 to 1 do
			grid_mark_active g (a+da, b+db) true
		done
	done

(** [grid_activate_cell_rect g x y dx dy] marks the cells of the rectangle
	with lower left at [x, y] and upper right at [x+dx, y+dy] as active.
	Only marks the borders of the rectangle. *)
let grid_activate_cell_rect g x y dx dy =
	(* mark the quads that overlap or border the [x, x+dx] [y, y+dy] cell rectangle
	 considering strictly the rectangle borders, not its inside *)
	for a = divi x g.si - 1 to divi (x+dx) g.si + 1 do
		grid_mark_active g (a, divi y g.si - 1) true;
		grid_mark_active g (a, divi (y+dy) g.si + 1) true
	done;
	for b = divi y g.si - 1 to divi (y+dy) g.si + 1 do
		grid_mark_active g (divi x g.si - 1, b) true;
		grid_mark_active g (divi (x+dx) g.si + 1, b) true
	done

(** [grid_trim g x y dx dy] returns [i, j, di, dj] such that
	the [i, j] to [i+di, j+dj] rectangle is the smallest subrectangle
	containing all live cells in [x, y] to [x+dx, y+dy] in g.
	if there are no live cells returns x, y, 0, 0 *)
let grid_trim g x y dx dy =
	let test r r_lim s ds pa mo =
		(* does mo r where mo is incr/decr and r is a ref,
		 as long as the cells pa s, ..., pa s+ds are all black
		 and !r <> r_lim *)
		while (
			let check = ref true in
			for k = s to s+ds do
				let (n, m) = pa k in
				if grid_get g n m <> 0
				then check := false
			done;
			!check && !r <> r_lim) do
			mo r
		done
	in
	(* first decrease width & height *)
	let dx_ = ref dx in
	test dx_ 0 y dy (fun j -> x + !dx_, j) decr;
	let dy_ = ref dy in
	test dy_ 0 x dx (fun i -> i, y + !dy_) decr;
	(* then increase left & bottom *)
	let x_ = ref x in
	test x_ (x + !dx_) y dy (fun j -> !x_, j) incr;
	dx_ := !dx_ - (!x_ - x);
	let y_ = ref y in
	test y_ (y + !dy_) x dx (fun i -> i, !y_) incr;
	dy_ := !dy_ - (!y_ - y);
	!x_, !y_, !dx_, !dy_

(** {1 Interface} *)

(** height of the bar for status etc, in pixels *)
let text_height = 20

(** actual y size, excluding status bar *)
let height () = size_y () - text_height

(** for consistency with {!height} *)
let width () = size_x ()

(** [write l x y] writes a list of strings one line each with the top one starting at [x, y],
	in color [co] (and black bg) *)
let rec write l x y co =
	match l with
	| [] -> ()
	| h::t ->
		set_color black;
		fill_rect x y (6*String.length h) 19 (* to overlay the text on top of the CA, more pretty *);
		moveto x y; set_color co; draw_string h;
		write t x (y-text_height) co

(** [top_left s co] erases the first (horizontal) half of the {!text_height} top pixels and writes [s] with color [co] at the top left *)
let top_left s co =
	set_color black; fill_rect 0 (height ()) (width () / 2) text_height;
	write [s] 10 (height ()) co

(** [status s] {!top_left}s text with dashes on the side *)
let status s = top_left (" - "^s^" - ") cyan

(** [error s] {!top_left}s text in red and all-caps with [ERROR:] in front *)
let error s = top_left ("ERROR: "^(String.uppercase_ascii s)) red

(** same but for the second half, and texts start at middle of screen *)
let top_right s co =
	set_color black; fill_rect (width () / 2) (height ()) (width () / 2) text_height;
	write [s] (width () / 2 + 10) (height ()) co

(** [show_state s] {!top_right}s the current generation, max speed and torus *)
let show_state s =
	top_right (String.concat " -- "
		["time : " ^ string_of_int s.t;
		"max speed : " ^ (if s.sp = 0 then "unlimited" else string_of_int s.sp ^ "fps");
		"torus : " ^ (if s.g.tor = None then "no" else "yes");
		"display : " ^ (if s.sh then "yes" else "no");
		"tick increment : " ^ (string_of_int s.inc)]
	) cyan

(** [input autoc mess] reads text from the homemade input field,
	where autoc is an array of possibilities, and mess is the prompt displayed to the user *)
let input autoc mess = 
	(* s is the fields contents *)
	top_left "Awaiting input..." yellow;
	top_right mess cyan;
	let rec input_aux s =
		let st = wait_next_event [Key_pressed] in
		let c = st.key in 
		if c = '\013' then (top_right "" white; top_left "" white; s) (* enter *)
		else if c = '\008' then ( (* backspace *)
			let ns = if s = "" then s else (String.sub s 0 (String.length s - 1)) in
			Unix.sleepf 0.1;
			top_left ns yellow; input_aux ns
		) else if c = '\009' then ( (* tab *)
			(* take the first value in autoc that starts by s *)
			match Array.find_opt (String.starts_with ~prefix:s) autoc with
			| None -> input_aux s
			| Some t ->
				Unix.sleepf 0.1;
				top_left t yellow;
				input_aux t
		) else (
			Unix.sleepf 0.1;
			top_left (s^(String.make 1 c)) yellow;
			input_aux (s^(String.make 1 c))
		)
	in String.trim (input_aux "")

(** fill the cell space with black *)
let erase_all () = set_color black; fill_rect 0 0 (width ()) (height ())

(** [small_zoom s] gives whether the z is small enough for us to change behaviour *)
let small_zoom s = s.z < 4

(** width of visible space (in cells). for torus is smaller *)
let cells_width s =
	match s.g.tor with
	| None -> width ()/s.z
	| Some (da, _) -> s.g.si*da
(** height of visible space (in cells). for torus is smaller *)
and cells_height s =
	match s.g.tor with
	| None -> height ()/s.z
	| Some (_, db) -> s.g.si*db

(** [draw i j c s] draws a cell, aka a square. [c] is the color, [s] is the state (for some params) *)
let draw i j c s =
	let x = (i-s.u)
	and y = (j-s.v) in
	if s.sh && (
		s.g.tor <> None
		|| (0 <= y && y < cells_height s && 0 <= x && x < cells_width s)
	) then (
		set_color s.col.(c);
		let o = if s.gl && not (small_zoom s) then 1 else 0 in
		fill_rect (x*s.z+o) (y*s.z+o) (s.z-1-o) (s.z-1-o);
	)

(** draws the grid lines, not at very low zoom *)
let draw_grid s =
	if s.sh && s.gl && not (small_zoom s) then begin
		set_color (rgb 32 32 32);
		for x = 0 to cells_width s do
			let x = if s.g.tor <> None then x + 100/s.z else x in
			if modu (x+s.u) 10 = 0 then set_color (rgb 64 64 64);
			if modu (x+s.u) 10 = 1 then set_color (rgb 32 32 32);
			moveto (x*s.z) 0;
			lineto (x*s.z) (height ());
		done;
		set_color (rgb 32 32 32);
		for y = 0 to cells_height s do
			let y = if s.g.tor <> None then y + 100/s.z else y in
			if modu (y+s.v) 10 = 0 then set_color (rgb 64 64 64);
			if modu (y+s.v) 10 = 1 then set_color (rgb 32 32 32);
			moveto 0 (y*s.z);
			lineto (width ()) (y*s.z)
		done
	end

(** [redraw_all s] redraws all cells in [s.g] and the grid lines where applicable *)
let redraw_all s =
	auto_synchronize false;
	erase_all ();
	let si = s.g.si in
	Hashtbl.iter (fun (a, b) (q, _) ->
		for x = 0 to si - 1 do
			for y = 0 to si - 1 do
				draw (a*si+x) (b*si+y) q.(x*si+y) s
			done;
		done;
	) s.g.q;
	draw_grid s; (* drawing the grid is actually really expensive *)
	auto_synchronize true

(** {1 Pattern saving/loading} *)

(** [to_rle g x y dx dy r] makes an RLE string for the cells [x, y] to [x+dx, y+dy] of [g], with [r] the rulestring *)
let to_rle g x y dx dy r =
	let rec string_of_state k =
		(* character choice taken from golly for interoperability *)
		if k = 0 then "b"
		else if k = 1 then "o"
		else if k <= 24 then String.make 1 (char_of_int (65 + k - 1))
		else let hi = (k-24)/24 in (String.make 1 (char_of_int (112 + hi)))^string_of_state (k - (hi+1)*24)
	in
	let b = Buffer.create (dx*dy) in
	let cur = ref "!"
	and ti = ref 0 in
	let add_token () =
		if !ti <= 0 then () else
		let o = if !ti = 1
		then !cur
		else (string_of_int (!ti)) ^ (!cur)
		in
		Buffer.add_string b o
	in
	let line_end () =
		if !cur = "$"
		then incr ti
		else (
			if !cur <> "!" && !cur <> string_of_state 0
			then add_token ();
			cur := "$"; ti := 1
		)
	in
	let use_new k =
		let z = string_of_state k in
		if !cur = z
		then incr ti
		else (
			if !cur <> "!"
			then add_token ();
			cur := z; ti := 1
		)
	in
	(* we have two algorithms, which can be more or less effective depending on the grid.
	   Either we iterate over all the cells in the rectangle: O(dx*dy),
	   or we iterate over all the active cells: O(#g.q) (plus some other stuff but negligible.) *)
	if dx*dy < Hashtbl.length g.q || true then (
		for j = y + dy -1 downto y do
			let empty = Array.for_all (fun i -> grid_get g i j = 0) (Array.init dx (fun k -> k+x)) in
			if not empty then (
				for i = x to x + dx - 1 do
					(* we function on columns but rle functions on lines (plus our lines start from bottom) *)
					(* this is the costy part, because we're requesting a lot of cells which,
					   as it happens, may have no remote possibility of being alive *)
					use_new (grid_get g i j);
				done;
			);
			line_end ()
		done;
	) else (
		Hashtbl.to_seq g.q
		(* filter out chunks which surely can't be it, but this may keep a few redundant ones on the edges.
		 this is the costy part of this algorithm: iterating over the whole of g *)
		|> Seq.filter (fun ((a, b), _) ->
			x/g.si <= a && a <= (x+dx)/g.si &&
			y/g.si <= b && b <= (y+dy)/g.si
		)
		|> List.of_seq
		(* sort by decreasing b, then decreasing a. the way group is made will flip a's again to increasing *)
		|> List.sort (fun ((a, b), _) ((c, d), _) -> -2*(compare b d) - compare a c)
		(* to actually read a RLE line in a row (which is fixed j, increasing i),
		   we need to read one line of each quadrant that has this b,
		   then the next lines for those quadrants, etc
		   so we group by b *)
		|> (let rec group gr l =
			match gr, l with
			| [], [] -> []
			| [], h::t -> group [h] t
			| _, [] -> [gr]
			| x::_, h::t ->
				(* same b line *)
				if snd (fst h) = snd (fst x)
				then group (h::gr) t
				else (gr :: group [h] t)
			in group [])
		|> (fun l -> List.fold_left (fun last_b gr ->
			let b = snd (fst (List.hd gr)) in
			(* add $s for empty lines *)
			if !cur = "$"
			then ti := !ti + (last_b - b - 1)*g.si
			else (
				add_token ();
				cur := "$";
				ti := (last_b - b - 1)*g.si
			);
			for j = g.si - 1 downto 0 do
				ignore(List.fold_left (fun last_a ((a, _), (q, _)) ->
					(* add bs for empty cells since the last quad *)
					if last_a < a - 1 then (
						if !cur = string_of_state 0
						then ti := !ti + (a - 1 - last_a)*g.si
						else (
							add_token ();
							cur := string_of_state 0;
							ti := (a - 1 - last_a)*g.si
						)
					);
					for i = 0 to g.si - 1 do
						use_new q.(i*g.si+j)
					done;
					a
				) (x/g.si - 1) gr);
				line_end ();
			done;
			b
		) (snd (fst (List.hd (List.hd l)))) l)
		|> ignore;
	);
	if !cur <> "!" && !cur <> "$"
	then add_token ();
	"x = "^(string_of_int dx)^
	", y = "^(string_of_int dy)^
	", rule = "^r^"\n"^(Buffer.contents b)^"!"

(** trying to parse as RLE something which isn't *)
exception Not_rle

(** [from_rle s g x y] interprets [s] as an RLE string,
	and adds its contents in grid [g] with the left corner at [x, y]
 	@raise Not_rle if s can't be made sense of as an RLE string *)
let from_rle s g x y =
	try begin
	let l = String.split_on_char '\n' s in
	let md = List.hd l
		|> String.split_on_char '='
		|> List.map (fun s -> String.trim (if String.exists ((=) ',') s then String.sub s 0 (String.index s ',') else s))
		|> List.tl
		|> Array.of_list
	in
	let dx = int_of_string md.(0) 
	and dy = int_of_string md.(1) in
	let r = String.concat "" (List.tl l) in
	let k = ref 0 in
	let i = ref 0 in
	let j = ref 0 in
	let n = ref 0 in
	let c = ref [] in
	let toactivate = Hashtbl.create 1 in
	while (!k < String.length r) do
		(match r.[!k] with
		| '0'..'9' -> n := !n*10 + (int_of_char r.[!k] - 48)
		| '$' -> j := !j + (max !n 1); i := 0; n := 0
		| 'b'  -> i := !i + (max !n 1); n := 0
		(* character choice taken from golly for interoperability *)
		| 'o'..'y' | 'A'..'X' ->
			let co =
				if r.[!k] = 'o' then 1
				else if r.[!k] < 'o' then int_of_char r.[!k] - 65 + 1
				else 24*(int_of_char r.[!k] - 112 + 1) + (
					incr k;
					assert ('A' <= r.[!k] && r.[!k] <= 'X');
					int_of_char r.[!k] - 65 + 1
				)
			in
			let i_lim = !i+(max !n 1) in
			while !i < i_lim do
				let i_ = x + !i
				and j_ = y + dy - 1 - !j in
				c := (grid_get g i_ j_, (i_, j_))::(!c);
				grid_set g i_ j_ co;
				Hashtbl.replace toactivate (divi i_ g.si, divi j_ g.si) true;
				incr i
			done;
			n := 0
		| '!' -> ()
		| _ -> assert false);
		incr k
	done;
	Hashtbl.iter (fun (a, b) _ -> grid_activate_adjacent g (a, b)) toactivate;
	!c, dx, dy
	end
	with _ -> raise Not_rle

(** [set_clipboard s] copies [s] to clipboard, using xclip. *)
let set_clipboard s =
	let oc = Unix.open_process_out "xclip -selection clipboard -in" in
	output_string oc s; flush oc;
	close_out oc

(** get clipboard contents, using xclip *)
let get_clipboard () =
	try
		let ic = Unix.open_process_in "xclip -selection clipboard -out" in
		let s = In_channel.input_all ic in
		close_in ic;
		s
	with _ -> ""

(** gives the sorted list of the filenames (with extension) in cwd *)
let get_rle_files () =
	Sys.readdir "."
	|> Array.to_list
	|> List.filter (String.ends_with ~suffix:".rle")
	|> Array.of_list

(** get a file name using {!input} and normalize it to .rle *)
let get_file_name () = 
	let s = String.trim (input (get_rle_files ()) "enter file name, put nothing to cancel") in
	if s = "" then None else
	if String.ends_with ~suffix:".rle" s
	then Some s
	else Some (s ^ ".rle")

(** [save_to_file s name] saves string [s] to [.rle] file [name] *)
let save_to_file s name =
	let oc = open_out name in
	output_string oc s;
	close_out oc

(** [read_from_file name] gets the contents of file [name] as a string. *)
let read_from_file name =
	let ic = open_in name in
	let s = In_channel.input_all ic in
	close_in ic;
	s

(** {1 Automata configurations} *)

(** [frb64 c] gets a number out of a base 64 digit. <{i base64digit}> ::= [0]...[9] | [A]...[Z] | [a]...[z] | [+] | [_] *)
let rec frb64 c =
	match c with
	| '0'..'9' -> int_of_char c - 48
	| 'A'..'Z' ->int_of_char c - 65 + 10
	| 'a'..'z' -> int_of_char c - 97 + 36
	| '+' -> 62
	| '_' -> 63
	| _ -> raise (Invalid_argument "frb64")

(** [explode s] gives the list of the characters of [s] *)
let explode s =
	let rec aux i =
		if i = String.length s
		then []
		else s.[i]:: aux (i+1)
	in aux 0

(** [count l c] counts the number of occurrences of [c] in [l] *)
let rec count l c = match l with 
	| [] -> 0
	| i::s -> if i = c then 1 + count s c else count s c

(** [bs bi su] returns a function by a method adjacent to the Bbi/Ssu notation ({{:https://conwaylife.com/wiki/Life-like_cellular_automaton#Notation}on lifewiki}) *)
let bs bi su : transition_func = 
	let test s = (* returns function that tests whether a count matches s *)
		if s = "" then (fun _ -> false) else
		let parts = String.split_on_char ',' s in
		(fun c -> List.exists 
		(fun ss -> if String.contains ss '-' (* set *)
			then (
				let bo = Array.of_list @@ List.map int_of_string @@ String.split_on_char '-' ss
				in bo.(0) <= c && c <= bo.(1))
			else int_of_string ss = c) (* single value *)
		parts) in
	let bi_ = test bi and su_ = test su in
	(fun l v ->
		let c = count (List.hd l) 1 in
		if v = 0 then
			if bi_ c then 1 else 0
		else
			if su_ c then 1 else 0
	)

(** parse [s] as a rule string either with k states, and return a {!transition_func}. \n
 	if [s] contains a [/], then it is parsed by {!bs}. \\
	else, it is parsed with the following format:

	<{i number}> ::=

		<{i base64digit}>

		| ['] <{i base64digit}> <{i base64digit}> Two-digit base64 number

	<{i function}> ::=

		<{i number}> Constant function

		| [!] Value of current cell
		
		| [#] <{i number}> Count of neighbours of given state

		| [@] <{i number}> <{i number}> Count of neighbours of the state (second number) in neighbourhood section (first number)

		| ( [|] | [&] ) <{i function}> <{i function}> Boolean or and and of functions returning 0 or 1

		| ( [<] | [=] | [>] ) <{i function}> <{i function}> Comparison of the result of two functions (returns 0 or 1)
	
		| [?] <{i function}> <{i function}> <{i function}> If the first function returns 1, then return the result of the second, else return the result of the third.

	All integers are supposed to be smaller than [64^2 = 4096].
*)
let parse_rule s k : transition_func =
	(* int function of bool function of two variables*)
	let iobf f = fun x y -> if f x y then 1 else 0 in
	(* parses the first tokens of the list into
		the function built by the first operator,
		and the list of remaining tokens
	the function built here takes as parameter not the actual states, but the counts (so that said counts don't get recounted) *)
	let rec parse_aux = function
		| [] -> raise (Invalid_argument "parse_rule")
		| h::t -> (match h with
			| '0'..'9' | 'a'..'z' | 'A'..'Z' | '+' | '_' -> (fun _ _ -> frb64 h), t
			| '!' -> (fun _ v -> v), t
			| '#' -> let f, r = parse_aux t in
				(fun c v -> c.(0).(f c v)), r
			| '>' | '<' | '=' | '|' | '&' | '\'' | '@' ->
				let f, r = parse_aux t in
				let g, s = parse_aux r in
				(fun c v -> (match h with
					| '>' -> iobf (>)
					| '<' -> iobf (<)
					| '=' -> iobf (=)
					| '|' -> (lor)
					| '&' -> (land)
					| '\'' -> fun x y -> 64*x+y
					| '@' -> (fun x y -> c.(x).(y))
					| _ -> assert false
				) (f c v) (g c v)), s
			| '?' ->
				let f, r = parse_aux t in
				let g, s = parse_aux r in
				let h, e = parse_aux s in
				(fun c v -> if (f c v = 1) then (g c v) else (h c v)), e
			(* skip (lets people put parentheses, spaces) *)
			| _ -> parse_aux t
		)
	in 
	if String.exists ((=) '/') s
	then
		let i = String.index s '/' in
		bs (String.sub s 1 (i-1)) (String.sub s (i+2) (String.length s - i - 2))
	else 
		let get_counts l =
			l
			|> List.map (fun p ->
				let a = Array.make k 0 in
				List.iter (fun i -> a.(i) <- a.(i)+1) p;
				a)
			|> Array.of_list
		in
		fun l v -> (fst @@ parse_aux @@ explode s) (get_counts l) v

(** Moore neighbourhood of range given *)
let moore r : neighbourhood = 
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
	[List.concat (it 1 0)]

(** Neumann neighbourhood of range given *)
let neumann r : neighbourhood =
	let rec it i j =
		if i = (r+1) then [] else
		if j = (r-(abs i)+1) then it (i+1) (-r+(abs (i+1))) else
		if i = 0 && j = 0 then it i (j+1)
		else (i, j)::it i (j+1)
	in [it (-r) 0]

(** Cross neighbourhood of range given *)
let cross r : neighbourhood = 
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
	[List.concat (it r)]

(** Saltire (diagonal cross) neighbourhood of range given *)
let saltire r : neighbourhood =
	let rec it k =
		if k = 0 then [] else
			(-k, -k)::(-k, k)::(k, -k)::(k, k)::it (k-1)
	in [it r]

(** default settings for an automatan *)
let default_automata = {
	f = (fun _ _ -> 0);
	n = moore 1;
	c = [|black; white|]
}

(** [radius n] gets the biggest offset in [n] *)
let radius (n: neighbourhood) =
	List.fold_left
		(fun m (di, dj) -> max m (max (abs di) (abs dj)))
		0
		(List.concat n)

(** automata name to automata config, read from [.cat_config] *)
let config =
	let this = Hashtbl.create 1 in
	let tup_of_string s =
		String.split_on_char ',' s
		|> (fun l -> (int_of_string (List.hd l), int_of_string (List.nth l 1))) in
	read_from_file ".cat_config"
	|> String.split_on_char '*'
	|> List.tl
	|> List.iter (fun s ->
		let l = String.split_on_char '\n' (String.trim s) in
		let name = List.hd l in
		List.fold_left
			(fun (aut: automaton) s ->
				let v = try String.sub s 3 (String.length s-3)
						with _ -> raise (Invalid_argument "line too short in .cat_config")
				in
				match s.[0] with
				| 'f' -> {aut with f = parse_rule v (Array.length aut.c)}
				| 'n' -> let r = String.sub v 1 (String.length v - 1) in
					{aut with n = match v.[0] with
						| 'M' -> moore (int_of_string r)
						| 'N' -> neumann (int_of_string r)
						| '+' -> cross (int_of_string r)
						| '*' -> saltire (int_of_string r)
						| _ ->
						v
						|> String.split_on_char '@'
						|> List.map (String.split_on_char ';')
						|> List.map (List.map tup_of_string)
					}
				| 'c' ->
						{aut with c = if v = "RGB" then [|black; red; green; blue|] else
						v
						|> String.split_on_char ';'
						|> List.map int_of_string
						|> Array.of_list
					}
				| _ -> raise (Invalid_argument "empty line in .cat_config")
			)
			default_automata
			(List.tl l)
		|> Hashtbl.add this name;
	);
	this

(** Displays information about CAT *)
let info () = write help_first_half 50 (height () - 50) white;
	write (help_second_half @
	["";
	"Available CA are:"
	]@
	[
		Hashtbl.to_seq_keys config
		|> List.of_seq
		|> List.sort Stdlib.compare
		|> String.concat ", "
	]
	@["Put Q while choosing CAs to close CAT."])
	(width () / 2 + 50) (height () - 50) white

(** {1 Simulation} *)

(** associates to a character the displacement (from vim shortcuts) *)
let char_to_delta = function
	| 'h' -> (-1, 0)
	| 'j' -> (0, -1)
	| 'k' -> (0, 1)
	| 'l' -> (1, 0)
	| _ -> (0, 0)

(** [scroll s m] translates s with input m *)
let scroll s m =
	let (du, dv) = char_to_delta m in
	if s.g.tor <> None then (error "un(zoom) and scroll not available on a torus"; s)
	else (
		let du = du*(max 1 (30/s.z))
		and dv = dv*(max 1 (30/s.z)) in
		let ns = {s with u = s.u+du; v = s.v+dv} in
		redraw_all ns;
		ns
	)

(** [zoom s m] (un)zooms s with input m *)
let zoom s m =
	let dz = match m with
		| 'u' -> -1
		| 'z' -> 1
		| _ -> 0
	in
	if s.g.tor <> None then (error "un(zoom) and scroll not available on a torus"; s)
	else (
		if s.z <= 1 then (error "already at minimum zoom"; s)
		else (
			let du, dv =
				(width ()) * dz / (2*s.z*(s.z+dz)),
				(height ()) * dz / (2*s.z*(s.z+dz))
			in
			let ns = {s with u = s.u+du; v = s.v+dv; z = s.z+dz} in
			redraw_all ns;
			ns
		)
	)

(** factorises common logic to forward and backward *)
let rec direction main b err s once =
	let start = Unix.gettimeofday() in
	if s.sh then auto_synchronize false;
	try begin
		let g_ = grid_copy s.g in
		let ns = main s g_ in
		show_state ns;
		if ns.sh then auto_synchronize true;
		(* time we should have taken but didn't *)
		if ns.sp > 0 then (
			let dt = 1. /. (float_of_int ns.sp) +. start -. Unix.gettimeofday () in
			if dt > 1e-4 then (* Unix.sleepf has a ~0.1ms overhead *)
				Unix.sleepf dt
		);
		let m = if key_pressed () then read_key() else '\005' in
		if once || m = ' '
		then (status "pause"; pause {ns with b = b})
		else
		direction main b err (match m with
		| 'h' | 'j' | 'k' | 'l' -> scroll ns m
		| 'u' | 'z' -> zoom ns m
		| _ -> ns) false
	(* for backward *)
	end with Failure _ -> err s

(** [forward s once] moves [s.inc] generation after state [s], then stops if [once] *)
and forward s once =
	let real_main s g_ =
		let {g; f; n; col; rule; h; u; v; z; gl; sp; fo; t; sh} = s in
		let toactivate = Hashtbl.create 1 in
		let change = ref [] in
		Hashtbl.iter (fun (a, b) (q, t) ->
			let changed = ref false in
			if not t then () else
			(* go apply rules and display if active *)
			let q_ = Array.mapi (fun p k ->
				let x = divi p g.si
				and y = modu p g.si in
				let i = a*g.si+x
				and j = b*g.si+y in
				let rec get_values l =
					(* applies the relatives coordinates in l
					   to the coordinates of the current cell to get the states.*)
					match l with
					| [] -> []
					| hd::tl -> grid_get g (i+fst(hd)) (j+snd(hd)) :: get_values tl
				in
				let k_ = f (List.map get_values n) k in (* new cell state *)
				if k <> k_ then begin (* do nothing if unchanged *)
					changed := true;
					draw i j k_ s;
					change := (k, (i, j))::!change (* add this change to the changelist *)
				end;
				k_
			) q in
			(* deal with quadrant activity *)
			if !changed
			then (
				Hashtbl.replace g_.q (a, b) (q_, grid_is_active g a b);
				Hashtbl.replace toactivate (a, b) true
			) else
				(* this quad is inactive *)
				if Array.exists ((<>) black) (grid_quad g_ a b)
				then grid_mark_active g_ (a, b) false
				else grid_remove_quad g_ a b
		) g.q;
		Hashtbl.iter (fun (a, b) _ -> grid_activate_adjacent g_ (a, b)) toactivate;
		let ns = {s with g = g_; h = (1, !change)::h; t = t+1} in
		if fo && g.tor = None
		then (
			let w, h = cells_width s, cells_height s in
			let x, y, dx, dy = grid_trim g_ u v w h in
			if x = u || y = v || x + dx = u + w || y + dy = v + h (* touching the edge *)
			then {ns with fo = false}
			else (
				let du = x - (w - dx)/2 - u
				and dv = y - (h - dy)/2 - v in
				if du = 0 && dv = 0
				then ns
				else (
					let ns = {ns with u = u+du; v = v+dv} in
					redraw_all ns;
					ns
				)
			)
		) else ns
	in
	direction (fun s _ ->
		let ns = ref s in
		for i = 1 to s.inc do
			let g_ = grid_copy !ns.g in
			ns := real_main !ns g_;
		done;
		!ns
	) false ignore s once

(** [backwards s once] sends us [s.inc] generations before state [s], then stops if [once] *)
and backward s once =
	let real_main s g_ = 
		(* we're already going back, we should undo the top of history *)
		List.iter (fun (k, (i, j)) -> 
			grid_set g_ i j k;
			draw i j k s;
			grid_mark_active s.g (divi i s.g.si, divi j s.g.si) true
		) (snd (List.hd s.h));
		{s with g = g_; h = List.tl s.h; t = s.t - fst (List.hd s.h)}
	in
	direction (fun s _ ->
		(* we use abs and not just a for loop to account for weird stuff *)
		let ns = ref s in
		while abs (s.t - !ns.t) < s.inc do
			let g_ = grid_copy !ns.g in
			ns := real_main !ns g_;
		done;
		!ns
	) true (fun s -> auto_synchronize true; error "no visible history"; pause {s with b = false}) s once
	(* and yes that's a derek walcott reference. BFI forever. *)

(** [select (i1, j1) (i2, j2) s] selects the rectangle whose corners are the two tuples,
 and waits for keyboard input to act on them. eventually gives the hand back to pause *)
and select (i1, j1) (i2, j2) s =
	let u, v, g, z, col = s.u, s.v, s.g, s.z, s.col in
	let x = u+min i1 i2
	and y = v+min j1 j2
	and dx = abs(i2-i1)
	and dy = abs(j2-j1)
	in
	(* first autotrim selection if it's not all black *)
	let x_, y_, dx_, dy_ = grid_trim g x y dx dy in
	if (dx_ < dx || dy_ < dy || x_ > x || y_ > y)
		&& not (dx_ = 0 && dy_ = 0 && grid_get g (x_) (y_) = 0)
	then select (x_ - u, y_ - v) (x_ + dx_ - u, y_ + dy_ - v) s
	else begin
		(* put a rectangle around selection*)
		set_color magenta; (* hopefully won't be used in a CA *)
		(* the +1 -2 is to stay well inside cell borders *)
		draw_rect ((x-u)*z+1) ((y-v)*z+1) (max 0 ((dx+1)*z-2)) (max 0 ((dy+1)*z-2));
		let erase_magenta_rect () = 
			for i = x to x+dx do
				draw i y (grid_get g i y) s;
				draw i (y+dy) (grid_get g i (y+dy)) s
			done;
			for j = y to y+dy do
				draw x j (grid_get g x j) s;
				draw (x+dx) j (grid_get g (x+dx) j) s
			done;
			draw_grid s
		in
		(* wait for key and act *)
		let rec act () =
			let k = Char.lowercase_ascii (read_key ()) in
			match k with
			| 'o'->
				(* erase magenta rect *)
				erase_magenta_rect ();
				status "pause";
				pause s
			| 'd' | 'r' ->
				let ng = grid_copy g in
				let c = ref [] in
				(* replace all cells here *)
				for i = x to x+dx do
					for j = y to y+dy do
						c := (grid_get g i j, (i, j))::!c;
						grid_set ng i j (
							if k = 'd'
							then 0
							else Random.int @@ Array.length col
						)
					done
				done;
				grid_activate_cell_rect ng x y dx dy;
				let ns = {s with g = ng; h = (0, !c)::s.h} in
				if k = 'd'
				then set_clipboard (to_rle g x y (dx+1) (dy+1) s.rule);
				redraw_all ns; (* we have to do this cuz we messed up the grid lines *)
				status "pause";
				pause ns
			| 't' | 'h' | 'j' | 'k' | 'l' -> erase_magenta_rect ();
				let ng = grid_copy g in
				let c = ref [] in
				let kx, ky = char_to_delta k in
				let dx_, dy_ = (if k = 't' then dy, dx else dx, dy) in
				let x_, y_ = (x+kx, y+ky) in
				let transform (i, j) =
					if k = 't' then
					(* we rotate by bot right then translate dx to left *)
					(x+dx+(j-y)-dx, y+(x+dx-i))
					else (i+kx, j+ky)
				in
				let prev (i, j) =
					if k = 't' then transform (transform (transform (i, j)))
					else (i-kx, j-ky)
				in
				(* rotate all points (erasing those that won't get overwritten) *)
				for i = x to x+dx do
					for j = y to y+dy do
						let i_, j_ = transform (i, j) in
						grid_set ng i_ j_ (grid_get g i j);
						draw i_ j_ (grid_get g i j) s;
						c := (grid_get g i_ j_, (i_, j_))::!c;
						let _i, _j = prev (i, j) in
						(* erase *)
						if _i < x || _i > x+dx || _j < y || _j > y+dy
						then (
							grid_set ng i j 0;
							draw i j 0 s;
							c := (grid_get g i j, (i, j))::!c;
						)
					done;
				done;
				grid_activate_cell_rect ng x_ y_ dx_ dy_;
				let ns = {s with g = ng; h = (0, !c)::s.h} in
				select (x_-u, y_-v) (x_+dx_-u, y_+dy_-v) ns
			| 'c' | 's' ->
				erase_magenta_rect();
				let rle = to_rle g x y (dx+1) (dy+1) s.rule in
				set_clipboard rle;
				if k = 's' then (match get_file_name () with
				| Some name -> save_to_file rle name
				| None -> ());
				status "pause";
				pause s
			| 'a' -> erase_magenta_rect();
				let a_min, a_max, b_min, b_max =
					Hashtbl.fold (fun (a, b) _ (a_min, a_max, b_min, b_max) -> 
					(min a a_min, max a a_max, min b b_min, max b b_max)
				) g.q (max_int, min_int, max_int, min_int)
				in
				let i = g.si*a_min-u
				and j = g.si*b_min-v
				in
				let dx = (a_max-a_min+1)*g.si
				and dy = (b_max-b_min+1)*g.si
				in
				select (i, j) (i+dx, j+dy) s
			| _ -> act ()
		in act ()
	end

(** [edit s] lets the user manually edit cells, then sends back to pause,
   with a modified grid and history. *)
and edit s =
	status "editing (o to get Out)";
	let ng = grid_copy s.g in
	let ns = ref {s with g = ng} in
	let c = ref [] in
	let released = ref false in
	let rec ed () =
		let st = wait_next_event [Button_down; Key_pressed; Mouse_motion; Button_up] in
		if st.keypressed
		then (
			match st.key with
			| 'o' -> ()
			| 'h' | 'j' | 'k' | 'l' -> ns := scroll (!ns) st.key; ed ()
			| 'u' | 'z' -> ns := zoom (!ns) st.key; ed ()
			| 'p' -> (match !c with
				| [] -> ed ()
				| (k, (x, y))::t ->
					c := t;
					grid_set ng x y k;
					draw x y k !ns;
					ed ()
			)
			| _ -> ed ()
		) else if not st.button
		then (released := true; ed())
		else begin
			(* coordinates of the cell you clicked in g *)
			let mx = fst(mouse_pos ())/ !ns.z + !ns.u
			and my = snd(mouse_pos ())/ !ns.z + !ns.v
			in 
			if !released || (!c = [] || (mx, my) <> snd (List.hd !c)) then (
				released := false;
				let k = grid_get ng mx my in
				grid_set ng mx my ((grid_get ng mx my + 1) mod (Array.length s.col));
				grid_activate_adjacent ng (divi mx ng.si, divi my ng.si);
				draw mx my (grid_get ng mx my) !ns;
				c := (k, (mx, my)) :: !c);
			ed ()
		end
	in
	ed ();
	status "pause";
	if !c = []
	then pause !ns (* don't make an empty change, bad ui *)
	else (
		(* reverse because we want the eldest change of a given cell first,
		   the one that has the value it had originally *)
		pause {!ns with h = (0, List.rev !c)::s.h}
	)

(** [pause s] listens for keyboard input (or selection) and then acts upon it,
   with [s] the next state *)
and pause s =
	let {g; f; n; col; rule; h; u; v; z; gl; sp; fo; t; sh; inc; b} = s in
	show_state s;
	(* scroll by du horizontally and dv vertically *)
	let st = wait_next_event [Button_down; Key_pressed] in
	let m =
		if st.button
		then '\005' (* ASCII "enquiry", hack(ish) for selection *)
		else Char.lowercase_ascii st.key
	in
	match m with
	| ' ' ->
		status ("running ("^(if b then "backward)" else "forward)"));
		(if b then backward else forward) s false
	| 'f' -> status "running (forward)"; forward s false
	| 'n' -> status "pause"; forward s true
	| 'b' -> status "running (backward)"; backward s false
	| 'p' -> status "pause"; backward s true
	| 'e' -> edit s
	| 'q' -> erase_all () (* and purposefully do nothing next *)
	| 'd' -> erase_all (); draw_grid s; pause {s with g = grid_new g.si g.tor; h = []; t = 0}
	| 'r' | 'x' ->
		let sx, sy =
			match g.tor with
			| Some (_, _) -> cells_width s, cells_height s
			| None -> 6*width ()/(10*z), 6*height ()/(10*z)
		in
		(* get a random grid that fills 60% of the screen *)
		let ng = grid_new g.si g.tor in
		if m = 'r' then
			for i = 0 to sx - 1 do
				for j = 0 to sy - 1 do
					grid_set ng i j (Random.int (Array.length col))
				done;
			done
		else
			for i = 0 to sx/2 do
				for j = 0 to sy/2 do
					let co = (Random.int (Array.length col)) in
					grid_set ng i j co;
					grid_set ng (sx-i) j co;
					grid_set ng i (sy-j) co;
					grid_set ng (sx-i) (sy-j) co
				done
			done;
		grid_activate_cell_rect ng 0 0 (sx-1) (sy-1);
		(* and change the offset to put it in the center *)
		let nu, nv =
			if g.tor <> None
			then -100/z, -100/z
			else -2*width ()/(10*z), -2*height ()/(10*z)
		in
		let ns = {s with g = ng; h = []; u = nu; v = nv; t = 0} in
		redraw_all ns;
		pause ns
	| 'i' ->
			info ();
			ignore(wait_next_event [Key_pressed]);
			redraw_all s;
			pause s
	| 'h' | 'j' | 'k' | 'l' -> pause (scroll s m)
	| 'z' | 'u' -> pause (zoom s m)
	| 'g' -> erase_all (); pause {s with gl = not s.gl}
	| '\005' -> status "selecting (o to get Out)";
		(* now modify the grid *)
		let rec get_snd_pos () =
			(* the first one being in st all the way up wa *)
			let stat = wait_next_event [Button_up; Key_pressed] in
			if stat.keypressed then (
				if stat.key = 'o' (* exit on o *)
				then (status "pause"; pause s)
				else get_snd_pos ()
			) else (
				if stat.mouse_x < 0 || stat.mouse_x >= width () || stat.mouse_y < 0 || stat.mouse_y >= height () (* out of screen *)
				then (status "pause"; pause s)
				else select (st.mouse_x/z, st.mouse_y/z) (stat.mouse_x/z, stat.mouse_y/z) s
			)
		in
		get_snd_pos ()
	| 'v' -> (
		try begin
			let rle = get_clipboard () in
			(* this is the system clipboard, which might or might not be RLE.
			   if we fail, from_rle raises an exception which we catch a bit further down *)
			let ng = grid_copy g in
			let px, py = mouse_pos () in
			let x = px/z + u
			and y = py/z + v in
			let c, dx, dy = from_rle rle ng x y in
			let ns = {s with g = ng; h = (0, c)::h} in
			redraw_all ns;
			pause ns
		end with Not_rle -> (error "can't paste, clipboard is not RLE"; pause s)
		)
	| 'o' | 'a' ->
		(try
			match get_file_name () with
			| Some name ->
				let rle = read_from_file name in
				set_clipboard rle;
				let ng = if m = 'o' then grid_new g.si g.tor else grid_copy g in
				let px, py = mouse_pos () in
				let x, y = if m = 'o' then 0, 0 else px/z + u, py/z + v in
				let c, dx, dy = from_rle rle ng x y in
				(* calculated to put the pattern approx in the center *)
				let u_, v_, h_ =
					if m = 'o'
					then (dx*z - width ())/(2*z), (dy*z - height ())/(2*z), []
					else u, v, (0, c)::h in
				let ns = {s with g = ng; h = h_; u = u_; v = v_; t = if m = 'o' then 0 else t} in
				redraw_all ns;
				status "pause"; pause ns
			| None -> status "pause"; pause s
		with
			| Not_rle -> (error "file content is not RLE"; pause s)
			| Sys_error _ -> (error "file does not exist"; pause s)
		)
	| 'w' ->
		erase_all ();
		let tor = match g.tor with
			| None -> Some ((width () - 200) / (z * g.si), (height () - 200) / (z * g.si))
			| Some _ -> None
		in
		let ns = {s with g = grid_new g.si tor; h = []; u = -100/z; v = -100/z} in
		draw_grid ns;
		pause ns
	| 'c' -> let st = input [||] "Max fps (integer, >= 0), 0 means unlimited" in
		(try
			let sp = int_of_string st in
			assert (sp >= 0);
			status "pause";
			pause {s with sp = sp}
		with _ -> error "not a positive integer"; pause s)
	| 'm' -> pause {s with fo = not fo}
	| 's' ->
		if sh
		then (
			set_color black;
			fill_rect 0 0 (width ()) (height ());
			pause {s with sh = not sh; fo = false} (* don't follow if nothing's onscreen *)
		) else (
			let ns = {s with sh = not sh} in
			redraw_all ns;
			pause ns
		)
	| 't' -> let st = input [||] "Number of ticks (integer, > 0)." in
		(try
			let inc = int_of_string st in
			assert (inc > 0);
			status "pause";
			pause {s with inc = inc}
		with _ -> error "not a positive integer"; pause s)
	| _ -> error "unknown command (press i for info)"; pause s

(** default internal simulator state *)
let default_params = {
	g = grid_new 1 None;
	f = (fun _ _ -> 0);
	n = moore 1;
	col = [|black; white|];
	rule = "none";
	h = [];
	u = 0;
	v = 0;
	z = 10;
	gl = true;
	sp = 60;
	fo = false;
	t = 0;
	sh = true;
	inc = 1;
	b = false;
}

(** automata selection menu *)
let rec choose () =
	erase_all();
	info ();
	let m = input (Array.of_seq (Hashtbl.to_seq_keys config)) "choosing CA" in
	if m = "Q" then ()
	else begin
		(match Hashtbl.find_opt config m with
		| Some aut ->
			erase_all(); status "pause";
			draw_grid default_params;
			pause {default_params with f =
				aut.f; n = aut.n;
				col = aut.c; rule = m;
				g = grid_new (radius aut.n) None;
			}
		| _ -> ());
		choose ()
	end

let () =
	(* initialize the graphics screen & the RNG *)
	Random.self_init ();
	open_graph "";
	(* this is a ''very'' cheap way to deal with window resizes
	essentially, we already request all the pixels we're gonna get
	so it can only get smaller and we're fine lol *)
	resize_window 2800 1500;
	set_color black;
	fill_rect 0 0 2800 1500;
	set_window_title "Cellular Automata Terminal";
	set_font "fixed";
	(* welcome screen. am I indulging myself? yes *)
	set_color white;
	set_line_width 3;
	Unix.sleepf 0.3; (* needed for the size to adjust *)
	(* face *)
	let mp = width()/2 in
	let h y = height()*y / 1000 in
	let a = Array.map (fun (x, y) -> (mp+x, h y))
	and b = Array.map (fun (x, y, i, j) -> (mp+x, h y, mp+i, h j)) in
	draw_poly (a[|(0, 200); (450, 500); (0, 800); (-450, 500)|]);
	(* ears *)
	draw_poly (a[|(-240, 640); (-190, 900); (-140, 710)|]);
	draw_poly (a[|(240, 640); (190, 900); (140, 710)|]);
	(* whiskers *)
	draw_segments (b[|(-70, 420, -380, 370); (-70, 410, -370, 270); (-70, 400, -300, 200)|]);
	draw_segments (b[|(70, 420, 380, 370); (70, 410, 370, 270); (70, 400, 300, 200)|]);
	Unix.sleepf 0.5;
	(* eyes *)
	set_color yellow;
	fill_poly (a[|(-120, 530); (-50, 570); (-120, 610); (-190, 570)|]);
	fill_poly (a[|(120, 530); (50, 570); (120, 610); (190, 570)|]);
	(* C A T *)
	draw_poly_line (a[|(-300, 950); (-400, 950); (-400, 850); (-300, 850)|]);
	draw_poly_line (a[|(-50, 850); (-50, 950); (50, 950); (50, 850)|]); moveto (mp-50) (h 900); lineto (mp+50) (h 900);
	moveto (mp+300) (h 950); lineto (mp+400) (h 950); moveto (mp+350) (h 950); lineto (mp+350) (h 850);
	write ["PRESS ANY KEY TO START"] (mp-60) (h 150) yellow;
	ignore(read_key());
	set_line_width 1;
	try
		choose ()
	with Graphic_failure _-> ()
