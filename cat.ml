(** Cellular Automata Terminal: simulator for outer-totalistic 2D automata, including interface and commands. 

	Requires Graphics module, xclip for clipboard interactions, and generally expects to be on linux. *)
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
	" - g (Grid)     : toggle the grid lines";
	" - w (Wrap)     : toggle periodic boundary conditions (ie torus). This will reset the grid.";
	" - y            : switch between a grid-based algorithm and a quadtree-based algorithm.";
	"                 (there were no other keys left.)";
	"                 Scroll and zoom are unavailable in torus mode.";
	" - s (Show)     : toggle display of cells. (No display is faster.)";
	" - c (Cap)      : set max fps. 0 for unlimited. Default is 60.";
	" - t (Ticks)    : set the number of ticks the simulator advances in each frame."
]
let help_second_half = [
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
	"                 You can exit selection at any point with o (Out).";
	" - v (Paste)    : insert RLE from clipboard with the left corner at mouse position";
	" - o (Open)     : load grid from an RLE file";
	" - a (Add)      : insert RLE from file with lower left at mouse position";
	" - i (Info)     : shows this. Go back to pause by pressing any key";
	" - q (Quit)     : Does Exactly What It Says On The Tin. (Goes back to the CA choice screen.)";
	"                 Put Q again while choosing CA to close CAT.";
]

(** a cell's state is identified to a positive integer *)
type state = int

(** a sector is a square matrix of states (here stored as a 1D Iarray.t) *)
type sector = state Iarray.t

(** macrocells for hashlife *)
type quadtree =
	| Leaf of leaf
	| Cell of cell
and cell  = {
	nw: quadtree; ne: quadtree;
	sw: quadtree; se: quadtree;
	h: int;
	r: quadtree option array; (** r.(i) is the state of the center after 2^i ticks, with i <= n-2. Array.lenght r is log(size)-1. *)
}
and leaf = {
	h: int;
	s: sector;
}

module HashedQuadtree = struct
	type t = quadtree
	(* the hash comparisons don't bring any assurance,
		but i want to avoid comparing stuff whenever possible *)
	let equal p q = match p, q with
		| Cell c, Cell d -> c.h = d.h &&
			c.nw == d.nw && c.ne == d.ne &&
			c.sw == d.sw && c.se == d.se
		| Leaf l, Leaf k -> l.h = k.h && l.s = k.s
		| _ -> false
	(* we store hashes to not recalculate them *)
	let hash = function
		| Leaf l -> l.h
		| Cell c -> c.h
end

module Cache = Hashtbl.Make ( HashedQuadtree )

type qcache = quadtree Cache.t

(** for regular algorithm: quadrant coordinates in the plane to (matrix, activeness). *)
type glattice = ((int * int, sector * bool) Hashtbl.t)

type spacedata =
	| Grid of glattice
	| Quad of (qcache * quadtree) (** for hashlife: the cache and the root.
	invariant: the sw-est corner of ne of root is at (0, 0). *)

(** abstraction of what we need to know about a cell configuration *)
type configuration = {
	mutable s: spacedata;
	si: int; (** each sector's width/height in cells. sometimes noted r for radius in comments *)
	tor: (int * int) option; (** [None] if using unbounded grid, [Some (w, h)] if using bounded grid, where [w, h] are the height and width in sectors *)
}

(** child directions in a quadtree *)
type direction = Nw | Ne | Se | Sw

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
	c: color Iarray.t
}

(** record type for the internal state of the simulator, to make calls less annoying *)
type params = {
	c: configuration;
	f: transition_func;
	n: neighbourhood;
	col: color Iarray.t; (** the display colors for each state
		(and so implicitly also the number of states.)
		0 should be black. TODO: floats? maybe generic 'state params *)
	rule: string; (** for rule= in rle. TODO: interpret this at some point? *)
	h: history;
	u: int; (** u, v are the cell offsets of the lower left corner *)
	v: int;
	z: int; (** the onscreen size of a cell in pixels, > 0 *)
	gl: bool; (** whether to draw the grid lines *)
	sp: int; (** speed cap in fps (0 for unlimited) *)
	fo: bool; (** whether to try and follow the living blob *)
	t: int; (** simulation time (since origin) *)
	sh: bool; (** whether to draw cells *)
	inc: int; (** increment, number of steps we advance by *)
	b: bool; (** going backwards? *)
}

(* custom indexing operator to not go mad *)
let (.!()) = Iarray.get

(** {1 Configurations} *)

(** shorcuts for actual euclidean division*)
let (//) = Int.ediv

(** see {!//} *)
let (%%) = Int.erem

(** [cwrap g (a, b)] wraps around the sectors numbers if out of window and [c.tor] is not [None]. *)
let cwrap c (a, b) = match c.tor with
	| None -> a, b
	| Some (da, db) -> a %% da, b %% db

(** {2 Quad specifics} *)

(** gives the (cache, root) of a Quad-based configuration *)
let gq c = match c.s with
	| Grid _ -> raise (Invalid_argument "gq of Grid")
	| Quad t -> t

let is_leaf = function
	| Leaf _ -> true
	| _ -> false

(** get the cell of a Cell *)
let gc = function
	| Leaf _ -> raise (Invalid_argument "gc of Leaf")
	| Cell c -> c

(** get the hash of a quatree *)
let qhash = HashedQuadtree.hash

(** we can't really index trees but ygwim *)
let nw, ne, se, sw =
	(fun q -> (gc q).nw),
	(fun q -> (gc q).ne),
	(fun q -> (gc q).se),
	(fun q -> (gc q).sw)

(** floor of base 2 log of integer >= 1 *)
let log n =
	let r = ref 0 in
	let n = ref n in
	while !n > 1 do
		n := !n lsr 1;
		incr r
	done;
	!r

(** [qmax_size_rect a b a_ b_] is the smallest qsize that wholly contains the [a, b] to [a_, b_] sector rectangle *)
let qmax_size_rect a b a_ b_ =
	2 + log (max
		(max (abs a) (abs a_))
		(max (abs b) (abs b_))
	)

(** in powers of two from the radius, that is *)
let qsize = function
	| Leaf _ -> 0
	| Cell c -> 1 + Array.length c.r

(** same as [qoffset] but having the size not a quad *)
let qoffset_size k si =
	if k = 1
	then si, 0
	else (let d = si lsl (k - 2) in d, d)

(** [qoffset q si] gives the offsets to use when going up/right, down/left in q *)
let qoffset q si =
	qoffset_size (qsize q) si

(** [qcell t nw ne se sw] gets a cell which has as children nw ne se sw.
	If such a cell already exists in the cache, return that. *)
let qcell c nw ne se sw =
	assert ( (* shouldn't be an issue but you never know *)
		qsize nw = qsize ne &&
		qsize ne = qsize se &&
		qsize se = qsize sw
	);
	let q = Cell {
		nw = nw; ne = ne; se = se; sw = sw;
		h = Hashtbl.hash (qhash nw, qhash ne, qhash se, qhash sw);
		r = Array.make (qsize nw) None;
	} in
	if Cache.mem c q
	then Cache.find c q
	else (Cache.add c q q; q)

(** [qleaf c s] gets a leaf with with sector s (if there's already one in the table, get that) *)
let qleaf c s =
	let l = Leaf { s = s; h = Hashtbl.hash s } in
	if Cache.mem c l
	then Cache.find c l
	else (Cache.add c l l; l)

(** [qvoid c si n] gives an all-black quad of size n *)
let rec qvoid c si n =
	if n = 0
	then qleaf c (Iarray.init (si*si) (Fun.const 0))
	else
		let v = qvoid c si (n-1) in
		qcell c v v v v

(** [qis_void c si q] returns whether q only contains zeroes *)
let qis_void c si q =
	q == qvoid c si (qsize q)

(** [qcenter c si q n t] returns a larger quadtree that puts enough
	0 (or bits of q if t) around q for it to be of size at least n. n >= 1 *)
let rec qcenter c si q n t =
	match q with
	| Leaf l ->
		let v = qvoid c si 0 in
		qcenter c si (qcell c v q v v) n t
	| Cell _ ->
		if qsize q >= n
		then q
		else
			let qa, qb, qc, qd =
				(* what we pad with *)
				if t
				then nw q, ne q, se q, sw q
				else (
					let v = qvoid c si (qsize q - 1) in
					v, v, v, v
				)
			in
			qcenter c si (
				qcell c
				(qcell c qc     qd     (nw q) qb)
				(qcell c qc     qd     qa     (ne q))
				(qcell c (se q) qd     qa     qb)
				(qcell c qc     (sw q) qa     qb)
			) n t

(** [qrecenter c si q t] removes all layers of zeroes around q.
	more precisely, it returns the central quad of size n
	where n is the smallest such that the values of q outside of
	[|-r2^{n-1}, r(2^{n-1}-1)|]^2, it's only zeroes.
	note if t is not None, it instead peels down to tor w/h. *)
let rec qrecenter ca si q t =
	if qsize q <= 1
	then q
	else begin
		let a, b, c, d = nw q, ne q, se q, sw q in
		if (match t with
			| None -> List.for_all (qis_void ca si) [
				sw a; nw a; ne a;
				nw b; ne b; se b;
				ne c; se c; sw c;
				se d; sw d; nw d
			]
			| Some (w, _) -> qsize q > log w
		) then qrecenter ca si (qcell ca (se a) (sw b) (nw c) (ne d)) t
		else q
	end

(** [qrandom c si n k] constructs a random size-n quadtree in [|0, k[| *)
let rec qrandom c si n k =
	if n = 0
	then (
		let s = Iarray.init (si*si) (fun _ -> Random.int k) in
		qleaf c s
	) else (
		let nw = qrandom c si (n-1) k in
		let ne = qrandom c si (n-1) k in
		let se = qrandom c si (n-1) k in
		let sw = qrandom c si (n-1) k in
		qcell c nw ne se sw
	)

(** qsymv: symmetry of a quad around its right-hand side.
    
	qsymh: symmetry of a quad around its bottom side *)
let qsymv, qsymh =
	(* a, b whether to invert i, j,
		m function that gets the quadrants in another order,
		cb the one who called us *)
	let rec qsym_aux a b m cb c si q = match q with
		| Leaf l ->
			let s = Iarray.init (si*si) (fun k ->
				let i = k // si
				and j = k %% si in 
				l.s.!((if a then si-1-i else i)*si+(if b then si-1-j else j))
			) in
			qleaf c s
		| _ ->
			let nw, ne, se, sw = m q in
			let nw = cb c si nw
			and ne = cb c si ne
			and se = cb c si se
			and sw = cb c si sw in
			qcell c nw ne se sw
	and qsymv c si q = qsym_aux true false
		(fun q -> ne q, nw q, sw q, se q)
		qsymv c si q
	and qsymh c si q = qsym_aux false true
		(fun q -> sw q, se q, ne q, nw q)
		qsymh c si q
	in qsymv, qsymh

(** [qsym c si n k] constructs a random size-n quadtree in [|0, k[|,
	except it has cross symetry (c4 i think?) *)
let rec qsym c si n k =
	if n = 0 then qrandom c si 0 k
	else (
		let nw = qrandom c si (n-1) k in
		let ne = qsymv c si nw in
		let se = qsymh c si ne in
		let sw = qsymh c si nw in
		qcell c nw ne se sw
	)

(** [qfind co i j l c q] finds position [i, j] in [q],

	on a leaf: applies [l] to the {!sector} and the position in the {!quadrant}.

	on a cell: applies [c] to the cell, the recursive result of [qfind], and the {!direction} taken. *)
let rec qfind co i j l c q =
	match q with
	| Leaf n ->
		if 0 <= i && i < co.si && 0 <= j && j < co.si
		then
			let k = i*co.si+j in
			l n.s k
		(* might happen if someone calls directly qfind on a leaf *)
		else qfind co i j l c (qcenter (fst (gq co)) co.si q 1 false)
		(* qfind never gets called with out-of-bounds *)
	| _ ->
		(* p k is the smallest p that will include coord k. *)
		let p k =
			(* quad goes from -r2^{p-1} to r2^{p-1} - 1 *)
			let k =
				if k >= 0
				then 1+k
				else -k
			in
			let p = ref 0 in
			while co.si lsl (!p-1) < k do incr p done;
			!p
		in
		(* make sure q extends this far *)
		let r = qcenter
			(fst (gq co))
			co.si
			q
			(max (p i) (p j))
			false
		in
		(* d is the abs of the coords of the centers of the quads.
			if children are leaves work differently (because arrays are indexed from 0) *)
		let dl, dr = qoffset r co.si in
		if j >= 0 then (
			if i < 0
			then c r (qfind co (i+dl) (j-dr) l c (nw r)) Nw
			else c r (qfind co (i-dr) (j-dr) l c (ne r)) Ne
		) else (
			if i < 0
			then c r (qfind co (i+dl) (j+dl) l c (sw r)) Sw
			else c r (qfind co (i-dr) (j+dl) l c (se r)) Se
		)

(** {2 Grid specifics} *)

let is_grid c =
	match c.s with
	| Grid _ -> true
	| _ -> false

(** get the hashtable of a Grid-based configuration *)
let gh c = match c.s with
	| Grid h -> h
	| Quad _ -> raise (Invalid_argument "gh of Quad")

(** [gmark_active c t] marks [t] as alive in Grid [c] *)
let gmark_active c t =
	let t = cwrap c t in
	let h = gh c in
	match Hashtbl.find_opt h t with
	| None -> Hashtbl.add h t (Iarray.init (c.si*c.si) (Fun.const 0), true)
	| Some (_, true) -> ()
	| Some (s, false) -> Hashtbl.replace h t (s, true)

(** [gactivate_adcacent c (a, b)] marks all sectors a distance <= 1 of one of (a, b). *)
let gactivate_adjacent c (a, b) =
	for da = -1 to 1 do
		for db = -1 to 1 do
			gmark_active c (a+da, b+db)
		done
	done

(** [gactivate_t c h] does the same for all keys of h *)
let gactivate_t c h =
	let h = gh c in
	Hashtbl.iter (fun (a, b) _ ->
		gactivate_adjacent c (a, b)
	) h

(** [gactivate_cell_rect c x y dx dy] activates the sectors bordering the rectangle (NOT the interior). *)
let gactivate_cell_rect c x y dx dy =
	for a = x // c.si - 1 to (x+dx) // c.si + 1 do
		gmark_active c (a, y // c.si - 1);
		gmark_active c (a, (y+dy) // c.si + 1) 
	done;
	for b = y // c.si - 1 to (y+dy) // c.si + 1 do
		gmark_active c (x // c.si - 1, b);
		gmark_active c ((x+dx) // c.si + 1, b)
	done

(** [gbounds h] gives the a_min, a_max, b_min, b_max if the [(a, b)]s are the keys of h *)
let gbounds h =
	if Hashtbl.length h = 0
	then (0, 0, 0, 0)
	else
		Hashtbl.fold (fun (a, b) (s, t) (a_min, a_max, b_min, b_max) -> 
			(min a a_min, max a a_max, min b b_min, max b b_max)
		) h (max_int, min_int, max_int, min_int)

(** {2 Main configuration utilities} *)

(** [cnew si tor g] creates a new {!configuration} with no sectors, quad size set to [si] and toroidal settings [tor].
 	it creates a Grid if g and a Quad if not g. *)
let cnew si tor g =
	{s =
		if g
		then Grid (Hashtbl.create 1)
		else begin
			let c = Cache.create 1 in
			Quad (c, qvoid c si 0)
		end;
	tor = tor;
	si = si}

(** [ccopy c] returns a {!configuration} with the internal spacedata *)
let ccopy c =
	match c.s with
	| Grid h -> {c with s = Grid (Hashtbl.copy h)}
	| Quad q -> {c with s = Quad q}

(** [csector c a b] gives the sector at index [a, b] in c *)
let csector c a b =
	match c.s with
	| Grid h -> (match Hashtbl.find_opt h (cwrap c (a, b)) with
		| None -> Iarray.init (c.si * c.si) (Fun.const 0)
		| Some (s, _) -> s)
	| Quad (_, q) ->
		qfind c (a*c.si) (b*c.si) (fun s _ -> s) (fun _ s _ -> s) q

(** [cget c i j] gets the {!state} of cell [i, j] in [c] *)
let cget c i j =
	match c.s with
	| Grid h ->
		(match Hashtbl.find_opt h (cwrap c (i // c.si, j // c.si)) with
		| None -> 0
		| Some (s, _) -> s.!((i %% c.si)*c.si+j %% c.si))
	| Quad (_, q) ->
		qfind c i j (.!()) (fun _ k _ -> k) q

(** [grid_set c i j co] sets [i, j]'s state to [co] in [c]. *)
let cset c i j co =
	match c.s with
	| Grid h ->
		let a = i // c.si and b = j // c.si
		and x = i %% c.si and y = j %% c.si in
		(* we need to copy else sectors are shared between generations *)
		let s = Iarray.mapi
			(fun l v -> if l = x*c.si+y then co else v)
			(csector c a b)
		in
		Hashtbl.replace h (cwrap c (a, b)) (s, true)
	| Quad (ca, q) ->
		c.s <- Quad(ca,
			qfind c i j
			(fun a k ->
				let b = Iarray.mapi
					(fun l v -> if l = k then co else v)
					a
				in
				qleaf ca b
			)
			(fun r s -> function
				| Nw -> qcell ca s      (ne r) (se r) (sw r)
				| Ne -> qcell ca (nw r) s      (se r) (sw r)
				| Se -> qcell ca (nw r) (ne r) s      (sw r)
				| Sw -> qcell ca (nw r) (ne r) (se r) s
			)
			q
		)

(** [ctrim c x y dx dy] returns [i, j, di, dj] such that
	the [i, j] to [i+di, j+dj] rectangle is the smallest subrectangle
	containing all live cells in [x, y] to [x+dx, y+dy] in [c].
	if there are no live cells returns [x, y, 0, 0]
	TODO: smth better for quad? *)
let ctrim c x y dx dy =
	let test r r_lim s ds pa mo =
		(* does mo r where mo is incr/decr and r is a ref,
		 as long as the cells pa s, ..., pa s+ds are all black
		 and !r <> r_lim *)
		while (
			let check = ref true in
			for k = s to s+ds do
				let (n, m) = pa k in
				if cget c n m <> 0
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

(** [gtoq c] transforms a Grid into the equivalent Quad.
	Note: everything is left active. *)
let gtoq c =
	assert (is_grid c);
	let h = gh c in
	let a, a_, b, b_ = gbounds h in
	(* very rough estimation. that's ~ the number of leaves without caching,
		but a lot of stuff is cached so it's ok *)
	let ca = Cache.create ((b_-b)*(a_-a)) in
	let n = qmax_size_rect a b a_ b_ in
	(* a b coords of sw-est of ne corner, k wanted qsize *)
	let rec make_cell a b k =
		if k = 0 then (
			qleaf ca (csector c a b)
		) else (
			let dl, dr = qoffset_size k 1 in
			let nw = make_cell (a-dl) (b+dr) (k-1)
			and ne = make_cell (a+dr) (b+dr) (k-1)
			and se = make_cell (a+dr) (b-dl) (k-1)
			and sw = make_cell (a-dl) (b-dl) (k-1)
			in
			qcell ca nw ne se sw
		)
	in
	let r = make_cell 0 0 n in
	{c with s = Quad(ca, r)}

(** [qtog c] transforms a Quad into the equivalent Grid. *)
let qtog c =
	assert (not (is_grid c));
	let ca, r = gq c in
	(* we know pretty precisely how many we have *)
	let h = Hashtbl.create (1 lsl (2 * qsize r)) in
	let c = {c with s = Grid h} in
	(* adds all the leaves of quadtree [q] with the sw-est at [a, b] *)
	let rec add_cells a b q = match q with
		| Leaf l ->
			Hashtbl.replace h (a, b) (l.s, true);
			gactivate_adjacent c (a, b)
		| Cell _ ->
			let d = 1 lsl (qsize q - 1) in
			if not (qis_void ca c.si (nw q)) then add_cells a (b+d) (nw q);
			if not (qis_void ca c.si (ne q)) then add_cells (a+d) (b+d) (ne q);
			if not (qis_void ca c.si (se q)) then add_cells (a+d) b (se q);
			if not (qis_void ca c.si (sw q)) then add_cells a b (sw q)
	in
	let d = 1 lsl (qsize r - 1) in
	add_cells (-d) (-d) r;
	c

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
		fill_rect x y (10*String.length h) 20 (* to overlay the text on top of the CA, more pretty *);
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
		"max fps : " ^ (if s.sp = 0 then "unlimited" else string_of_int s.sp ^ "fps");
		"torus : " ^ (if s.c.tor = None then "no" else "yes");
		"tick : " ^ (string_of_int s.inc);
		"mode : " ^ (if is_grid s.c then "grid" else "quad")]
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
	match s.c.tor with
	| None -> width ()/s.z
	| Some (w, _) -> s.c.si*w
(** height of visible space (in cells). for torus is smaller *)
and cells_height s =
	match s.c.tor with
	| None -> height ()/s.z
	| Some (_, h) -> s.c.si*h

(** position of centre of root tree when in quad + tor (just 0 rest of the time) *)
let root_pos s =
	match s.c.tor, s.c.s with
	| _, Grid _
	| None, _ -> 0
	| Some (w, _), Quad _ -> s.c.si*w/2

(** if [i, j] are coordinates in [s], then [screen_coord (i, j) s] is
	where this should go onto the screen (applying the needed offsets) *)
let screen_coord (i, j) s =
	let d = root_pos s in
	(i-s.u+d, j-s.v+d)

(** vice-versa: if [x, y] are coordinates of a cell on the screen, then
	[conf_coord (x, y) s] returns [i, j] the coordinaets of that place in [s] *)
let conf_coord (x, y) s =
	let d = root_pos s in
	(x+s.u-d, y+s.v-d)

(** [draw i j c s] draws a cell, aka a square. [c] is the color *)
let draw i j c s =
	let x, y = screen_coord (i, j) s in
	if s.sh && (
		s.c.tor <> None
		|| (0 <= y && y < cells_height s && 0 <= x && x < cells_width s)
	) then (
		set_color s.col.!(c);
		if s.z > 1
		then (
			let o = if s.gl && not (small_zoom s) then 1 else 0 in
			fill_rect (x*s.z+o) (y*s.z+o) (s.z-1-o) (s.z-1-o);
		) else if s.z = 1
		then plot x y
	)

(** draws the grid lines, not at very low zoom *)
let draw_grid_lines s =
	(* drawing the grid is actually really expensive *)
	if s.sh && s.gl && not (small_zoom s) then begin
		set_color (rgb 32 32 32);
		for x = 0 to cells_width s do
			let x = if s.c.tor <> None then x + 100/s.z else x in
			if (x+s.u) %% 10 = 0 then set_color (rgb 64 64 64);
			if (x+s.u) %% 10 = 1 then set_color (rgb 32 32 32);
			moveto (x*s.z) 0;
			lineto (x*s.z) (height ());
		done;
		set_color (rgb 32 32 32);
		for y = 0 to cells_height s do
			let y = if s.c.tor <> None then y + 100/s.z else y in
			if (y+s.v) %% 10 = 0 then set_color (rgb 64 64 64);
			if (y+s.v) %% 10 = 1 then set_color (rgb 32 32 32);
			moveto 0 (y*s.z);
			lineto (width ()) (y*s.z)
		done
	end

(** [draw_sector s se u v] draws {!sector} [se] with cell offsets u and v *)
let draw_sector s se u v =
	let si = s.c.si in
	for x = 0 to si - 1 do
		for y = 0 to si - 1 do
			draw (u+x) (v+y) se.!(x*si+y) s
		done;
	done

(** [draw_quad s q u v] draws {!quadtree} q with quad with sw-est of ne at u, v. *)
let rec draw_quad s q u v =
	match q with
	| Leaf l -> draw_sector s l.s u v
	| Cell _ ->
		let dl, dr = qoffset q s.c.si in
		let ca = fst (gq s.c) in
		if not (qis_void ca s.c.si (nw q)) then draw_quad s (nw q) (u-dl) (v+dr);
		if not (qis_void ca s.c.si (ne q)) then draw_quad s (ne q) (u+dr) (v+dr);
		if not (qis_void ca s.c.si (se q)) then draw_quad s (se q) (u+dr) (v-dl);
		if not (qis_void ca s.c.si (sw q)) then draw_quad s (sw q) (u-dl) (v-dl)

(** [redraw_all s] redraws all cells in [s.c] and the grid lines where applicable *)
let redraw_all s =
	auto_synchronize false;
	erase_all ();
	(match s.c.s with
	| Grid h -> Hashtbl.iter (fun (a, b) (q, _) -> draw_sector s q (a*s.c.si) (b*s.c.si)) h
	| Quad (_, r) ->
		draw_quad s r 0 0
	);
	draw_grid_lines s;
	auto_synchronize true

(** associates to a character the displacement (from vim shortcuts) *)
let char_to_delta = function
	| 'h' -> (-1, 0)
	| 'j' -> (0, -1)
	| 'k' -> (0, 1)
	| 'l' -> (1, 0)
	| _ -> (0, 0)

(** [scroll s m] translates [s] with input [m] *)
let scroll s m =
	let (du, dv) = char_to_delta m in
	if s.c.tor <> None then (error "un(zoom) and scroll not available on a torus"; s)
	else (
		let du = du*(max 1 (30/s.z))
		and dv = dv*(max 1 (30/s.z)) in
		let ns = {s with u = s.u+du; v = s.v+dv} in
		redraw_all ns;
		ns
	)

(** [zoom s m] (un)zooms [s] with input [m] *)
let zoom s m =
	let dz = match m with
		| 'u' -> -1
		| 'z' -> 1
		| _ -> 0
	in
	if s.c.tor <> None then (error "un(zoom) and scroll not available on a torus"; s)
	else (
		if s.z <= 1 && m = 'u' then (error "already at minimum zoom"; s)
		else (
			(* calculated to keep in center. make a drawing and you'll see *)
			let du, dv =
				(width ()) * dz / (2*s.z*(s.z+dz)),
				(height ()) * dz / (2*s.z*(s.z+dz))
			in
			let ns = {s with u = s.u+du; v = s.v+dv; z = s.z+dz} in
			redraw_all ns;
			ns
		)
	)

(** {1 Pattern saving/loading} *)

(** [to_rle c x y dx dy r] makes an RLE string for the cells [x, y] to [x+dx-1, y+dy-1] of [c], with [r] the rulestring *)
let to_rle c x y dx dy r =
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
	(* push the last thing on top of the stack *)
	let add_token () =
		if !ti <= 0 then () else
		let o = if !ti = 1
		then !cur
		else (string_of_int (!ti)) ^ (!cur)
		in
		Buffer.add_string b o
	in
	(* adds a $ or one on top of the $ pile *)
	let line_end () =
		if !cur = "$"
		then incr ti
		else (
			if !cur <> "!" && !cur <> string_of_state 0
			then add_token ();
			cur := "$"; ti := 1
		)
	in
	(* read an actual state and add onto pile *)
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
	(* stack up t*c.si m on the pile, except if previous was 0 *)
	let stack t m =
		let t = t*c.si in
		if !cur <> m && !cur <> (string_of_state 0)
		then (
			add_token ();
			cur := m;
			ti := 0
		);
		ti := !ti + t
	in
	begin
	match c.s with
	| Grid h ->
		(* we have two algorithms, which can be more or less effective depending on the grid.
		   Either we iterate over all the cells in the rectangle: O(dx*dy),
		   or we iterate over all the active cells: O(#h) (plus some other stuff but negligible.) *)
		if dx*dy < Hashtbl.length h then (
			(* we function on columns but rle functions on lines (plus our columns start from bottom) *)
			for j = y + dy -1 downto y do
				if Iarray.exists (fun i -> cget c i j <> 0) (Iarray.init dx (fun k -> k+x)) then (
					for i = x to x + dx - 1 do
						(* this is the costy part, because we're requesting a lot of cells which,
						   as it happens, may have no remote possibility of being alive *)
						use_new (cget c i j);
					done;
				);
				line_end ()
			done;
		) else (
			Hashtbl.to_seq h
			(* filter out chunks which surely can't be it, but this may keep a few redundant ones on the edges.
			 this is the costy part of this algorithm: iterating over the whole of g *)
			|> Seq.filter (fun ((a, b), _) ->
				x // c.si <= a && a <= (x+dx) // c.si &&
				y // c.si <= b && b <= (y+dy) // c.si
			)
			|> List.of_seq
			(* sort by decreasing b, then decreasing a. the way group is made will flip a's again to increasing *)
			|> List.sort (fun ((a, b), _) ((c, d), _) -> -2*(compare b d) - compare a c)
			(* to actually read a RLE line in a row (which is fixed j, increasing i),
			   we need to read one line of each sector that has this b,
			   then the next lines for those sectors, etc, so we group by b *)
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
			|> (fun l -> ignore @@ List.fold_left
				(fun last_b gr ->
					let b = snd (fst (List.hd gr)) in
					(* add $s for empty lines *)
					stack (last_b - b - 1) "$";
					for j = c.si - 1 downto 0 do
						ignore @@ List.fold_left (fun last_a ((a, _), (s, _)) ->
							(* add bs for empty cells since the last quad *)
							if last_a < a - 1 then stack (a - 1 - last_a) (string_of_state 0);
							for i = 0 to c.si - 1 do
								use_new s.!(i*c.si+j)
							done;
							a
						) (x // c.si - 1) gr;
						line_end ();
					done;
					b
				)
				(snd @@ fst @@ List.hd @@ List.hd l)
				l
			)
		)
	| Quad (ca, r) ->
		(* for quads we don't have an activity measure, but we can be reasonably sure
		that this will be ~efficient thanks to log? *)
		(* adds the jth line of a quad, where its sw-est of ne is currently at u *)
		let rec add_line j u = function
			| Leaf l ->
				for i = 0 to c.si - 1 do
					use_new l.s.!(i*c.si+j)
				done
			| q ->
				let dl, dr = qoffset q c.si in
				if j >= 0 then (
					if x < u then add_line (j-dr) (u-dr) (nw q);
					if u <= x+dx-1 then add_line (j-dr) (u+dl) (ne q)
				) else (
					if x < u then add_line (j+dl) (u-dr) (sw q);
					if u <= x+dx-1 then add_line (j+dl) (u+dl) (se q)
				)
		in
		let n = qmax_size_rect (x // c.si) (y // c.si) ((x+dx-1) // c.si) ((y+dy-1) // c.si) in
		let r = qcenter ca c.si r n false in
		for j = y+dy-1 downto y do
			add_line j 0 r;
			line_end ()
		done
	end;
	if !cur <> "!" && !cur <> "$"
	then add_token ();
	"x = "^(string_of_int dx)^
	", y = "^(string_of_int dy)^
	", rule = "^r^"\n"^(Buffer.contents b)^"!"

(** trying to parse as RLE something which isn't *)
exception Not_rle

(** [from_rle s c x y] interprets [s] as an RLE string,
	and adds its contents in conf [c] with the left corner at [x, y].
	returns the change list, width and height
 	@raise Not_rle if s can't be made sense of as an RLE string *)
let from_rle s c x y =
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
	let ch = ref [] in
	let toactivate = Hashtbl.create 1 in
	while !k < String.length r do
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
				ch := (cget c i_ j_, (i_, j_)) :: !ch;
				cset c i_ j_ co;
				if is_grid c then Hashtbl.replace toactivate (i_ // c.si, j_ // c.si) true;
				incr i
			done;
			n := 0
		| '!' -> ()
		| _ -> assert false);
		incr k
	done;
	if is_grid c then gactivate_t c toactivate;
	!ch, dx, dy
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

(** {1 Automata parameters} *)

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

(** default settings for an automatan *)
let default_automata = {
	f = bs "3" "2,3";
	n = moore 1;
	c = Iarray.of_array [|black; white|]
}

(** [radius n] gets the biggest offset in [n] *)
let radius (n: neighbourhood) =
	List.fold_left
		(fun m (di, dj) -> max m (max (abs di) (abs dj)))
		0
		(List.concat n)

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
				| 'f' -> {aut with f = parse_rule v (Iarray.length aut.c)}
				| 'n' -> let r = String.sub v 1 (String.length v - 1) in
					{aut with n = match v.[0] with
						| 'M' -> moore (int_of_string r)
						| 'N' -> neumann (int_of_string r)
						| _ ->
						v
						|> String.split_on_char '@'
						|> List.map (String.split_on_char ';')
						|> List.map (List.map tup_of_string)
					}
				| 'c' ->
						{aut with c = if v = "RGB" then Iarray.of_array [|black; red; green; blue|] else
						v
						|> String.split_on_char ';'
						|> List.map int_of_string
						|> Iarray.of_list
					}
				| _ -> raise (Invalid_argument "empty line in .cat_config")
			)
			default_automata
			(List.tl l)
		|> Hashtbl.add this name;
	);
	this

(** Displays information about CAT *)
let info () =
	write help_first_half 50 (height () - 50) white;
	write (help_second_half @
	[""; "Available CA are:"] @
	[
		Hashtbl.to_seq_keys config
		|> List.of_seq
		|> List.sort Stdlib.compare
		|> String.concat ", "
	] @
	["Enter Q while choosing CAs to close CAT."])
	(width () / 2 + 50) (height () - 50) white


(** {1 Simulation} *)

(** [sector_next s change se a b] gets the immediate successor of sector [se] = [a, b] based on its neighbours in [s],
	writing changes in [change], and returns that new {!sector} and whether any changes have been made *)
let sector_next s change se a b =
	let changed = ref false in
	(Iarray.mapi (fun p k ->
		let x = p // s.c.si
		and y = p %% s.c.si in
		let i = a*s.c.si+x
		and j = b*s.c.si+y in
		(* applies the relatives coordinates in argument
		   to the coordinates of the current cell to get the states.*)
		let rec get_values = function
			| [] -> []
			| hd::tl -> cget s.c (i + fst hd) (j + snd hd) :: get_values tl
		in
		let k_ = s.f (List.map get_values s.n) k in (* new cell state *)
		if is_grid s.c && k <> k_ then begin (* do nothing if unchanged.
			also qforward doesn't move with time so doesn't care about this *)
			changed := true;
			draw i j k_ s;
			change := (k, (i, j))::!change
		end;
		k_
	) 
	se, !changed)

(** [qforward_two_to j s change] simulates [s]'s configuration (assumed to be Quad)
	2^[j] generations and returns a {!configuration} as result, adding changes to [change] *)
let qforward_two_to j s =
	let {c; f; n; col; rule; h; u; v; z; gl; sp; fo; t; sh} = s in
	let co = c in
	let ca, r = gq co in
	(* get the center of [q] after 2^[j] steps, assuming j small enough. *)
	let rec ( .?() ) q j =
		assert (qsize q >= 2);
		let c = gc q in
		if c.r.(j) = None
		then c.r.(j) <-
			if qsize q = 2
			then Some (
				(* computing the center of a 4si after 1 step *)
				assert (j = 0);
				let co_ = {co with s = Quad(ca, q)} in
				let ns = {s with c = co_} in
				let get a b =
					let se = csector co_ a b in
					let se_, _ = sector_next ns (ref []) se a b in
					qleaf ca se_
				in
				let rnw = get (-1) 0
				and rne = get 0 0
				and rse = get 0 (-1)
				and rsw = get (-1) (-1)
				in
				qcell ca rnw rne rse rsw
			) else Some (
				let n = qsize q in
				let t = min j (n-3) in
				let a, b, c, d = (nw q), (ne q), (se q), (sw q) in
				(* cover the halfway slab. *)
				let qnw = a.?(t)
				and qnn = (qcell ca (ne a) (nw b) (sw b) (se a)).?(t)
				and qne = b.?(t)
				and qee = (qcell ca (sw b) (se b) (ne c) (nw c)).?(t)
				and qse = c.?(t)
				and qss = (qcell ca (ne d) (nw c) (sw c) (se d)).?(t)
				and qsw = d.?(t)
				and qww = (qcell ca (sw a) (se a) (ne d) (nw d)).?(t)
				and qcc = (qcell ca (se a) (sw b) (nw c) (ne d)).?(t)
				in
				if j = n-2
				then
					(* go a bit further *)
					let rnw = (qcell ca qnw qnn qcc qww).?(j-1)
					and rne = (qcell ca qnn qne qee qcc).?(j-1)
					and rse = (qcell ca qcc qee qse qss).?(j-1)
					and rsw = (qcell ca qww qcc qss qsw).?(j-1)
					in
					qcell ca rnw rne rse rsw
				else
					(* recompose as-is. yes, a bit ugly *)
					let rnw = qcell ca (se qnw) (sw qnn) (nw qcc) (ne qww)
					and rne = qcell ca (se qnn) (sw qne) (nw qee) (ne qcc)
					and rse = qcell ca (se qcc) (sw qee) (nw qse) (ne qss)
					and rsw = qcell ca (se qww) (sw qcc) (nw qss) (ne qsw)
					in
					qcell ca rnw rne rse rsw
			);
		Option.get c.r.(j)
	in
	(* calculate the size of input so that result exists and contains all we want *)
	let p = (max
		(j + 2) (* must have its result after or equal 2^j (also ensures >= 2 so has result) *)
		(qsize r + 2)) (* result must have r2^{p-1} >= r2^j + r2^n, so p >= n+2 (because p-1 >= j+1 already true) *)
	in
	let q = qcenter ca co.si r p (co.tor <> None) in
	let q_ = q.?(j) in
	let q_ = qrecenter ca co.si q_ co.tor in
	{c with s = Quad(ca, q_)}

(** [gforward_one s change] simulates one generation from [s]'s config (assumed to be Grid),
	and returns a configuration as result, adding changes to [chage] *)
let gforward_one s change =
	let {c; f; n; col; rule; h; u; v; z; gl; sp; fo; t; sh} = s in
	let h = gh c in
	let c_ = ccopy c in
	let h_ = gh c_ in
	let toactivate = Hashtbl.create 1 in
	Hashtbl.iter (fun (a, b) (se, t) ->
		if t
		then
			let se_, changed = sector_next s change se a b in
			(* deal with sector activity *)
			if changed
			then (
				Hashtbl.replace h_ (a, b) (se_, true);
				Hashtbl.replace toactivate (a, b) true
			) else (
				(* this quad is inactive *)
				if Iarray.exists ((<>) black) se_
				then Hashtbl.replace h_ (a, b) (se_, false)
				else Hashtbl.remove h_ (a, b)
			)
	) h;
	gactivate_t c_ toactivate;
	c_

(** factorises common logic to forward and backward *)
let rec direction main b err s once =
	let start = Unix.gettimeofday() in
	if s.sh then auto_synchronize false;
	try begin
		let ns = main s in
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
	direction (fun s ->
		let ns = ref s in
		let change = ref [] in
		(match s.c.s with
		| Grid _ ->
			for i = 1 to s.inc do
				ns := {!ns with c = gforward_one !ns change}
			done
		| Quad (ca, r) ->
			for j = 0 to log s.inc do
				if s.inc land (1 lsl j) = (1 lsl j)
				then ns := {!ns with c = qforward_two_to j !ns}
			done;
			let rec diff u v = function
				| Cell c, Cell d ->
					(* as usual center offsets *)
					let dl, dr = qoffset (Cell c) s.c.si in
					if c.nw != d.nw then diff (u-dl) (v+dr) (c.nw, d.nw);
					if c.ne != d.ne then diff (u+dr) (v+dr) (c.ne, d.ne);
					if c.se != d.se then diff (u+dr) (v-dl) (c.se, d.se);
					if c.sw != d.sw then diff (u-dl) (v-dl) (c.sw, d.sw)
				| Leaf l, Leaf m ->
					for k = 0 to s.c.si*s.c.si - 1 do
						if l.s.!(k) <> m.s.!(k)
						then (
							let x = k // s.c.si
							and y = k %% s.c.si
							in
							change := (l.s.!(k), (u+x, v+y))::!change;
							draw (u+x) (v+y) m.s.!(k) !ns
						)
					done
				| _ -> raise (Invalid_argument ("diff of quadtrees of different sizes"))
			in
			let q = snd (gq !ns.c) in
			let n = max (qsize r) (qsize q) in
			(* if tor then q and r have precisely the same size *)
			diff 0 0 (qcenter ca s.c.si r n false, qcenter ca s.c.si q n false)
		);
		let ns = {!ns with t = s.t + s.inc; h = (s.inc, !change)::s.h} in
		if ns.fo && ns.c.tor = None
		then (
			let w, h = cells_width s, cells_height s in
			let x, y, dx, dy = ctrim ns.c ns.u ns.v w h in
			if x = ns.u || y = ns.v || x + dx = ns.u + w || y + dy = ns.v + h (* touching the edge *)
			then {ns with fo = false}
			else (
				let du = x - (w - dx)/2 - ns.u
				and dv = y - (h - dy)/2 - ns.v in
				if du = 0 && dv = 0
				then ns
				else (
					let ns = {ns with u = ns.u+du; v = ns.v+dv} in
					redraw_all ns;
					ns
				)
			)
		) else ns
	) false ignore s once

(** [backwards s once] sends us at least [s.inc] generations before state [s], then stops if [once].
	(may be more than [s.inc] after modifying [s.inc]) *)
and backward s once =
	direction (fun s ->
		(* we use abs and not just a for loop to account for weird stuff *)
		let h = ref s.h in
		let t = ref s.t in
		let c = ref s.c in
		let stop = ref false in
		while abs (s.t - !t) < s.inc && not !stop do
			let c_ = ccopy !c in
			(* we're already going back, we should undo the top of history *)
			List.iter (fun (k, (i, j)) -> 
				cset c_ i j k;
				draw i j k s;
				if is_grid s.c then gactivate_adjacent c_ (i %% s.c.si, j %% s.c.si);
			) (snd (List.hd !h));
			t := !t - (fst (List.hd !h));
			h := List.tl !h;
			c := c_;
			if once then stop := true
		done;
		{s with c = !c; h = !h; t = !t}
	) true
	(fun s ->
		auto_synchronize true;
		error "no visible history";
		(* and yes that's a derek walcott reference. BFI forever. *)
		pause {s with b = false; sh = true; fo = false}
	)
	s
	once

(** [select (i1, j1) (i2, j2) s] selects the rectangle whose corners are the two tuples,
	and waits for keyboard input to act on them. eventually gives the hand back to pause *)
and select (i1, j1) (i2, j2) s =
	let u, v, c, z, col = s.u, s.v, s.c, s.z, s.col in
	let x, y = conf_coord (min i1 i2, min j1 j2) s
	and dx = abs (i2-i1)
	and dy = abs (j2-j1)
	in
	(* first autotrim selection if it's not all black *)
	let x_, y_, dx_, dy_ = ctrim c x y dx dy in
	if (dx_ < dx || dy_ < dy || x_ > x || y_ > y)
		&& not (dx_ = 0 && dy_ = 0 && cget c x_ y_ = 0)
	then select (screen_coord (x_, y_) s) (screen_coord (x_+dx_, y_+dy_) s) s
	else begin
		(* put a rectangle around selection*)
		set_color magenta; (* hopefully won't be used in a CA *)
		(* the +1 -2 is to stay well inside cell borders *)
		let i, j = screen_coord (x, y) s in
		draw_rect (i*z+1) (j*z+1) (max 0 ((dx+1)*z-2)) (max 0 ((dy+1)*z-2));
		let erase_magenta_rect () = 
			for i = x to x+dx do
				draw i y (cget c i y) s;
				draw i (y+dy) (cget c i (y+dy)) s
			done;
			for j = y to y+dy do
				draw x j (cget c x j) s;
				draw (x+dx) j (cget c (x+dx) j) s
			done;
			draw_grid_lines s
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
				let nc = ccopy c in
				let ch = ref [] in
				(* replace all cells here *)
				for i = x to x+dx do
					for j = y to y+dy do
						ch := (cget c i j, (i, j))::!ch;
						cset nc i j (
							if k = 'd'
							then 0
							else Random.int @@ Iarray.length col
						)
					done
				done;
				if is_grid nc then gactivate_cell_rect nc x y dx dy;
				let ns = {s with c = nc; h = (0, !ch)::s.h} in
				if k = 'd'
				then set_clipboard (to_rle c x y (dx+1) (dy+1) s.rule)
				else (
					match nc.s, nc.tor with
					| Quad (ca, q), Some (w, _) ->
						nc.s <- Quad (ca, qcenter ca nc.si q (log w) false)
					| _ -> ()
				);
				redraw_all ns; (* we have to do this cuz we messed up the grid lines *)
				status "pause";
				pause ns
			| 't' | 'h' | 'j' | 'k' | 'l' -> erase_magenta_rect ();
				let nc = ccopy c in
				let ch = ref [] in
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
						let k = cget c i j in
						cset nc i_ j_ k;
						draw i_ j_ k s;
						ch := (cget c i_ j_, (i_, j_))::!ch;
						let _i, _j = prev (i, j) in
						(* erase *)
						if _i < x || _i > x+dx || _j < y || _j > y+dy
						then (
							cset nc i j 0;
							draw i j 0 s;
							ch := (k, (i, j))::!ch;
						)
					done;
				done;
				if is_grid nc then gactivate_cell_rect nc x_ y_ dx_ dy_;
				let ns = {s with c = nc; h = (0, !ch)::s.h} in
				select (screen_coord (x_, y_) s) (screen_coord (x_+dx_, y_+dy_) s) ns
			| 'c' | 's' ->
				erase_magenta_rect();
				let rle = to_rle c x y (dx+1) (dy+1) s.rule in
				set_clipboard rle;
				if k = 's' then (match get_file_name () with
				| Some name -> save_to_file rle name
				| None -> ());
				status "pause";
				pause s
			| 'a' -> erase_magenta_rect();
				(match c.s with
				| Grid h ->
					let a_min, a_max, b_min, b_max = gbounds h in
					let i, j = screen_coord (c.si*a_min, c.si*b_min) s
					in
					let dx = (a_max-a_min+1)*c.si
					and dy = (b_max-b_min+1)*c.si
					in
					select (i, j) (i+dx, j+dy) s
				| Quad (_, r) ->
					let d = c.si lsl (qsize r - 1) in
					select (-d, -d) (2*d ,2*d) s)
			| _ -> act ()
		in act ()
	end

(** [edit s] lets the user manually edit cells, then sends back to pause,
	with a modified grid and history. *)
and edit s =
	status "editing (o to get Out)";
	let ns = ref {s with c = ccopy s.c} in
	let ch = ref [] in
	let toactivate = Hashtbl.create 1 in
	let released = ref false in
	let rec ed () =
		let st = wait_next_event [Button_down; Key_pressed; Mouse_motion; Button_up] in
		if st.keypressed
		then (
			match st.key with
			| 'o' -> ()
			| 'h' | 'j' | 'k' | 'l' -> ns := scroll (!ns) st.key; ed ()
			| 'u' | 'z' -> ns := zoom (!ns) st.key; ed ()
			| 'p' -> (match !ch with
				| [] -> ed ()
				| (k, (x, y))::t ->
					ch := t;
					cset !ns.c x y k;
					draw x y k !ns;
					ed ()
			)
			| _ -> ed ()
		) else if not st.button
		then (released := true; ed())
		else begin
			(* coordinates of the cell you clicked in g *)
			let mx, my = mouse_pos () in
			let mx, my = conf_coord (mx / !ns.z, my / !ns.z) !ns in
			let d = root_pos s in
			if -d <= mx && mx < -d + cells_width !ns
			&& -d <= my && my < -d + cells_height !ns
			&& (!released
				|| !ch = []
				|| (mx, my) <> snd (List.hd !ch)
			) then (
				released := false;
				let k = cget !ns.c mx my in
				let k_ = (k + 1) mod (Iarray.length s.col) in
				cset !ns.c mx my k_;
				if is_grid !ns.c then Hashtbl.replace toactivate (mx // !ns.c.si, my // !ns.c.si) true;
				draw mx my (cget !ns.c mx my) !ns;
				ch := (k, (mx, my)) :: !ch
			);
			ed ()
		end
	in
	ed ();
	status "pause";
	if is_grid !ns.c then gactivate_t !ns.c toactivate;
	if !ch = []
	then pause !ns (* don't make an empty change, bad ui *)
	else pause {!ns with h = (0, !ch)::s.h}

(** [pause s] listens for keyboard input (or selection) and then acts upon it,
   with [s] the next state *)
and pause s =
	let {c; f; n; col; rule; h; u; v; z; gl; sp; fo; t; sh; inc; b} = s in
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
	| 'd' -> erase_all (); draw_grid_lines s; pause {s with c = cnew c.si c.tor (is_grid c); h = []; t = 0}
	| 'r' | 'x' ->
		let sx, sy =
			match c.tor with
			| Some (_, _) -> cells_width s, cells_height s
			| None ->
				(if is_grid c
				then 6*width ()/(10*z), 6*height ()/(10*z)
				else width () / z, height () / z)
		in
		(* get a random grid that fills 60% of the screen *)
		let nc = (
			match m, c.s with
			| 'r', Grid _ ->
				(let nc = cnew c.si c.tor true in
				for i = 0 to sx - 1 do
					for j = 0 to sy - 1 do
						cset nc i j (Random.int (Iarray.length col))
					done;
				done;
				nc)
			| 'r', Quad (ca, _)
			| 'x', Quad (ca, _) ->
				let n = log (sy/c.si) in
				let q = (if m = 'r' then qrandom else qsym) ca c.si n (Iarray.length col) in
				{c with s = Quad(ca, q)}
			| 'x', Grid _ ->
				(let nc = cnew c.si c.tor true in
				for i = 0 to sx/2 do
					for j = 0 to sy/2 do
						let co = (Random.int (Iarray.length col)) in
						cset nc i j co;
						cset nc (sx-i) j co;
						cset nc i (sy-j) co;
						cset nc (sx-i) (sy-j) co
					done
				done;
				nc)
			| _ -> assert false
		) in
		if is_grid nc then gactivate_cell_rect nc (-1) (-1) (sx-1) (sy-1);
		(* and change the offset to put it in the center *)
		let nu, nv =
			if nc.tor <> None
			then -100/z, -100/z
			else if not (is_grid c)
			then - cells_width s / 2, - cells_height s / 2
			else -2*width ()/(10*z), -2*height ()/(10*z)
		in
		let ns = {s with c = nc; h = []; u = nu; v = nv; t = 0} in
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
				else (
					select (st.mouse_x/z, st.mouse_y/z) (stat.mouse_x/z, stat.mouse_y/z) s
				)
			)
		in
		get_snd_pos ()
	| 'v' -> (
		try begin
			let rle = get_clipboard () in
			(* this is the system clipboard, which might or might not be RLE.
			   if we fail, from_rle raises an exception which we catch a bit further down *)
			let nc = ccopy c in
			let px, py = mouse_pos () in
			let x, y = conf_coord (px/z, py/z) s in
			let ch, dx, dy = from_rle rle nc x y in
			let ns = {s with c = nc; h = (0, ch)::h} in
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
				let nc =
					if m = 'o'
					then cnew c.si None (is_grid c)
					else ccopy c
				in
				let px, py = mouse_pos () in
				let x, y =
					if m = 'o'
					then 0, 0
					else conf_coord (px/z, py/z) s in
				let ch, dx, dy = from_rle rle nc x y in
				(* calculated to put the pattern approx in the center *)
				let u_, v_, h_ =
					if m = 'o'
					then (dx*z - width ())/(2*z), (dy*z - height ())/(2*z), []
					else u, v, (0, ch)::h in
				let ns = {s with c = nc; h = h_; u = u_; v = v_; t = if m = 'o' then 0 else t} in
				redraw_all ns;
				status "pause"; pause ns
			| None -> status "pause"; pause s
		with
			| Not_rle -> (error "file content is not RLE"; pause s)
			| Sys_error _ -> (error "file does not exist"; pause s)
		)
	| 'w' ->
		erase_all ();
		let tor = match c.tor, is_grid c with
			| None, true -> Some ((width () - 200) / (z * c.si), (height () - 200) / (z * c.si))
			| None, false ->
				let d = 1 lsl (log ((height () - 100) / (z*c.si))) in
				Some (d, d)
			| Some _, _ -> None
		in
		let ns = {s with c = cnew c.si tor (is_grid c); h = []; u = -100/z; v = -100/z} in
		draw_grid_lines ns;
		pause ns
	| 'c' -> let st = input [||] "Max fps (integer, >= 0), 0 means unlimited" in
		let x =
			(try
				let sp = int_of_string st in
				assert (sp >= 0);
				status "pause";
				sp
			with _ ->
				error "not a positive integer";
				s.sp)
		in
		pause {s with sp = x}
	| 'm' -> pause {s with fo = not fo}
	| 's' ->
		if sh then (
			set_color black;
			fill_rect 0 0 (width ()) (height ());
			pause {s with sh = not sh; fo = false} (* don't follow if nothing's onscreen *)
		) else (
			let ns = {s with sh = not sh} in
			redraw_all ns;
			pause ns
		)
	| 't' -> let st = input [||] "Number of ticks (integer, > 0)." in
		let x =
			(try
				let inc = int_of_string st in
				assert (inc > 0);
				status "pause";
				inc
			with _ ->
				error "not a positive integer";
				s.inc)
		in
		pause {s with inc = x}
	| 'y' ->
		status "pause";
		if is_grid c
		then (
			let ns =
				if c.tor <> None
				(* can't really shift tor across stuff *)
				then
					let d = 1 lsl (log ((height () - 100) / (z*c.si))) in
					{s with c = cnew 1 (Some(d, d)) false}
				else {s with c = gtoq c} in
			redraw_all ns;
			pause ns
		) else (status "pause"; pause {s with c = qtog c})
	| _ -> error "unknown command (press i for info)"; pause s

(** default internal simulator state *)
let default_params = {
	c = cnew 1 None true;
	f = bs "3" "2,3";
	n = moore 1;
	col = Iarray.of_array [|black; white|];
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
			draw_grid_lines default_params;
			pause {default_params with f =
				aut.f; n = aut.n;
				col = aut.c; rule = m;
				c = cnew (radius aut.n) None true;
			}
		| _ -> ());
		choose ()
	end

let () =
	(* initialize the graphics screen & the RNG *)
	Random.self_init ();
	open_graph "";
	(* this is a cheap way to deal with window resizes
	essentially, we already request all the pixels we're gonna get
	so it can only get smaller and we're fine lol *)
	resize_window 2800 1500;
	set_color black;
	fill_rect 0 0 2800 1500;
	set_window_title "Cellular Automata Terminal";
	set_font "AdwaitaMono"; (* works on gnome *)
	set_font "Andale Mono"; (* works on mac *)
	set_color white;
	Unix.sleepf 0.3; (* needed for the size to adjust *)
	set_line_width 1;
	try
		choose ()
	with Graphic_failure _-> ()
