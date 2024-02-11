(** Graphical representations of sorting algorithms. *)
open Graphics
(** List of arguments given on command line *)
let arg = Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv-1)) 
(** Help message *)
let info = "Graphical representation of sorting algorithms.\nEach value is a line.\nHigher vales are higher lines and are in bluer color.\nTo be called with the names of the sorts you want, e.g. i or insertion.\nSorts: selection, insertion, bubble, merge, quick, and radix.\nPress any key to do next sort.\nOptions are:\n -l [int] : define the size of the array to be sorted (default is 1024).\n -s [string] : set the speed of the graphical representation. n(one) means 0 delay, r(eal) means same delay for all sorts, to compare them, and e(qual) means approximately same running time, for demonstrating the algorithms. default is equal.\n -c : shows comparison counts.\n -p : pure performance tests, no display, results in console.\n -b [int] : change the base for radix sort. -w : repeats the sorts given until you press enter.\n -help : shows this message."
(** List of names of available sorts *)
let sorts = ["s"; "i"; "b"; "m"; "q"; "r"]
(** Search the value after the keyword (string) in a list (to be called with {!arg}) *)
let rec search l k = 
	match l with
	| w::x::_ when w = k -> x
	| _::tl -> search tl k
	| [] -> "-1"
(** Whether to display or just do pure performance tests *)
let pure = List.mem "-p" arg 
(** Size of array to be sorted *)
let lim = (let x = int_of_string (search arg "-l") in if x = -1 then (if pure then 1024 else 1024) else x) 
(** The delay type. n for none, r for real or e for equal*)
let speed = let x = search arg "-s" in 
	match x with
	| "real" | "r" -> "r"
	| "none" | "n" -> "n"
	| _ -> "e"
(** Whether to show comparison counts *)
let show = List.mem "-d" arg
(** Loops 10**argument times. argument is ignored when {!speed} is equal or none *)
let pause t = let rec dec x = if x = 0 then () else dec (x-1) in dec (int_of_float (10.**float_of_int (if speed = "r" then 4 else if speed = "n" || pure then 0 else t)))
(** Sets or shows the title of the sort *)
let title s = if pure then print_string s else set_window_title s 
(** The comparison count. to be changed, for now too lazy to implement this correctly (plus would need an accumulator that might slow -p down) *)
let comp = ref 0 
(** Infix lower than that increases {!comp} *)
let (^<) a b = if pure then a < b else (comp := !comp + 1; a < b) 
(** Base for {!rsort} *)
let ba = let x = int_of_string (search arg "-b") in if x = -1 then 8 else x 
let () = if not pure then (open_graph ""; resize_window lim lim; set_color black; fill_rect 0 0 lim lim); Random.self_init() (* initializing graphics screen *)
(** Gives color depending on value, bluer for bigger and greener for smaller *)
let col h = let c = h*255/lim in rgb 0 (255-c) c 
(** Draws bar of at index first argument and height second argument *)
let draw i h = if speed <> "n" then (moveto i 0; set_color (col h); lineto i h; set_color black; lineto i lim) 
(** {!draw} a list with the first value at given index *)
let drawl j l = 
	let rec it l i = 
		match l with
		| [] -> () 
		| hd::tl -> draw i hd; it tl (i+1) in it l j
(** The array to be sorted *)
let t = Array.make lim 0
(** Fills {!t} with random values *)
let rec b i = if i = lim then () else let r = Random.int lim in (if not pure then draw i r; t.(i) <- r; b (i+1))
(** {!draw}s the value at index given in {!t} at that index *)
let dra i = draw i t.(i)
(** Swaps the values at indexes given in {!t} and {!draw}s them *)
let swap i j = let tmp = t.(i) in t.(i) <- t.(j); t.(j) <- tmp; if not pure then (dra i; dra j) 
(** Redisplays t, for when {!speed} is n, there is no display so it has to be displayed at the end *)
let display() = let rec it i = if i = lim then () else (moveto i 0; set_color (col t.(i)); lineto i t.(i); set_color black; lineto i lim; it (i+1)) in it 0

(** Performs in place selection sort on {!t} *)
let ssort() =
	let rec find_min i = if i = lim-1 then i else let ni = find_min (i+1) in if (pause 4;  t.(i) ^< t.(ni)) then i else ni in
	let rec sort i =
		if i = lim then () else
		let min_i = find_min i in
		(swap i min_i;
		sort (i+1)) in
	sort 0

(** Performs in place insetion sort on {!t} *)
let isort() =
	let rec sort i =
		if i = lim then () else
		let rec it j =
			if (pause 4; 0 ^< j && t.(j) ^< t.(j-1)) then (swap j (j-1); it (j-1)) in
		it i; sort (i+1) in
	sort 0

(** Performs in place bubble sort on {!t} *)
let bsort() =
	let rec sort i j =
		if j = 0 then () else
		if i = j then (sort 0 (j-1)) else
		(if t.(i+1) ^< t.(i) then swap i (i+1); sort (i+1) j)
	in sort 0 (lim-1)

(** Performs in place quick sort on {!t} *)
let qsort () =
	let rec sort i j =
		if i >= j  then () else
		let b = t.(j) in (* the pivot *)
		let rec split l r =
			if l >= r 
			then (swap (r+1) j; if (pause 6; t.(r+1) ^< t.(l)) then (swap l (r+1); l) else r+1) (* reorganizing the middle value, the middle +1 value and the pivot so that they are in the right place. middle + 1goes to j to leave room to maneuver with middle value and pivot. *) 
			else
			let l_ = if (pause 6; t.(l) ^< b) then l+1 else l in
			let r_ = if (pause 6; b ^< t.(r)) then r-1 else r in
			if l_ = l && r_ = r then (* if left value should be on the right and right one should be on the left *)
				(swap l r;
				split (l+1) (r-1))
			else split l_ r_ in
		let m = split i (j-1) in
		sort i (m-1); 
		sort (m+1) j
	in sort 0 (lim-1)

(** Performs radix sort on {!t}, with {!ba} buckets *)
let rsort() =
	let make = let rec it i = if i = ba then [] else []::it (i+1) in it 0 in
	let rec put l i =
		match l with
		| [] -> ()
		| hd::tl -> draw i hd; pause 6; put tl (i+1) in
	let rec sort th l bu = (* bu is the list of the buckets *)
		(if not pure && List.for_all (fun x -> x = []) bu then put l 0);
		if th >= lim then () else
		match l with
		| [] -> sort (th*ba) (List.concat (List.map List.rev bu)) make
		| hd::tl -> (let n = hd/th mod ba in (* this thing gives you the log_ba di th radix of hd *)
			sort th tl (List.mapi (fun j v -> if j = n then hd::v else v) bu)) in
	sort 1 (Array.to_list t) make

(** Performs merge sort on {!t} (not in place)*)
let msort() =
	let rec sort l i =
		if List.length l = 1
		then (if not pure then draw i (List.hd l); l)
		else
			let mid = List.length l/2 in
			let rec split l i g =
				if i = mid then (g,l) else split (List.tl l) (i+1) (List.hd l::g) in
			let (m, n) = split l 0 [] in
			let rec fuse a b j = 
				if a = [] then (if not pure then drawl j b; b) else if b = [] then (if not pure then drawl j a; a) else
				let va = List.hd a and vb = List.hd b in
				if (pause 6; va ^< vb)
				then (if not pure then draw j va(*; drawa (j+1) x; drawa (j+Array.length a) b*); va::(fuse (List.tl a) b (j+1)))
				else (if not pure then draw j vb(*; drawa (j+1) a; drawa (j+Array.length a+1) x*); vb::(fuse a (List.tl b) (j+1))) (* the commented parts of code used (with now obsolete functions) actually show the process of merging instead of jsut writing the merged list on top of the other two, but was too slow *)
			in fuse (sort m i) (sort n (i+mid)) i
	in let l = sort (Array.to_list t) 0 in 
		let rec cop l i = (* to copy the values of the list into t for -s n *)
			match l with
			| [] -> ()
			| hd::tl -> (t.(i) <- hd; cop tl (i+1)) in if not pure then cop l 0

(** Runs the sort identified by the string in argument *)
let run s =
	b 0;
	comp := 0;
	let start_time = Sys.time() in
	(match s with
	| "s" | "selection" | "select" -> title "Selection sort"; ssort()
	| "i" | "insertion" | "insert" -> title "Insertion sort"; isort()
	| "b" | "bubble" -> title "Bubble sort"; bsort()
	| "m" | "merge" -> title "Merge sort"; msort()
	| "q" | "quick" -> title "Quick sort"; qsort()
	| "r" | "radix" -> title "Radix sort"; rsort()
	| _ -> ());
	if pure then (print_endline (" took "^string_of_float (Sys.time()-.start_time)^" seconds.")) else
	(if speed = "n" then display();
	if show then (
	set_color white; 
	moveto 10 (lim-20);
	draw_string (string_of_int !comp^" comparisons"));
	ignore(read_key()))
(** Runs every sort in {!sorts} *)
let runall () = List.iter (fun s -> run s) sorts
(** Executes the things on the command line *)
let di () = 
	if List.mem "-help" arg 
	then print_endline info 
	else if List.mem "-a" arg 
	then runall() 
	else List.iter run (List.filter (fun v -> List.mem v sorts) arg);
	if pure then (b 0; let start_time = Sys.time() in ignore (Array.sort Stdlib.compare t); print_string ("For comparison, Array.sort took "^string_of_float (Sys.time() -. start_time)^" seconds.\n(Although it does not have the constraints my sorts have due to display checks.)"))
(** Loops, exit should be done with fatal I/O error *)
let rec wa() = di(); if List.mem "-w" arg then wa()
let () = 
	try wa() with 
	| Graphic_failure "fatal I/O error" -> () (* normal closing, just to hide error message *)
	| Failure s when s = "int_of_string" -> print_endline "Error : non-integer arguments for -l or -s."; print_endline info
	| Invalid_argument _ -> print_endline "Error : negative values somewhere."
	| Match_failure _ -> print_endline "Error : match failure."
	| Stack_overflow -> print_endline "Error : stack overflow. probably too large -l."
	| Division_by_zero -> print_endline "Error : division by 0. radix sort needs a strictly positive number of buckets."
	| Graphic_failure s -> print_endline ("Error : something unforeseen relating to graphics. more precisely : "^s)
	| Failure s -> print_endline ("Error : unknown failure. more precisely : "^s)
