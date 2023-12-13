#load "graphics.cma"
#laod "unix.cma"
open Graphics
let n = [(-1, 0);(1, 0);(0, 1);(0, -1);(-1, -1); (-1, 1); (1, -1); (1, 1)]
let div = print_endline "div"; let m = read_line() in if m = "" then 6*List.length n else int_of_string m
let rand = print_endline "rand";let m = read_line() in if m = "" then 50 else int_of_string m
let var () = Random.int (2*rand+1) - rand
let () = Random.self_init(); open_graph ""; resize_window 999 999; set_color black; fill_rect 0 0 998 998
let rec b i r =
	let rec l y =
		if y = 100
		then [||]
		else Array.append [|if r = 'l' then [|Random.int 1999 - 1000; Random.int 1999 - 1000; Random.int 1999 - 1000|] else if r = 'e' then [|0; 0; 0|] else [|Random.int 999|]|] (l (y + 1)) in
	if i =  100
	then [||]
	else Array.append [|l 0|] (b (i + 1) r)
let wrap k = if k < 0 then 100+k else if k >= 100 then k-100 else k
let rec dcopy a i = if i = Array.length a then [||] else Array.append [|Array.copy a.(i)|] (dcopy a (i+1))
let rec lsum l =
 	match l with
	| [] -> 0
	| i::s -> i+lsum s
let rec di c m p =
	let c_ = dcopy c 0 in
	let draw x y = 
		set_color(
		let col h = let cy = int_of_float(255.*.(float_of_int(h))/. 1000.) in min (max 0 cy) 255 in
		let cell = c_.(x).(y) in 
		let rc = col (cell.(0)) and gc = col (cell.(1)) and bc = col (cell.(2)) in
		if m = 0 then rgb rc gc bc else if m = 1 then rgb rc 0 0 else if m = 2 then rgb 0 gc 0 else rgb 0 0 bc); 
		fill_rect (x*10) (y*10) (9) (9) in
	let rec it i j =
		if i < 100
		then
			(if j < 100
			then ((c_.(i).(j) <- [|c.(i).(j).(0)+var()+(lsum (List.map (fun d -> (c.(wrap(i+fst(d))).(wrap(j+snd(d))).(0)-(c.(i).(j).(0)))/(div)) n)); 
								  c.(i).(j).(1)+var()+(lsum (List.map (fun d -> (c.(wrap(i+fst(d))).(wrap(j+snd(d))).(1)-(c.(i).(j).(1)))/(div)) n));
								  c.(i).(j).(2)+var()+(lsum (List.map (fun d -> (c.(wrap(i+fst(d))).(wrap(j+snd(d))).(2)-(c.(i).(j).(2)))/(div)) n))|]);
				draw i j;
				it i (j+1))
			else it (i+1) 0) in
		let rec wait f =
			if key_pressed() then (let k = read_key() in if k = 'o' then (choose()) else if k = 'r' then wait 0 else if k = 'g' then wait 1 else if k = 'b' then wait 2 else wait f) else
			if button_down()
			then 
				(let mx = fst(mouse_pos())/10 and my = snd(mouse_pos())/10 in
				(c_.(mx).(my).(f) <- (c_.(mx).(my).(f)+1) mod 1000;
				draw mx my; Unix.sleepf 0.001;wait f))
			else wait f
		and choose () =
		let k = read_key() in
		match k with
		| 'p' -> di c_ m 'n'
		| 'm' -> di c_ ((m+1) mod 4) 'o'
		| 'n' -> di (b 0 'l') m 'o'
		| 'c' -> close_graph(); ignore(Unix.system "ocaml whatever_2d.ml")
		| 'd' -> di (b 0 'e') m 'o'
		| 'x' -> close_graph()
		| 'r' -> wait 0
		| 'g' -> wait 1
		| 'b' -> wait 2
		| _ -> choose()
		in 
		it 0 0; if button_down() || p = 'o' then choose() else di c_ m 'n'
let () = di (b 0 'l') 0 'n'
