#load "graphics.cma"
#load "unix.cma"
open Graphics 
let () =if (print_endline "Do you want the info page ? [N/y]"; read_line())="y" then (print_endline "Welcome to the Game Of Life simulator (pattern matching version).";print_endline "Once you have entered the settings, it will start. "; print_endline "When the sim is running, you can pause it by clicking (keep the mouse button down until it stops).";print_endline "In pause, press keys to do the following:";print_endline"- p (Play) : restarts the sim.";print_endline "- e (Edit) : click on a cell to change it state, then the sim will go back to pause.";print_endline "- m (Manual Mode) : lets you change the state of any cells you click. To exit manual mode and go back to pause, click outside the window.";print_endline "- n (New) : reboots the sim from another random setting."; print_endline "- c (Change) : lets you change the settings.";print_endline "- d (Delete) : empties the grid (useful with Manual Mode).";print_endline "- x (EXit) : closes the sim."; print_endline "(Tip: e and m are slow (and it becomes hard to aim!) when size is > 100)";print_endline "Stable structures are colored in green, oscillators in blue and gliders in red. The rest, including rare structures that fit into one of the categories, is in white.")
let si = print_endline "Size of the grid in cells ? (int, recommended <= 100)"; read_int()
let z = print_endline "Width of a cell in pixels ? (int, size*this should <= your screen size)"; read_int()
let d = print_endline "Start density of life ? (int)"; print_string "1/"; read_int()
let time = print_endline "Delay between frames ? (float, there is also the delay of calculation time if you took a large size)"; read_float()
let () = Random.self_init(); open_graph ""; resize_window (si*z+1) (si*z+1); set_color black; fill_rect 0 0 (si*z) (si*z) 
let rec build x y l b =
        if x = si
        then b
        else
                if y = si
                then build (x+1) 0 [] (Array.append b [|Array.of_list(l)|])
                else
                        let i = Random.int d in
                        if i = 0
                        then build x (y+1) (l@[1]) b
                        else build x (y+1) (l@[0]) b
let rec dcopy t i = if i = Array.length t - 1 then [|Array.copy t.(i)|] else Array.append [|Array.copy t.(i)|] (dcopy t (i+1))
let turn a =
	let rec bu i j l =
		if i = Array.length(a.(0))
		then [||]
		else 
			if j = -1
                        then Array.append [|l|] (bu (i+1) (Array.length a - 1) [||])
			else bu i (j-1) (Array.append l [|a.(j).(i)|])
	in bu 0 (Array.length a - 1) [||]
let flip a =
        let rec it i =
                match i with
                | -1 -> [||]
                | _ -> Array.append [|a.(i)|] (it (i-1))
        in it (Array.length a -1)
let gs=
 let ga=[|[|2; 0; 0; 0; 0; 2|];
          [|0; 0; 1; 1; 0; 0|];
          [|0; 1; 0; 0; 1; 0|];
          [|0; 0; 1; 1; 0; 0|];
          [|2; 0; 0; 0; 0; 2|]|]
 and gb=[|[|0; 0; 0; 0; 2|];
          [|0; 1; 1; 0; 0|];
          [|0; 1; 0; 1; 0|];
          [|0; 0; 1; 0; 0|];
          [|2; 0; 0; 0; 2|]|]
 and gc=[|[|2; 0; 0; 0; 2; 2|];
          [|0; 0; 1; 0; 0; 2|];
          [|0; 1; 0; 1; 0; 0|];
          [|0; 1; 0; 0; 1; 0|];
          [|0; 0; 1; 1; 0; 0|];
          [|2; 0; 0; 0; 0; 2|]|]
 and gd=[|[|2; 0; 0; 0; 2|];
          [|0; 0; 1; 0; 0|];
          [|0; 1; 0; 1; 0|];
          [|0; 0; 1; 0; 0|];
          [|2; 0; 0; 0; 2|]|]
 and ge=[|[|2; 0; 0; 0; 0; 2|];
          [|0; 0; 1; 1; 0; 0|];
          [|0; 1; 0; 0; 1; 0|];
          [|0; 1; 0; 0; 1; 0|];
          [|0; 0; 1; 1; 0; 0|];
          [|2; 0; 0; 0; 0; 2|]|]
 and gf=[|[|0; 0; 0; 0; 2|];
          [|0; 1; 1; 0; 0|];
          [|0; 1; 0; 1; 0|];
          [|0; 0; 1; 1; 0|];
          [|2; 0; 0; 0; 0|]|]
 and gg=[|[|0; 0; 0; 0; 2; 2|];
          [|0; 1; 1; 0; 0; 2|];
          [|0; 1; 0; 1; 0; 0|];
          [|0; 0; 1; 0; 1; 0|];
          [|2; 0; 0; 1; 0; 0|];
          [|2; 2; 0; 0; 0; 2|]|]
 in let gb_=turn gb and gc_=turn gc and gg_=turn gg in let gb__=turn gb_ and gc__=turn gc_ and gg__=turn gg_ in let gb___=turn gb__ and gc___=turn gc__ and gg___=turn gg__ in
 [[|[|0; 0; 0; 0|]; 
    [|0; 1; 1; 0|]; 
    [|0; 1; 1; 0|]; 
    [|0; 0; 0; 0|]|]; ga; turn ga; gb; gb_; gb__; gb___; gc; gc_; gc__; gc___; gd; ge; gf; turn gf; gg; gg_; gg__; gg___]
let bs=
 let ba=[|[|0; 0; 0; 0; 0|];
          [|0; 1; 1; 1; 0|];
          [|0; 0; 0; 0; 0|]|]
 and bb=[|[|2; 0; 0; 0; 2; 2|];
          [|0; 0; 1; 0; 0; 0|];
          [|0; 1; 0; 0; 1; 0|];
          [|0; 1; 0; 0; 1; 0|];
          [|0; 0; 0; 1; 0; 0|];
          [|2; 2; 0; 0; 0; 2|]|]
 and bc=[|[|2; 0; 0; 0; 0; 0|];
          [|0; 0; 1; 1; 1; 0|];
          [|0; 1; 1; 1; 0; 0|];
          [|0; 0; 0; 0; 0; 2|]|] in let bb_ = flip bb and bc_ = flip bc
          in [ba; turn ba; bb; bc; turn bb; turn bc; bb_; turn bb_; bc_; turn bc_]
let rs=
 let ra=[|[|2; 2; 0; 0; 0|];
          [|0; 0; 0; 1; 0|];
          [|0; 1; 0; 1; 0|];
          [|0; 0; 1; 1; 0|];
          [|2; 0; 0; 0; 0|]|] 
 and rb=[|[|0; 0; 0; 2; 2|];
          [|0; 1; 0; 0; 0|];
          [|0; 0; 1; 1; 0|];
          [|0; 1; 1; 0; 0|];
          [|0; 0; 0; 0; 2|]|]
 and rc=[|[|2; 0; 0; 0; 2|];
          [|2; 0; 1; 0; 0|];
          [|0; 0; 0; 1; 0|];
          [|0; 1; 1; 1; 0|];
          [|0; 0; 0; 0; 0|]|]
 and rd=[|[|0; 0; 0; 0; 0|];
          [|0; 1; 0; 1; 0|];
          [|0; 0; 1; 1; 0|];
          [|2; 0; 1; 0; 0|];
          [|2; 0; 0; 0; 2|]|] in let ra_ = turn ra and rb_ = turn rb and rc_ = turn rc and rd_ = turn rd in
          let ra__ = turn ra_ and rb__ = turn rb_ and rc__ = turn rc_ and rd__ = turn rd_ in
          let ra___ = turn ra__ and rb___ = turn rb__ and rc___ = turn rc__ and rd___ = turn rd__
 in [ra; rb; rc; rd; ra_; rb_; rc_; rd_; ra__; rb__; rc__; rd__; ra___; rb___; rc___; rd___]
let rec doit g =
        let g_ = dcopy g 0
        in let rec c x y = 
                let rec c_ a b =
                if a = 2
                then 0
                else if b = 2
                     then c_ (a+1) (-1)
                     else if (a = 0 && b = 0) || a+x = si || a+x = -1 || b+y = si || b+y = -1
                          then c_ a (b+1)
                          else g.(x+a).(y+b) + c_ a (b+1)
                in c_ (-1) (-1)
        in let col x y =
                let rec test p v =
        		let rec itest i j =
		        	if i = Array.length p
                                then 1
        			else
			        	if j = Array.length(p.(i))
		        		then itest (i+1) 0
	        			else
        					let px = x+i-fst(v) and py = y+j-snd(v) in
					        if (if px >= 0 && py >= 0 && px < si && py < si then g_.(px).(py) = p.(i).(j) || p.(i).(j) = 2                                                                                         else p.(i).(j) <> 1)
				        	then itest i (j+1)
			        		else 0
		        in (if p.(fst(v)).(snd(v)) = 1 then itest 0 0 else 0)
	        in let rec iterco p x_ y_ =
        		if x_ = Array.length p
		        then false
	        	else
        			if y_ = Array.length(p.(0))
			        then iterco p (x_+1) 0
		        	else test p (x_, y_) = 1 || iterco p x_ (y_+1)
	        in let rec iterp l =
		        match l with
		        | [] -> false
                        | i::s -> iterco i 0 0 || iterp s
                in (if iterp gs then green else if iterp bs then blue else if iterp rs then red else white)
        in let rec update x y =
                if x < si
                then
                        if y = si
                        then update (x+1) 0
                        else
                                ((let v = c x y in
                                if v = 3
                                then g_.(x).(y) <- 1
                                else
                                if v <> 2
                                then g_.(x).(y) <- 0);
                                update x (y+1))
        in let rec show x y =
                if x < si
                then
                        if y = si
                        then show (x+1) 0
                        else
                                ((if g_.(x).(y) = 1
                                then set_color(col x y)
                                else set_color black);
                                fill_rect (z*x) (z*y) (z-1) (z-1);
                                show x (y+1))
        in update 0 0; show 0 0;Unix.sleepf time; if not (button_down()) then doit g_ else (let n = read_key() in 
        let rec choose k =
                match k with
                | 'n' -> doit(build 0 0 [] [||])
                | 'p' -> doit g_
                | 'e' -> (let rec wait ()= if (button_down()) then (let mx=fst(mouse_pos())/z and my=snd(mouse_pos())/z in (if g_.(mx).(my)=1 then g_.(mx).(my) <- 0 else g_.(mx).(my) <- 1)) 
                else wait() in wait(); show 0 0; choose(read_key()))
                | 'm' -> let rec wait()=
                        if (button_down())
                        then (let mx=fst(mouse_pos())/z and my=snd(mouse_pos())/z in 
                            (if mx >= 0 && mx < si && my >= 0 && my < si 
                            then ((if g_.(mx).(my)=1 then g_.(mx).(my) <- 0 else g_.(mx).(my) <- 1); show 0 0; Unix.sleepf 0.15;wait())
                            else choose(read_key())))
                        else wait() in wait()
                | 'd' -> let rec empty x y = if x < si then (if y = si then empty (x+1) 0 else (g_.(x).(y) <- 0; empty x (y+1))) in empty 0 0; show 0 0;choose(read_key())
                | 'c' -> close_graph(); ignore(Unix.system "ocaml gameoflife_sim.ml")
                | _ -> print_endline "Closed."
        in choose n)
let () = doit(build 0 0 [] [||])
