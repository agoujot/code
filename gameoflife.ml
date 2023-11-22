#load "graphics.cma"
#load "unix.cma"
open Graphics 
let info() = (print_endline "Welcome to the Game Of Life simulator."; print_endline "For more info about the Game Of Life, see the wikipedia page 'Conway's Game of Life' or the wiki conwaylife.com.";print_endline "Once you have entered the settings, it will start. "; print_endline "When the sim is running, you can pause it by clicking (keep the mouse button down until it stops).";print_endline "In pause, press keys to do the following:";print_endline"- p (Play) : restarts the sim.";print_endline "- e (Edit) : click on a cell to change it state, then the sim will go back to pause.";print_endline "- m (Manual Mode) : lets you change the state of any cells you click. To exit manual mode and go back to pause, click outside the window.";print_endline "- n (New) : reboots the sim from another random grid."; print_endline "- c (Change) : lets you change the settings.";print_endline "- d (Delete) : empties the grid (useful with Manual Mode).";print_endline "- r (Recognition) : toggles pattern recognition (slower)."; print_endline "- i (Info) : shows this page.";print_endline "- x (EXit) : closes the sim.";print_endline "For pattern recognition, the most common stables, oscillators and ships are colored, respectively, in green, blue, and red.";print_newline() ;print_endline "About the settings : "; print_endline "- for selection, [N/y] means that default is No and you need to enter y to change it, and a number between brackets means it is the default, and you can enter another number to change. For both, you can just press enter if the default is fine by you.";print_endline " - if you take size > 100, it may be not instant, especially with pattern coloring.";print_endline " - the size times the width of a cell should be less than your screen size, as you cannot zoom or un-zoom.";print_endline " - density 1 is useful to start with an already empty board.";print_endline" - you also need to take into account the calculation time if you took a large size."; print_newline(); print_endline"Have fun :) !")
let () =if (print_endline "Do you want the info page ? [N/y]"; read_line())="y" then info() else print_endline "Ok. If you want to see it later, you can stop the sim by clicking and get it by pressing i."
let si = let x = (print_endline "Size of the grid in cells ? [100]"; read_line()) in if x = "" then 100 else int_of_string x
let z = let x = (print_endline "Width of a cell in pixels ? [10]"; read_line()) in if x = "" then 10 else int_of_string x
let d = let x = (print_endline "Start density of life ? [2]"; print_string "1/"; read_line()) in if x = "" then 2 else int_of_string x
let time = let x = (print_endline "Delay between frames ? (float) [0.02]"; read_line()) in if x = "" then 0.02 else float_of_string x
let rule = print_endline "Enter the name of the rule you want: [GOL]"; print_endline "(If you mess with this, I highly recommend you look around conwaylife. Key words neighborhood, LtL and HROT.)"; read_line()
let neighbours = match rule with
| "bosco" -> [(-5,-5);(-5,-4);(-5,-3);(-5,-2);(-5,-1);(-5,0);(-5,1);(-5,2);(-5,3);(-5,4);(-5,5);(-4,-5);(-4,-4);(-4,-3);(-4,-2);(-4,-1);(-4,0);(-4,1); (-4,2);(-4,3);(-4,4);(-4,5);(-3,-5);(-3,-4);(-3,-3);(-3,-2);(-3,-1);(-3,0);(-3,1);(-3,2);(-3,3);(-3,4);(-3,5);(-2,-5);(-2,-4);(-2,-3);(-2,-2);(-2,-1);(-2,0);(-2,1);(-2,2);(-2,3);(-2,4);(-2,5);(-1,-5);(-1,-4);(-1,-3);(-1,-2);(-1,-1);(-1,0);(-1,1);(-1,2);(-1,3);(-1,4);(-1,5);(0,-5);(0,-4);(0,-3);(0,-2);(0,-1);(0,1);(0,2);(0,3);(0,4);(0,5);(1,-5);(1,-4);(1,-3);(1,-2);(1,-1);(1,0);(1,1);(1,2);(1,3);(1,4);(1,5);(2,-5);(2,-4);(2,-3);(2,-2);(2,-1);(2,0);(2,1);(2,2);(2,3);(2,4);(2,5);(3,-5);(3,-4);(3,-3);(3,-2);(3,-1);(3,0);(3,1);(3,2);(3,3);(3,4);(3,5);(4,-5);(4,-4);(4,-3);(4,-2);(4,-1);(4,0);(4,1);(4,2);(4,3);(4,4);(4,5);(5,-5);(5,-4);(5,-3);(5,-2);(5,-1);(5,0);(5,1);(5,2);(5,3);(5,4);(5,5)]
| "marine" -> [(1,-1);(1,0);(1,1);(1,2);(0,-1);(0,1);(0,2);(-1,-1);(-1,0);(-1,1);(-1,2);(-2,-1);(-2,0);(-2,1);(-2,2)]
| "factorio" -> [(-3,0);(-2,0);(-1,0);(1,0);(2,0);(3,0);(0,-3);(0,-2);(0,-1);(0,1);(0,2);(0,3)]
| "custom" -> (let typ = print_endline "Enter the type of neighborhood you want: [moore]"; read_line() in
match typ with
| "neumann" -> let ra = print_endline "Enter the range of your von Neumann neighborhood:"; read_int() in 
let rec rep x y = if x = ra+1 then [] else if y = abs(ra-abs(x))+1 then rep (x+1) (-abs(ra-abs(x))) else [(x,y)]@rep x (y+1)
in rep (-ra) 0
| "cross" -> let ra = print_endline "Enter the range of your cross neighborhood:"; read_int() in 
let rec rep x = if x = ra+1 then [] else if x != 0 then [(0,x);(x,0)]@rep (x+1) else rep (x+1)
in rep (-ra)
| "hash" -> let ra = print_endline "Enter the range of your hash neighborhood:"; read_int() in 
let rec rep x = if x = ra+1 then [(1,1);(1,0);(1,-1);(0,1);(0,-1);(-1,1);(-1,0);(-1,-1)] else [(1,x);(1,-x);(-1,x);(-1,-x);(x,1);(x,-1);(-x,1);(-x,-1)]@rep (x+1)
in rep 2
| "saltire" -> let ra = print_endline "Enter the range of your saltire neighborhood:"; read_int() in
let rec rep x = if x = ra+1 then [] else [(x,x);(x,-x);(-x,x);(-x,-x)]@rep (x+1)
in rep 1
| "custom" -> let rec en () = let x=read_int() and y = read_int() in if not(x = 0 && y = 0) then [(x,y)]@en() else []
in print_endline "Enter the tuples of variations, one value per line. 0 0 to stop."; en()
| _ -> let ra = print_endline "Enter the range of your Moore neighborhood:"; read_int() in 
let rec rep x y = if x = ra+1 then [] else if y = ra+1 then rep (x+1) (-ra) else if not (x = 0 && y = 0) then [(x,y)]@rep x (y+1) else rep x (y+1)
in rep (-ra) (-ra))
| _ -> [(1,1);(1,0);(1,-1);(0,1);(0,-1);(-1,1);(-1,0);(-1,-1)]
let counts = match rule with
	| "marine" -> ([6;7;8],[4;6;7;8;9])
	| "bosco" -> let rec range a b = if a = b+1 then [] else [a]@range (a+1) b in (range 34 45, range 33 57)
	| "factorio" -> ([3],[2])
	| "custom" -> print_endline "Enter the limits of the ranges, first those for birth and then those for survival.";let rec range a b = if a = b+1 then [] else [a]@range (a+1) b in (range (read_int()) (read_int()), range (read_int()) (read_int()))
	| _ -> ([3],[2;3])
let () = Random.self_init(); open_graph ""; resize_window (si*z+1) (si*z+1); set_color black; fill_rect 0 0 (si*z) (si*z)
let wrap co = if co < 0 then si+co else if co >= si then co-si else co
let fill x y = fill_rect (z*x) (z*y) (z-1) (z-1)
let rec build x y l b =
        if x = si
        then b
        else
                if y = si
                then build (x+1) 0 [] (Array.append b [|Array.of_list(l)|])
                else
                        let i = Random.int d in
                        if rule <> "it" then
                        (if i = 0
                        then (set_color white; fill x y; build x (y+1) (l@[1]) b)
                        else (set_color black; fill x y; build x (y+1) (l@[0]) b))
                        else
                        (if i = 0
                        then (set_color white; fill x y; build x (y+1) (l@[1]) b)
                        else (if i = 1
                        then (set_color (rgb 128 128 128); fill x y; build x (y+1) (l@[2]) b)
                        else (set_color black; fill x y; build x (y+1) (l@[0]) b)))
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
                if i = Array.length a
                then [||]
                else Array.append [|Array.of_list(List.rev (Array.to_list a.(i)))|] (it (i+1))
        in it (0)
let gs= match rule with
| "marine" -> []
| "factorio" -> []
| "bosco" -> []
| "it" -> []
| _ ->
 let ga=[|[|3; 0; 0; 0; 0; 3|];
          [|0; 0; 1; 1; 0; 0|];
          [|0; 1; 0; 0; 1; 0|];
          [|0; 0; 1; 1; 0; 0|];
          [|3; 0; 0; 0; 0; 3|]|]
 and gb=[|[|0; 0; 0; 0; 3|];
          [|0; 1; 1; 0; 0|];
          [|0; 1; 0; 1; 0|];
          [|0; 0; 1; 0; 0|];
          [|3; 0; 0; 0; 3|]|]
 and gc=[|[|3; 0; 0; 0; 3; 3|];
          [|0; 0; 1; 0; 0; 3|];
          [|0; 1; 0; 1; 0; 0|];
          [|0; 1; 0; 0; 1; 0|];
          [|0; 0; 1; 1; 0; 0|];
          [|3; 0; 0; 0; 0; 3|]|]
 and gd=[|[|3; 0; 0; 0; 3|];
          [|0; 0; 1; 0; 0|];
          [|0; 1; 0; 1; 0|];
          [|0; 0; 1; 0; 0|];
          [|3; 0; 0; 0; 3|]|]
 and ge=[|[|3; 0; 0; 0; 0; 3|];
          [|0; 0; 1; 1; 0; 0|];
          [|0; 1; 0; 0; 1; 0|];
          [|0; 1; 0; 0; 1; 0|];
          [|0; 0; 1; 1; 0; 0|];
          [|3; 0; 0; 0; 0; 3|]|]
 and gf=[|[|0; 0; 0; 0; 3|];
          [|0; 1; 1; 0; 0|];
          [|0; 1; 0; 1; 0|];
          [|0; 0; 1; 1; 0|];
          [|3; 0; 0; 0; 0|]|]
 and gg=[|[|0; 0; 0; 0; 3; 3|]; 
          [|0; 1; 1; 0; 0; 3|];
          [|0; 1; 0; 1; 0; 0|];
          [|0; 0; 1; 0; 1; 0|];
          [|3; 0; 0; 1; 0; 0|];
          [|3; 3; 0; 0; 0; 3|]|]
 in let gb_=turn gb and gc_=turn gc and gg_=turn gg in let gb__=turn gb_ and gc__=turn gc_ and gg__=turn gg_ in let gb___=turn gb__ and gc___=turn gc__ and gg___=turn gg__ in
 [[|[|0; 0; 0; 0|];
    [|0; 1; 1; 0|];
    [|0; 1; 1; 0|];
    [|0; 0; 0; 0|]|]; ga; turn ga; gb; gb_; gb__; gb___; gc; gc_; gc__; gc___; gd; ge; gf; turn gf; gg; gg_; gg__; gg___]
let bs=
 match rule with
 | "bosco" ->  []
 | "marine" -> []
 | "factorio" -> []
 | "it" -> []
 | _ ->
 let ba=[|[|0; 0; 0; 0; 0|];
          [|0; 1; 1; 1; 0|];
          [|0; 0; 0; 0; 0|]|]
 and bb=[|[|3; 0; 0; 0; 3; 3|];
          [|0; 0; 1; 0; 0; 0|];
          [|0; 1; 0; 0; 1; 0|];
          [|0; 1; 0; 0; 1; 0|];
          [|0; 0; 0; 1; 0; 0|];
          [|3; 3; 0; 0; 0; 3|]|]
 and bc=[|[|3; 0; 0; 0; 0; 0|];
          [|0; 0; 1; 1; 1; 0|];
          [|0; 1; 1; 1; 0; 0|];
          [|0; 0; 0; 0; 0; 3|]|] 
 and bd=[|[|0; 0; 0; 0; 3; 3|];
          [|0; 1; 1; 0; 3; 3|];
          [|0; 1; 1; 0; 0; 0|];
          [|0; 0; 0; 1; 1; 0|];
          [|3; 3; 0; 1; 1; 0|];
          [|3; 3; 0; 0; 0; 0|]|] 
 and be=[|[|0; 0; 0; 0; 3; 3|];
          [|0; 1; 1; 0; 3; 3|];
          [|0; 1; 0; 0; 0; 0|];
          [|0; 0; 0; 0; 1; 0|];
          [|3; 3; 0; 1; 1; 0|];
          [|3; 3; 0; 0; 0; 0|]|] in let bb_ = flip bb and bc_ = flip bc
    in [ba; turn ba; bb; bc; turn bb; turn bc; bd; turn bd; be; turn be; bb_; bc_; turn bb_; turn bc_]
let rs=
 match rule with
 | "bosco" -> []
 | "marine" -> []
 | "factorio" -> []
 | "it" -> []
 | _ ->
(let ra=[|[|3; 3; 0; 0; 0|];
          [|0; 0; 0; 1; 0|];
          [|0; 1; 0; 1; 0|];
          [|0; 0; 1; 1; 0|];
          [|3; 0; 0; 0; 0|]|]
 and rb=[|[|0; 0; 0; 3; 3|];
          [|0; 1; 0; 0; 0|];
          [|0; 0; 1; 1; 0|];
          [|0; 1; 1; 0; 0|];
          [|0; 0; 0; 0; 3|]|]
 and rc=[|[|3; 0; 0; 0; 3|];
          [|3; 0; 1; 0; 0|];
          [|0; 0; 0; 1; 0|];
          [|0; 1; 1; 1; 0|];
          [|0; 0; 0; 0; 0|]|]
 and rd=[|[|0; 0; 0; 0; 0|];
          [|0; 1; 0; 1; 0|];
          [|0; 0; 1; 1; 0|];
          [|3; 0; 1; 0; 0|];
          [|3; 0; 0; 0; 3|]|] in let ra_ = turn ra and rb_ = turn rb and rc_ = turn rc and rd_ = turn rd in
          let ra__ = turn ra_ and rb__ = turn rb_ and rc__ = turn rc_ and rd__ = turn rd_ in
          let ra___ = turn ra__ and rb___ = turn rb__ and rc___ = turn rc__ and rd___ = turn rd__
 in [ra; rb; rc; rd; ra_; rb_; rc_; rd_; ra__; rb__; rc__; rd__; ra___; rb___; rc___; rd___])
let rec doit g fast par =
        let g_ = dcopy g 0
        in let c x y t =
                let rec c_ l =
                        match l with
                        | [] -> 0
                        | i::s ->(let nx = x+fst(i) and ny = y+snd(i) in (if g.(wrap nx).(wrap ny) = t then 1 else 0) + c_ s) in c_ neighbours
        in let rec col x y =
                let rec test p v =
                        let rec itest i j =
                                if i = Array.length p
                                then 1
                                else
                                        if j = Array.length(p.(i))
                                        then itest (i+1) 0
                                        else
                                                let px = x+i-fst(v) and py = y+j-snd(v) in
                                                if g_.(wrap px).(wrap py) = p.(i).(j) || p.(i).(j) = 3
                                                then itest i (j+1)
                                                else 0
                        in (if p.(fst(v)).(snd(v)) <> 3 then itest 0 0 else 0)
                in let rec iterco p x_ y_ =
                        if x_ = Array.length p
                        then false
                        else
                                if y_ = Array.length(p.(0))
                                then iterco p (x_+1) 0
                                else (p.(x_).(y_) <> 0 && test p (x_, y_) = 1) || iterco p x_ (y_+1)
                in let rec iterp l =
                        match l with
                        | [] -> false
                        | i::s -> iterco i 0 0 || iterp s
                in (if iterp gs then (if g_.(x).(y) = 2 then rgb 0 128 0 else green) else if iterp bs then (if g_.(x).(y) = 2 then rgb 0 0 128 else blue) else if iterp rs then (if g_.(x).(y) = 2 then rgb 128 0 0 else red) else (if g_.(x).(y) = 2 then (rgb 128 128 128) else white))
        in let rec show x y =
                if x < si
                then
                        if y = si
                        then show (x+1) 0
                        else
                                ((if g_.(x).(y) <> 0
                                then set_color(col x y)
                                else set_color black);
                                fill x y;
                                show x (y+1))
        in let rec it x y =
                if x < si
                then
                        if y = si
                        then it (x+1) 0
                        else
                                (if rule <> "it" then
                                (let v = c x y 1 in
                                (if g_.(x).(y) = 1
                                then
                                (if not (List.mem v (snd(counts)))
                                then (g_.(x).(y) <- 0; if fast then (set_color black; fill x y)))
                                else
                                (if List.mem v (fst(counts))
                                then (g_.(x).(y) <- 1; if fast then (set_color white; fill x y)))); 
                                it x (y+1))
                                else
                                ((let v1 = c x y 1 and v2 = c x y 2 in
                                if (v1 = v2 && 2<=v1 && v1 <= 4) || (v2 <= v1 && 1<=v2 && v1<=4)
                                then (g_.(x).(y) <- 1; if fast && g.(x).(y) <> 1 then (set_color white; fill x y))
                                else if v1 <= v2 && 1 <= v1 && v2 <= 4
                                then (g_.(x).(y) <- 2; if fast && g.(x).(y) <> 2 then (set_color (rgb 128 128 128); fill x y))
                                else if v1 + v2 != 3 && v1+v2 != 4
                                then (g_.(x).(y) <- 0; if fast && g.(x).(y) <> 0 then (set_color black; fill x y))); it x (y+1)))
                else (if not(fast) then show 0 0)
        in it 0 0;Unix.sleepf time; if (not (button_down())) && par <> 1 then doit g_ fast 0 else (let n = read_key() and change x y = let nv = (g_.(x).(y) + 1) mod (if rule = "it" then 3 else 2) in g_.(x).(y) <- nv; if fast then (set_color (if nv = 2 then (rgb 128 128 128) else if nv = 1 then white else black); fill x y) else show 0 0 in
        let rec choose k =
                match k with
                | 'n' -> doit(build 0 0 [] [||]) fast 0
                | 'p' -> doit g_ fast 0
                | 'e' -> let rec wait ()= if (button_down()) then (let mx=fst(mouse_pos())/z and my=snd(mouse_pos())/z in change mx my) else wait() in wait(); choose(read_key())
                | 'm' -> (let rec wait()=
                        if (button_down())
                        then (let mx=fst(mouse_pos())/z and my=snd(mouse_pos())/z in 
                            (if mx >= 0 && mx < si && my >= 0 && my < si 
                            then (change mx my; Unix.sleepf 0.15; wait())
                            else choose(read_key())))
                        else wait() in wait())
                | 'd' -> let rec empty x y = if x < si then (if y = si then empty (x+1) 0 else (g_.(x).(y) <- 0; fill x y;empty x (y+1))) in set_color black; empty 0 0; choose(read_key())
                | 'c' -> print_endline "Enter new settings :"; close_graph(); ignore(Unix.system "gameoflife.ml")
                | 'r' -> (if not fast then (
                        let rec a x y =
                                if x < si 
                                then 
                                        if y = si
                                        then a (x+1) 0
                                        else
                                                (if g_.(x).(y) = 1
                                                then set_color white
                                                else set_color black;
                                                fill x y; a x (y+1))
                        in a 0 0)); doit g_ (not fast) 0
		| 'i' -> info(); choose(read_key())
                | 'f' -> doit g_ fast 1
                | _ -> print_endline "Closed."
        in choose n)
let () = doit (build 0 0 [] [||]) true 0
