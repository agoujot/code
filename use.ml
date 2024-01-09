open Graphics
open CAT
let () = ini(); info [
"";
"You also need to choose your CA."; 
"If you want a custom one, write a new file following the instructions above."; 
"Possibilities are: bosco, gol, beam, or arc.";
"(There is also test for testing, which contains whatever I am building now.";
"It may crash. It may keep you from moving your mouse. Ye be warned.)";
"To choose, press space, then press the keys. The text will appear in the top left as you type it.";
"Press - to reset and space to validate";
"If you enter an unknown string, default is beam.";
"This screen will update to show the rules here, then you'll have to press space again to start.";
""];
let rec inp s = if key_pressed() then (let c = read_key() in if 'a' <= c && c <= 'z' then (Unix.sleepf 0.1; bl (s^(String.make 1 c)); inp (s^(String.make 1 c))) else if c = '-' then (Unix.sleepf 0.1; bl ""; inp "") else if c = ' ' then s else inp s) else inp s in 
let m = inp "" in set_color black; fill_rect 0 0 1000 1000; if m = "bosco" then(
go
(fun l v -> let rec cou l = match l with | [] -> 0 | i::s -> if i = white then 1+cou s else cou s in let c = cou l in if v = white then (if c >= 33 && c <= 57 then white else black) else if c >= 34 && c <= 45 then white else black)
(let rec b i j = if i = 6 then [] else if j = 6 then b (i+1) (-5) else if i = 0 && j = 0 then b i (j+1) else (i, j)::b i (j+1) in b (-5) (-5))
100
[|black; white|]
(fun () -> let r = Random.int 2 in if r = 1 then white else black)
["";
"This is Bosco's rule, a higher-range outer-totalistic CA, invented by Kellie Evans";
"Named after a reflectorless flipping oscillator named Bosco.";
"Range 5 moore neighbourhood.";
"One type of life, S33-57 & B34-45.";
""]
)
else if m = "gol" then(
go
(fun l v -> let rec cou l = match l with | [] -> 0 | i::s -> if i = white then 1+cou s else cou s in let c = cou l in if c = 3 then white else if c = 2 then v else black)
[(1, 1);(1, 0);(1, -1);(0, 1);(0, -1);(-1, 1);(-1, 0);(-1, -1)]
100
[|white; black|]
(fun () -> let r = Random.int 2 in if r = 1 then white else black)
["";
"This is the Game of Life, one of the first CAs, invented by John Conway.";
"It is still one of the most complex.";
"Range 1 moore neighbourhood.";
"One type of life, S2-3 & B3.";
""]
)
else if m = "arc" then(
go
(fun l v -> let rec c l co = match l with | [] -> 0 | i::s -> if i = co then 1+c s co else c s co in
	let v3 = c l blue and v2 = c l green and v1 = c l red in
	if (v1 > 0 && v2 > 0) || v3 > 0
	then (if v1 > v2 then red else if v2 > v1 then green else blue)
	else black)
[(1,1);(1,0);(1,-1);(0,1);(0,-1);(-1,1);(-1,0);(-1,-1)]
100
[|black; red; green; blue|]
(fun () -> let r = Random.int 5 in if r = 0 then red else if r = 1 || r = 4 then green else if r = 2 then blue else black)
["";
"This is Arc, invented by a me.";
"Named so because of the similarity of the common pattern of a sort of twisted, rapidly changing line to electric arcs.";
"Plus, red and green do the same just opposite (and reflect themselves and do things with the other) so it already made me think of electricity.";
"It has a tendency to expand to a quite high density and stay a chaotic soup there.";
"Interesting part is it is not just random, there are patterns and as far as I know it is not periodic (or very big periods).";
"Range 1 moore neighbourhood.";
"Three types of life, red blue green.";
"Any cell will become:";
" - black if it has no blue neighbours and, 0/1/4+ neighbours in either red or green, all the others rely on this being false.";
" - red if it has more red neighbours than green.";
" - green if it has more green neighbours than red.";
" - blue if it has exactly as much of the other two."]
)
else if m = "test" then(
go
(*(fun l v -> let rec c l co = match l with | [] -> 0 | i::s -> if i = co then 1+c s co else c s co in
	let v1 = c l red and v2 = c l green and v3 = c l blue in
	if (v2 > 1&&v2 < 5) && v2 > v1 then green else
	if (v1 > 1&&v1 < 5) && v1 > v2 then red else
	if v3 > 0 && v1 = v2 && v1 > 0 then blue
	else black) *)
(fun l v -> let rec c l co = match l with | [] -> 0 | i::s -> if i = co then 1+c s co else c s co in
	let v3 = c l blue and v2 = c l green and v1 = c l red in
	if (if v1 > 0 && v1 < 5 then 3 else 0)+(if v2 > 0 && v2 < 5 then 2 else 0) + (if v3 > 0 then 2 else 0) >= 3
	then (if v1 > v2 then red else if v2 > v1 then green else blue)
	else black)
[(-1, -1);(-1, 0);(-1, 1);(0, -1);(0, 1);(1, -1);(1, 0);(1, 1)]
100
[|black; red; blue; green|]
(fun () -> let r = Random.int 4 in if r = 0 then red else if r = 1 then green else if r = 2 then blue else black)
[
"";
"Test CA. may do weird things.";
""]
)
else go
(fun l v -> let o = List.nth l 0::List.nth l 1::List.nth l 2::List.nth l 3::[] and
d = List.nth l 4::List.nth l 5::List.nth l 6::List.nth l 7::[] in
let test l_ col = List.exists (fun x -> x=col) l_ in 
if v = black then (if test o green && (not (test d red) || (test d blue && test o blue)) then green else black) else
if v = red then black else
if v = green then red else
if (not (test o blue) && not(test d green) && test o red) then green else blue)
[(1, 0);(-1, 0);(0, -1);(0, 1);(1, 1);(-1, 1);(1, -1);(-1, -1)]
100
[|black; blue; green; red|]
(fun () -> let r = Random.int 10 in if r < 5 then black else if r < 7 then red else if r < 9 then green else blue)
["";
"This is Beams, invented by me.";
"Made to easily allow recreating logic system.";
"Main (and maybe only) ship is 2*1, or gate is 2*1, and there are a lot of guns.";
"The ships, called sparks, are made of a green cell and a red one and go forth at c towards their green cell.";
"Named so because often when there is a gun it just looks like a continuous flow of yellow (r+g) light being reflected or split by the blue.";
"Three types of life (red green & blue), two neighborhoods (range one von neumann (orthogonal) and range one cross (diagonally).";
"A green cell becomes red.";
"A red cell becomes black.";
"A black cell becomes green if it is orthogonally next to a green cell and, either it is not next to a red diagonally,";
"    or it is next to a blue cell orthogonally and next to another diagonally.";
"A blue cell stays blue.";
""]
