open Graphics
open Cat
let () = ini(); info [
"";
"You also need to choose your CA.";
"If you want a custom one, write a new file following the instructions above.";
"Possibilities are: bosco, gol, beam, or arc.";
"I also keep some interesting but not that much CAs as ei, eii, eiii, and so forth for Experiment [roman numeral].";
"(There is also test for testing, which contains whatever I am building now.";
"It may not exist. It may crash. It may keep you from moving your mouse. Ye be warned.)";
"To choose, press space, then press the keys. The text will appear in the top left as you type it.";
"Press space to validate and backspace to delete the last character.";
"If you enter an unknown string, you will be asked again.";
"This screen will update to show the rules here, then you'll have to press space again to start.";
""];
let rec inp s = 
	if key_pressed() then 
		(let c = read_key() in 
		if c = ' ' then s else
		if c = char_of_int 8 then (let ns = String.sub s 0 (String.length s - 1) in Unix.sleepf 0.1; bl ns; inp ns) else
		(Unix.sleepf 0.1; bl (s^(String.make 1 c)); inp (s^(String.make 1 c))))
	else inp s in
let rec choose() = 
let m = inp "" in 
if m = "bosco" then(
go
(fun l v -> let rec cou l = match l with | [] -> 0 | i::s -> if i = white then 1+cou s else cou s in let c = cou l in if v = white then (if c >= 33 && c <= 57 then white else black) else if c >= 34 && c <= 45 then white else black)
(moore 5)
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
(moore 1)
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
(moore 1)
100
[|black; red; green; blue|]
(fun () -> let r = Random.int 4 in if r = 0 then red else if r = 1 then green else if r = 2 then blue else black)
["";
"This is Arc, invented by a me.";
"Named so because of the similarity of the common pattern of a sort of twisted, rapidly changing line to electric arcs.";
"Plus, red and green do the same just opposite (and reflect themselves and do things with the other) so it already made me think of electricity.";
"It has a tendency to expand to a medium density.";
"I recommend trying small patterns.";
"Something you can do is put a blue cell, let it fill everything, then put a red cell and a green cell next to it. That makes a kaleidoscope.";
"Interesting part is it is not just random, there are patterns and as far as I know it is not periodic (or very big periods).";
"Range 1 moore neighbourhood.";
"Three types of life, red blue green.";
"Any cell will become:";
" - black if it has no blue neighbours and, 0/1/4+ neighbours in either red or green, all the others rely on this being false.";
" - red if it has more red neighbours than green.";
" - green if it has more green neighbours than red.";
" - blue if it has exactly as much of the other two."]
)
else if m = "ei" then(
go
(fun l v -> let rec c l co = match l with | [] -> 0 | i::s -> if i = co then 1+c s co else c s co in
	let v3 = c l blue and v2 = c l green and v1 = c l red in
	if (v1 > 0 && v1 < 3)||(v2 > 0 && v3 > 0)
	then (if v1 > v2 then red else if v2 > v1 then green else blue)
	else black)
(moore 1)
100
[|black; red; green; blue|]
(fun () -> let r = Random.int 4 in if r = 0 then red else if r = 1 then green else if r = 2 then blue else black)
[
"";
"Weird thing found while balancing arc.";
"Name is ei for Experiment I, as I think I'll keep other diverse but not complex CAs.";
"Green & blue delimit spaces and red expands into these spaces from breaks in the barriers but dies easily so new spaces are created.";
""]
)
else if m = "eii" then (
go
(fun l v -> let rec c l co = match l with | [] -> 0 | i::s -> if i = co then 1+c s co else c s co in
	let v3 = c l blue and v2 = c l green and v1 = c l red in
	if (v1 > 0 && v2 > 0) || (v3 > 1)
	then (if v1 > v2 then red else if v2 > v1 then green else blue)
	else black)
(moore 1)
100
[|black; red; green; blue|]
(fun () -> let r = Random.int 4 in if r = 0 then red else if r = 1 then green else if r = 2 then blue else black)
["";
"Found also while fiddling with arc.."]
)
else if m = "beam" then(
go
(fun l v -> let o = List.nth l 0::List.nth l 1::List.nth l 2::List.nth l 3::[] and
d = List.nth l 4::List.nth l 5::List.nth l 6::List.nth l 7::[] in
let test l_ col = List.exists (fun x -> x=col) l_ in 
if v = black then (if test o green && (not (test d red) || (test d blue && test o blue)) then green else black) else
if v = red then black else
if v = green then red else
if (not (test o blue) && not(test d green) && test o red) then green else blue)
(neumann 1@saltire 1)
100
[|black; blue; green; red|]
(fun () -> let r = Random.int 10 in if r < 5 then black else if r < 7 then red else if r < 9 then green else blue)
["";
"This is Beams, invented by me.";
"Made to easily allow recreating logic system.";
"Main (and maybe only) ship is 2*1, or gate is 2*1, and there are a lot of guns.";
"The ships, called sparks, are made of a green cell and a red one and go forth at c towards their green cell.";
"Named so because often when there is a gun it just looks like a continuous flow of yellow (r+g) light being reflected or split by the blue.";
"Three types of life (red green & blue), two neighborhoods (range one von neumann (orthogonal) and range one saltire (diagonally).";
"A green cell becomes red.";
"A red cell becomes black.";
"A black cell becomes green if it is orthogonally next to a green cell and, either it is not next to a red diagonally,";
"    or it is next to a blue cell orthogonally and next to another diagonally.";
"A blue cell stays blue.";
""])
else (bl "UNKOWN CA"; choose()) in choose()
