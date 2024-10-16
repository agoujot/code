open Graphics
open Cat
let () =
let rec choose() = 
bl " - choosing CA - ";
info ["";
"Enter CA name.";
"Possibilities are:";
" - standard ones: gol, bosco, daynight, marine";
" - good ones I made: beam, eiii, flower, evi";
" - more gimmicky/historical ones: evii, eiv, eii, ei, arc";
"(e[roman number] stands for experiments)";
"And test, that containes whatever I am building right now.";
"It might not exist, it might be rubbish, it might keep you from moving your mouse.";
"Ye be warned.";
"Put X to close.";
""]; wait();
let m = inp "" in 
if m = "X" then () else
begin
if m = "bosco" then(
go
(bs "34-45" "33-57")
(moore 5)
100
[|black; white|]
equ
["";
"This is Bosco's rule, a higher-range outer-totalistic CA, invented by Kellie Evans";
"Named after a reflectorless flipping oscillator named Bosco.";
"Range 5 moore neighbourhood.";
"One type of life, B34-45/S33-57.";
""]
)
else if m = "gol" then(
go
(bs "3" "2-3")
(moore 1)
100
[|black; white|]
equ
["";
"This is the Game of Life, one of the first CAs, invented by John Conway.";
"It is still one of the most complex.";
"Range 1 moore neighbourhood.";
"One type of life, B3/S2-3.";
""]
)
else if m = "arc" then(
go
(fun l v ->
	let v3 = count l blue and v2 = count l green and v1 = count l red in
	if (v1 > 0 && v2 > 0) || v3 > 0
	then (if v1 > v2 then red else if v2 > v1 then green else blue)
	else black)
(moore 1)
100
[|black; red; green; blue|]
equ
["";
"This is Arc, invented by me.";
"Named so because of the similarity of the common pattern of a sort of twisted, rapidly changing line to electric arcs.";
"Plus, red and green do the same just opposite (and reflect themselves and do things with the other) so it already made me think of electricity.";
"It has a tendency to expand to a medium density.";
"I recommend trying small patterns.";
"Something you can do is put a blue cell, let it fill everything, then put a red cell and a green cell next to it. That makes a kaleidoscope.";
"Interesting part is it is not just random, there are patterns and as far as I know it is not periodic (or very big periods).";
"Range 1 moore neighbourhood.";
"Three types of life, red blue green.";
"Any cell will become:";
" - black if it has no blue neighbours and, no green or no red neighbours, all the others rely on this being false.";
" - red if it has more red neighbours than green.";
" - green if it has more green neighbours than red.";
" - blue if it has exactly as much of the other two."]
)
else if m = "ei" then(
go
(fun l v ->
	let v3 = count l blue and v2 = count l green and v1 = count l red in
	if (v1 > 0 && v1 < 3)||(v2 > 0 && v3 > 0)
	then (if v1 > v2 then red else if v2 > v1 then green else blue)
	else black)
(moore 1)
100
[|black; red; green; blue|]
equ
[
"";
"Weird thing found while balancing arc.";
"Name is ei for Experiment I, as I think I'll keep other diverse but not complex CAs.";
"Green & blue delimit spaces and red expands into these spaces from breaks in the barriers but dies easily so new spaces are created.";
""]
)
else if m = "eii" then (
go
(fun l v ->
	let v3 = count l blue and v2 = count l green and v1 = count l red in
	if (v1 > 0 && v2 > 0) || (v3 > 1)
	then (if v1 > v2 then red else if v2 > v1 then green else blue)
	else black)
(moore 1)
100
[|black; red; green; blue|]
equ
["";
"Found also while fiddling with arc.";
"Actually by needing two blue neighbours not one.";
"Lower life counts, not soup.";
"Interesting patterns.";
"";]
)
else if m = "eiii" then (
go
(fun l v ->
	let v1 = count l red and v2 = count l green and v3 = count l blue in
		if (v1 > 0 && v2 > 0) || v3 > 1 then
			(if v1 > v2 then red 
			else if v2 > v1 then green 
			else blue)
		else black)
((0, 0)::moore 1)
100
[|black; red; green; blue|]
equ
["";
"Balanced eii with dependancy upon previous condition.";
"Namely, the cell itself counts as one of its neighbours.";
"(if you do the same with arc you end up with imploding loops.)";
"Some of the patterns of eii are still visible.";
"This one also has a tendancy for spinning guns and blue explosions.";
"Even found, but it is a bit rare, a gun that moves diagonally as it shoots.";
"Should find a name for this, as it is remarkably not stable (compared to ei, eii, arc, or eiv)";
""]
)
else if m = "ev" then (
go
(bs "15-25" "14-24")
(moore 3)
100
[|black; white|]
equ
["";
"Something. I think this is the best one in moore 3+(0,0) not depending on current state.";
""]
)
else if m = "beam" then(
go
(fun l v -> let o = List.nth l 0::List.nth l 1::List.nth l 2::List.nth l 3::[] and
d = List.nth l 4::List.nth l 5::List.nth l 6::List.nth l 7::[] in
let test l_ col = List.exists (fun x -> x=col) l_ in 
if v = black then (if test o green && (not (test d red) || (test d blue && test o blue)) then green else black) else
if v = red then black else
if v = green then red else
blue)
(neumann 1@saltire 1)
100
[|black; blue; green; red|]
(fun _ () -> let r = Random.int 100 in if r < 45 then black else if r < 65 then green else if r < 85 then red else blue)
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
else if m = "eiv" then(
go
(fun l v -> 
let rec split l i j b = match l with | [] -> [b] | h::t -> if i = j then b::split l 0 j [] else split t (i+1) j (h::b) in
let l_ = split l 0 4 [] in
if List.for_all (List.mem white) l_ && v = white then white else black)
(moore 1)
100
[|black; white|]
equ
["";
"Yet another experiment.";
"Arrived here by... uh... started as an attempt to recreate if/then/else structures for logic systems.";
"(Because A && B <=> if a then b else false and A || B <=> if a then true else b.)";
"Now does something else.";
"B/W, moore 1 except it is split into two halves.";
"A cell becomes white if it is already and both halves contain a white cell.";
"Does nice patterns, though the splitting in two of a moore 1 is assymetrical, because it is like this:";
"A A A";
"A X B";
"B B B";
"(With X being the cell and A and B the two neighbourhoods.)";
"My problem is it is not the same vertically and horizontally.";
"And if you do";
"A B A      A A B";
"B X B  or  B X B";
"A B A      B A A";
"It is too stable.";
""])
else if m = "evi" then (
go
(bs "7-10" "6-9")
(moore 2)
100
[|black; white|]
equ
["";"More or less optimized for 2*2.";""]
)
else if m = "daynight" then (
go
(bs "3,6-8" "3-4,6-8")
(moore 1)
100
[|black; white|]
equ
[""; "Day & Night, moore 1, B3,6-8/S3-4,6-8, has the notable property that black is the exact opposite of white."; ""]
)
else if m = "marine" then (
go
(bs "6-8" "4,6-9")
((moore 1) @ [(2,-1);(2,0);(2,1);(2,2);(1,2);(0,2);(-1,2)])
100
[|black; white|]
equ
[""; "Marine, B6-8/S4,6-9, uses an assymetrical neighbourhood that causes life to flow down a certain direction (hence the name)."; ""]
)
else if m = "flower" then(
go
(bs "6-8" "5-8")
[(-1,-1);(-1,-2);(-2,-1);(-2,0);(-2,1);(-1,1);(-1,2);(0,2);(1,2);(1,1);(2,1);(2,0);(2,-1);(1,-2);(0,-2);(1,-1)]
100
[|black; white|]
equ
[""; "Flower, called so because of its highly atypical neighbourhood:";
" XXX ";
"XX XX";
"X O X";
"XX XX";
" XXX ";
"Aside from that, B6-8/S5-8."; ""]
) else if m = "evii" then (
go
(bs "1" "0-2") (* or 0-3 for square thingy *)
(moore 1)
100
[|black; white|]
equ
["";
"B1/S0-2. Draws sierpinski triangles very often.";
"Originally to replicate a pattern I made long ago.";
""]
)
else if m = "eviii" then (
go
(fun l v ->
	if 
		(List.nth l 0 = red    ) || 
		(List.nth l 1 = green  ) ||
		(List.nth l 2 = blue   ) ||
		(List.nth l 3 = yellow )
		then List.nth [red; green; blue; yellow] (Random.int 4) else
	black
)
(neumann 1)
100
[|black; red; green; blue; yellow|]
equ
["";"Long story.";""]
)
else if m = "test" then(
go
(fun l v -> let x = count l white in 
	if x >= 4 && x <= 6 then white else black (*12 18, 15 25, 16 29, 17 34, 20 41 *))
((0,0)::moore 1)
250
[|black; white|]
equ
["";
"Yes, I know, nothing bad happened this time.";
"(That bit of freezing your mouse really happened to me once though)";
""])
else (bl "UNKOWN CA");
choose() end
in choose()
