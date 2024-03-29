open Graphics
open Cat
let () =
let rec inp s = 
	if key_pressed() then 
		(let c = read_key() in 
		if c = char_of_int 13 then s else
		if c = char_of_int 8 then (let ns = if s = "" then s else (String.sub s 0 (String.length s - 1)) in Unix.sleepf 0.1; bl ns; inp ns) else
		(Unix.sleepf 0.1; bl (s^(String.make 1 c)); inp (s^(String.make 1 c))))
	else inp s in
let rec choose() = 
bl " - choosing CA - ";
info ["";
"Enter CA name.";
"Possibilities are gol, bosco, arc, beam, ei, eii, eiii, eiv, (experiments).";
"And test, that containes whatever I am building right now.";
"It might not exist, it might be rubbish, it might keep you from moving your mouse.";
"Ye be warned.";
"Put exit to leave.";
""]; wait();
let m = inp "" in 
if m = "exit" then () else
begin
if m = "bosco" then(
go
(fun l v -> let rec cou l = match l with | [] -> 0 | i::s -> if i = white then 1+cou s else cou s in let c = cou l in if v = white then (if c >= 33 && c <= 57 then white else black) else if c >= 34 && c <= 45 then white else black)
(moore 5)
100
[|black; white|]
equ
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
equ
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
equ
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
" - black if it has no blue neighbours and, no green or no red neighbours, all the others rely on this being false.";
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
(fun l v -> let rec c l co = match l with | [] -> 0 | i::s -> if i = co then 1+c s co else c s co in
	let v3 = c l blue and v2 = c l green and v1 = c l red in
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
(fun l v -> let rec c l co = match l with | [] -> 0 | i::s -> if i = co then 1+c s co else c s co in
	let v1 = c l red and v2 = c l green and v3 = c l blue in
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
(fun _ () -> let r = Random.int 100 in if r < 50 then black else if r < 70 then green else if r < 90 then red else blue)
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
"A blue cell stays blue (except if it has no blue neighbours, no diagonal green neighbours and a orthogonal red neighbour, in which case it becomes green.";
"That last condition is not really needed but it helps clear the random field from obstacles.)";
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
"Does nice patterns, though the spltting in two of a moore 1 is assymetrical, because it is like this:";
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
else if m = "test" then(
go
(fun l v -> if v = white then black else if List.nth l 0 = white then List.nth l 1 else List.nth l 2)
[(0, -1);(1, 0);(-1, 0)]
100
[|black; white|]
equ
["";
"Yes, I know, nothing bad happened this time.";
"(That bit of freezing your mouse really happened to me once though)";
""])
else (bl "UNKOWN CA");
choose() end
in choose()
