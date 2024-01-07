open Graphics
open CAT
let () = ini(); info [
"";
"You also need to choose your CA."; 
"If you want a custom one, write a new file following the instructions above."; 
"Possibilities are: bosco, gol, beam, or it.";
"To choose, click once, then press the keys. The text will appear in the top left as you type it.";
"Press - to reset and = to validate";
"If you enter an unknown string, default is beam.";
"This screen will update to show the rules here.";
""];
let rec inp s = if key_pressed() then (let c = read_key() in if 'a' <= c && c <= 'z' then (Unix.sleepf 0.2; bl (s^(String.make 1 c)); inp (s^(String.make 1 c))) else if c = '-' then (Unix.sleepf 0.2; bl ""; inp "") else if c = '=' then s else inp s) else inp s in 
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
else if m = "it" then(
go
(fun l v -> let rec c l co = match l with | [] -> 0 | i::s -> if i = co then 1+c s co else c s co in
	let v1 = c l white and v2 = c l blue in
	if (v1 = v2 && 2<=v1 && v1 <= 4) || (v2 <= v1 && 1<=v2 && v1<=4)
	then white
	else if v1 <= v2 && 1 <= v1 && v2 <= 4
	then blue
	else if v1 + v2 != 3 && v1+v2 != 4
	then black
	else v)
[(1,1);(1,0);(1,-1);(0,1);(0,-1);(-1,1);(-1,0);(-1,-1)]
100
[|black; blue; white|]
(fun () -> let r = Random.int 3 in if r = 1 then white else if r = 2 then blue else black)
["";
"This is It, invented by a friend of mine and named so by me by lack of imagination.";
"Range 1 moore neighbourhood.";
"Two types of life, life 1 in white and life 2 in blue.";
"Rules are complicated to give, look in the code.";
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
"Three types of life (red green & blue), two neighbouroods (range one von neumann (orthogonal) and range one cross (diagonally).";
"A green cell becomes red.";
"A red cell becomes black.";
"A black cell becomes green if it is orthogonally next to a green cell and, either it is not next to a red diagonally,";
"    or it is next to a blue cell orthogonally and next to another diagonally.";
"A blue cell stays blue.";
""]
