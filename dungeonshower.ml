open Graphics
type salle = {e : int list; g : int array array; c : string * (int * int); s : bool}
let erase() = set_color black; fill_rect 0 0 1000 1000
let tog s =
	let rec it i l n =
		if i = String.length s then [||] else
		if s.[i] = '|' then Array.append [|l|] (it (i+1) [||] "") else
		if s.[i+1] = '-' then it (i+2) (Array.append l (Array.make (if n = "" then 1 else int_of_string n) (int_of_char s.[i] - 48))) "" else
		it (i+1) l (n^(String.make 1 s.[i])) in
	it 0 [||] ""
let frg a =
	let rec it i j ac ti =
		if i = Array.length a then "" else
		if j = Array.length a.(i) then (if ti < 2 then "" else string_of_int ti)^string_of_int ac^"-|"^it (i+1) 0 (-1) 0 else
		if a.(i).(j) = ac then it i (j+1) ac (ti+1) else (if ti < 2 then "" else string_of_int ti)^(if ac >= 0 then string_of_int ac^"-" else "")^it i (j+1) (a.(i).(j)) 1
		in it 0 0 (-1) 0
let t = Hashtbl.create 50
let rec bl (s, v) = if s <> "0" then let cord = bl (Hashtbl.find t s).c in (fst cord + fst v, snd cord + snd v) else v
let rec explode s = let rec it s = if s = "" then [] else s.[0]::explode (String.sub s 1 (String.length s - 1)) in it s
let implode l = let rec b l = match l with | [] -> "" | i::s -> (String.make 1 i)^b s in b l
let toc s =
    let l = String.split_on_char ' ' s in
    (List.hd l, (int_of_string (List.nth l 1), int_of_string (List.nth l 2)))
let frc tup =
	fst tup^" "^string_of_int (fst (snd tup))^" "^string_of_int (snd(snd tup))
let read() =
	Hashtbl.clear t;
	let ic = open_in "user.txt" in
	let zz = int_of_string (input_line ic) and etaa = int_of_string (input_line ic) and scrolll = let s = input_line ic in let l = String.split_on_char ' ' s in (int_of_string (List.nth l 0), int_of_string (List.nth l 1)) in
	let rec it() = 
		try let nom = input_line ic and et = List.map int_of_string (String.split_on_char ' ' (input_line ic)) and gr = tog (input_line ic) and co = toc (input_line ic) and se = bool_of_string (input_line ic) in
		Hashtbl.add t nom {e=et; g=gr; c=co; s=se};
		it ()
		with End_of_file -> close_in ic
	in it (); (zz, etaa, scrolll)
let display_room s z eta scroll=
	let r = Hashtbl.find t s in
	let coord = bl r.c in
	let draw i j col = set_color col; let x = ((i+fst coord)*z+fst scroll) and y = ((j+snd coord)*z+snd scroll) in if x < 0 || y < 0 || x >= 1000 || y >= 1000 then () else fill_rect x y (z-1) (z-1) in 
	let rec it i j =
		if i = Array.length r.g then () else
		if j = Array.length r.g.(i) then it (i+1) 0 else
		((let v = r.g.(i).(j) in if v = 0 then () else if v = 1 || v = 4 then draw i j white else if v = 2 || v = 4 then draw i j (rgb 128 128 128) else draw i j (rgb 139 69 19)); it i (j+1))
	in it 0 0
let display z eta scroll =
	erase(); Hashtbl.iter (fun k v -> if List.mem eta v.e && v.s then display_room k z eta scroll) t
let rec som l c =
	match l with
	| c::s -> 1+som s c
	| _ -> 0
let rec di z eta scroll =
	display z eta scroll;
	let ic = open_in "orders.txt" in 
	let o = input_line ic in
	close_in ic; 
	let oc = open_out "orders.txt" in output_string oc "nothing to see"; 
	close_out oc;
	match explode o with
	| 'z'::l -> di z eta (fst scroll, snd scroll+10*(som l 'z'+1))
	| 'q'::l -> di z eta (fst scroll-10*(som l 'q'+1), snd scroll)
	| 's'::l -> di z eta (fst scroll, snd scroll-10*(som l 's'+1))
	| 'd'::l -> di z eta (fst scroll+10*(som l 'd'+1), snd scroll)
	| '<'::l -> di z (eta+som l '<'+1) scroll
	| '>'::l -> di z (eta-1-som l '>') scroll
	| 'f'::_ -> let oc = open_out "user.txt" in output_string oc (string_of_int z^"\n"^string_of_int eta^"\n"^string_of_int(fst scroll)^" "^string_of_int(snd scroll)^"\n"); Hashtbl.iter (fun k v -> output_string oc (k^"\n"^String.concat " " (List.map string_of_int v.e)^"\n"^frg v.g^"\n"^fst v.c^" "^string_of_int(fst (snd v.c))^" "^string_of_int(snd(snd v.c))^"\n"^string_of_bool v.s^"\n")) t; close_out oc; di z eta scroll
	| 'x'::_ -> ()
	| 'a'::' '::sa_ -> (let sa = implode sa_ in let r = Hashtbl.find t sa in di z (List.hd r.e) (let co = bl r.c in (-(fst co)*z, -(snd co)*z)))
	| 't'::' '::zz -> di (int_of_string (implode zz)) eta scroll
	| 'b'::' '::sa_ -> let sa = implode sa_ in let r = Hashtbl.find t sa in Hashtbl.replace t sa {r with s = true}; di z eta scroll
	| _ -> Unix.sleepf 0.1; di z eta scroll
let () = 
	open_graph ""; 
	resize_window 1000 1000; 
	set_window_title "Dungeon Shower";
	erase(); 
	let (zz, etaa, scrolll) = read() in
	di zz etaa scrolll
