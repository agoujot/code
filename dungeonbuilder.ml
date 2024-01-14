open Graphics
type salle = {e : int list; g : int array array; d : string; c : string * (int * int); v : string list}
let erase() = set_color black; fill_rect 0 0 1000 1000
let ic = open_in "salles.txt"
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
		if j = Array.length (a.(i)) then (if ti < 2 then "" else string_of_int ti)^(string_of_int ac)^"-|"^it (i+1) 0 (-1) 0 else
		if a.(i).(j) = ac then it i (j+1) ac (ti+1) else (if ti < 2 then "" else string_of_int ti)^(if ac >= 0 then string_of_int ac^"-" else "")^it i (j+1) (a.(i).(j)) 1
		in it 0 0 (-1) 0
let t = Hashtbl.create 10
let rec bl (s, v) = if s <> "0" then let cord = bl (Hashtbl.find t s).c in (fst cord + fst v, snd cord + snd v) else v
let toc s =
    let l = String.split_on_char ' ' s in
    (List.hd l, (int_of_string (List.nth l 1), int_of_string (List.nth l 2)))
let frc tup =
	fst tup^" "^string_of_int (fst (snd tup))^" "^string_of_int (snd(snd tup))
let pixel = int_of_string (input_line ic)
let etage = int_of_string (input_line ic)
let cor = let s = input_line ic in let l = String.split_on_char ' ' s in (int_of_string (List.nth l 0), int_of_string (List.nth l 1))
let () = 
	let rec it() = 
		try let nom = input_line ic and etage = input_line ic and grille = input_line ic and desc = input_line ic and co = input_line ic and voi = input_line ic in
		Hashtbl.add t 
		(String.sub nom 1 (String.length nom - 1)) 
		{e=List.map int_of_string (String.split_on_char ' ' etage); 
		g=tog(grille);
		d=desc;
		c=toc(co);
		v=String.split_on_char ' ' voi};
		it ()
		with End_of_file -> ()
	in it ()
let col_of_int x =
	match x with
	| 0 -> black
	| 1 -> white
	| 2 -> rgb 128 128 128
	| 3 -> red
	| 4 -> yellow
	| _ -> rgb 82 60 37
let display_room s z scroll tt =
	let r = Hashtbl.find tt s in
	let coord = bl (r.c) in
	let draw i j col = set_color col; let x = ((i+fst coord)*z+fst scroll) and y = ((j+snd coord)*z+snd scroll) in if x < 0 || y < 0 || x >= 1000 || y >= 1000 then () else fill_rect x y (z-1) (z-1) in 
	let rec it i j =
		if i = Array.length (r.g) then () else
		if j = Array.length (r.g.(i)) then it (i+1) 0 else
		(draw i j (let col = r.g.(i).(j) in col_of_int col); it i (j+1))
	in it 0 0; set_color blue; moveto (fst coord *z+fst scroll) (snd coord *z+snd scroll); (let x = fst coord *z+fst scroll+6*String.length s and y = snd coord * z + snd scroll + 6 * String.length s in if x < 0 || y < 0 || x >= 1000 || x >= 1000 then () else draw_string s)
let erase_console() = set_color (rgb 8 8 8); fill_rect 1000 0 500 1000
let display z eta scroll tt =
	erase(); Hashtbl.iter (fun k v -> if List.mem eta v.e then display_room k z scroll tt) tt
let rec write l x y = match l with | [] -> () | i::s -> (moveto x y; draw_string i; write s x (y-20))
let crop mat =
	let i_ = let rec it i = if Array.for_all (fun x -> x = 0) (mat.(i)) then it (i+1) else i in it 0
	and _i = let rec it i = if Array.for_all (fun x -> x = 0) (mat.(i)) then it (i-1) else i in it (Array.length mat - 1)
	and j_ = let rec it j = if List.for_all (fun x -> x = 0) (let rec b i = if i = Array.length mat then [] else mat.(i).(j)::b (i+1) in b 0) then it (j+1) else j in it 0
	and _j = let rec it j = if List.for_all (fun x -> x = 0) (let rec b i = if i = Array.length mat then [] else mat.(i).(j)::b (i+1) in b 0) then it (j-1) else j in it (Array.length (mat.(0))-1) in
	let rec ch i j l =
		if i = _i+1
		then [||]
		else
			if j = _j+1
			then Array.append [|l|] (ch (i+1) j_ [||])
			else ch i (j+1) (Array.append l [|mat.(i).(j)|])
	in ch i_ j_ [||]
let tr l = (erase_console(); set_color white; let l_ = let rec wrap l = match l with | [] -> [] | i::s -> if String.length i > 78 then String.sub i 0 78::wrap(String.sub i 78 (String.length i - 78)::s) else i::wrap s in wrap l in write l_ 1020 980)
let rec inp s =
	if key_pressed() then (let k = read_key() in if int_of_char k = 13 then (erase_console(); s) else if int_of_char k = 8 then ((if s <> "" then tr [String.sub s 0 (String.length s - 1)] else tr [""]); inp (if s <> "" then String.sub s 0 (String.length s - 1) else "")) else (let ns = s^(String.make 1 k) in tr [ns]; inp ns)) else inp s
let edit mat z =
	let rec wa c = 
		if button_down() then (let mi = fst(mouse_pos())/z and mj = snd(mouse_pos())/z in
		mat.(mi).(mj) <- c; set_color (col_of_int c); fill_rect (mi*z) (mj*z) (z-1) (z-1); wa c)
		else if key_pressed() then (let k = read_key() in if k = '0' || k = '1' || k = '2' || k = '3' || k = '4' || k = '5' then wa (int_of_char k - 48 ) else if k = 's' then crop mat else wa c)
		else wa c
	in tr ["0 pour des cases noires, 1 pour des blanches, 2 pour des grises, 3 pour des rouges, 4 pour des jaunes et 5 pour des marrons.";"Appuyez sur s pour sauver quand vous avez fini."]; wa 1
let modi g =
	set_color (rgb 4 4 4); fill_rect 0 0 1000 1000; let si = max (Array.length g) (Array.length g.(0)) in let z = 1000 / si in
	let mat = 
		let rec fill i j l = 
			if i = Array.length g 
			then (if i < Array.length g.(0) then Array.make_matrix (Array.length g.(0) - i ) (Array.length g.(0)) 0 else [||]) 
			else
  				if j = Array.length g.(0) 
				then Array.append [|Array.append l (if j < Array.length g then Array.make (Array.length g - j) 0 else [||])|] (fill (i+1) 0 [||]) 
				else fill i (j+1) (Array.append l [|g.(i).(j)|])
		in fill 0 0 [||] in
	let rec show i j =
		if i = Array.length mat then () else
		if j = Array.length mat.(i) then show (i+1) 0 else
		(set_color (col_of_int mat.(i).(j)); fill_rect (i*z) (j*z) (z-1) (z-1); show i (j+1))
	in show 0 0; edit mat z
let create() = 
	set_color black; fill_rect 0 0 1000 1000; tr ["Entrez la taille maximale."]; let ma = int_of_string(inp "") in let z = 1000/ma in
	let mat = Array.make_matrix ma ma 0 in edit mat z
let select_room z eta scroll tt =
	let rec wa eta scroll ch = 
		if ch then display z eta scroll tt;
		if button_down() then (let mi = (fst(mouse_pos())-fst scroll)/z and mj = (snd(mouse_pos())-snd scroll)/z in
			let rec find l = 
				match l with
				| [] -> "0"
				| i::s -> let r = Hashtbl.find tt i in let cord = bl (r.c) in if List.mem eta r.e && fst cord <= mi && snd cord <= mj && fst cord + Array.length r.g >= mi && snd cord + Array.length (r.g.(0)) >= mj then i else find s
			in find (List.of_seq (Hashtbl.to_seq_keys tt))) 
		else (if key_pressed ()
		then match read_key() with
			| 'z' -> wa eta (fst scroll, snd scroll+10) true
			| 'q' -> wa eta (fst scroll-10, snd scroll) true
			| 's' -> wa eta (fst scroll, snd scroll-10) true
			| 'd' -> wa eta (fst scroll+10, snd scroll) true
			| '<' -> wa (eta+1) scroll true
			| '>' -> wa (eta-1) scroll true
			| _ -> wa eta scroll false
		else wa eta scroll false)
	in tr ["Cliquez sur une salle pour la selectionner. (Dans le vide pour aucune)"]; wa eta scroll false
let rec placer na r z eta scroll =
	let t_  = Hashtbl.create 10 in
	let save () = 
		let rec b l =
			match l with
			| [] -> ()
			| (k, v)::s -> (if Hashtbl.mem t_ k then Hashtbl.replace t_ k {v with c = ("0", bl v.c)} else Hashtbl.add t_ k {v with c = ("0", bl v.c)}; b s) in 
		b (List.of_seq (Hashtbl.to_seq t)) in
	save(); 
	Hashtbl.remove t na;
	display z eta scroll t_;
	let sa = select_room z eta scroll t_ in
	tr ["Placer en fonction de "^sa^"."; "Cliquez la où vous voulez mettre le coin en bas a gauche."]; Unix.sleepf 0.2;
	let rec wa() =
		if button_down() then
		(let mi = (fst(mouse_pos())-fst scroll)/z-fst (if sa = "0" then (0, 0) else bl (Hashtbl.find t_ sa).c) and mj = (snd(mouse_pos())-snd scroll)/z-snd(if sa = "0" then (0, 0) else bl (Hashtbl.find t_ sa).c) in
		Hashtbl.add t na {r with c = (sa, (mi, mj))}; save(); display z eta scroll t_; 
		tr ["Entree pour valider, delete pour rechoisir une salle et autre chose pour refaire."];
		let m = read_key() in if int_of_char m = 13 then (tr [""]) else if int_of_char m = 8 then placer na r z eta scroll else(Hashtbl.remove t na; display z eta scroll t_; wa()))
		else wa()
	in wa()
let rec di z eta scroll =
	display z eta scroll t;
	match read_key() with
	| 'z' -> di z eta (fst scroll, snd scroll+10)
	| 'q' -> di z eta (fst scroll-10, snd scroll)
	| 's' -> di z eta (fst scroll, snd scroll-10)
	| 'd' -> di z eta (fst scroll+10, snd scroll)
	| '<' -> di z (eta+1) scroll
	| '>' -> di z (eta-1) scroll
	| 'l' -> tr("Liste des salles: "::List.map (fun k -> " - "^k) (List.of_seq (Hashtbl.to_seq_keys t))); di z eta scroll
	| '+' -> let na = tr ["Entrer nom."]; inp "" in
		tr ["Etage (s) ?"]; let et = List.map int_of_string (String.split_on_char ' ' (inp "")) in
		let gr = create() in
		tr ["Description ?"]; let de = inp "" in
		tr ["Salles voisines ?"]; let vo = String.split_on_char ' ' (inp "") in
		display z eta scroll t;
		placer na {e=et; g=gr; d=de; c=("0", (0, 0)); v=vo} z eta scroll;
		di z eta scroll
	| '-' -> let na = select_room z eta scroll t in if Hashtbl.mem t na then (Hashtbl.remove t na; di z eta scroll) else (tr ["Salle non existante."]; di z eta scroll)
	| 't' -> tr ["Entrer taille."]; di (int_of_string(inp "")) eta scroll
	| 'c' -> tr ["Les commandes disponibles sont: ";" - l : donne la liste des salles";" - + : cree une nouvelle salle";" - - : supprime une salle";" - t : redefinit la taille d'une case en pixels";" - c : affiche ceci";" - i : donne des informations sur d'une salle";" - m : modifie une salle. apres avoir choisi, appuyez sur n/g/d/r/v pour selectionner l'aspect que vous voulez changer.";" - f : ferme tout.";  " - z, q, s, ou d pour defiler respectivement vers le haut, la gauche, le bas, et la droite.";" < pour aller a l'etage en dessous (donc avec le numero plus grand)";" - > pour remonter un etage."]; di z eta scroll
	| 'i' -> (let na = select_room z eta scroll t in tr (if Hashtbl.mem t na then (let r = (Hashtbl.find t na) in [na; ""; r.d; ""; "Liee a :"]@List.map (fun x -> " - "^x) r.v) else ["Salle non existante."])); di z eta scroll
	| 'm' -> ((let na = select_room z eta scroll t in 
		if Hashtbl.mem t na 
		then 
			(tr ["Modification de "^na^"."; "Faites n pour changer le nom, g pour modifier la grille, d pour changer la description, r pour la replacer et v pour changer les voisins."]; 
			Unix.sleepf 0.1; let r = Hashtbl.find t na in 
			(Unix.sleepf 0.1; match read_key() with
			| 'n' -> tr ["Entrer le nouveau nom."]; let nn = inp "" in Hashtbl.remove t na; Hashtbl.add t nn r; Hashtbl.iter (fun k v -> Hashtbl.replace t k {v with c = if fst v.c = na then (nn, snd v.c) else v.c; v = List.map (fun x -> if x = na then nn else x) v.v}) t
			| 'g' -> Hashtbl.replace t na {r with g = modi(r.g)}
			| 'd' -> tr ["Entrer la description."]; Hashtbl.replace t na {r with d = inp ""}
			| 'r' -> placer na r z eta scroll
			| 'v' -> tr ["Entrer les voisins separes par des espaces."]; Hashtbl.replace t na {r with v = let rec rem l = match l with [] -> [] | i::s -> if Hashtbl.mem t i then i::rem s else rem s in rem (String.split_on_char ' ' (inp ""))}
			| _ -> tr ["n/g/d/r/v."]))
		else tr ["Salle non existante."]); 
		Unix.sleepf 0.1; di z eta scroll)
	| 'f' -> close_in ic; ignore (Unix.system "cp salles.txt sauvegarde_salles.txt"); 
		let oc = open_out "salles.txt" in 
		output_string oc (string_of_int z^"\n"); output_string oc (string_of_int eta^"\n"); output_string oc (string_of_int (fst scroll)^" "^string_of_int(snd scroll)^"\n");
		Hashtbl.iter (fun k v -> output_string oc ("_"^k^"\n"); output_string oc ((String.concat " " (List.map string_of_int v.e))^"\n"); output_string oc (frg v.g^"\n"); output_string oc (v.d^"\n"); output_string oc (frc v.c^"\n"); output_string oc (String.concat " " v.v^"\n")) t; close_out oc
	| _ -> tr ["Commande inconnue. Faites c pour en avoir la liste."]; di z eta scroll
let () = open_graph ""; resize_window 1500 1000; erase(); erase_console(); di pixel etage cor
