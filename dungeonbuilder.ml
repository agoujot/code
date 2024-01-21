open Graphics
type salle = {e : int list; g : int array array; d : string; c : string * (int * int); v : string list; j : int array array} (* e pour etage(s), g pour grille, d pour description, c pour coordonnées, v pour voisins et j pour joueurs (ce qu'ils voient)*)
type entite = {s : string; b : string -> string; t : string; d : string} (*s pour salle, b pour behaviour, t la string qui a donné b, d pour description*)
let erase() = set_color black; fill_rect 0 0 1000 1000
let erase_console() = set_color (rgb 16 16 16); fill_rect 1000 0 500 1000
let rec write l x y = match l with | [] -> () | i::s -> (moveto x y; draw_string i; write s x (y-20))
let tr l = (erase_console(); set_color white; let l_ = let rec wrap l = match l with | [] -> [] | i::s -> if String.length i > 78 then String.sub i 0 78::wrap(String.sub i 78 (String.length i - 78)::s) else i::wrap s in wrap l in write l_ 1020 980) (* affiche du texte dans la console en faisant attention à ne rien mettre offscreen.*)
let rec inp s =
	if key_pressed() then 
		(let k = read_key() in 
		if int_of_char k = 13 (* entree *)
		then (erase_console(); s) 
		else if int_of_char k = 8 (* backspace *)
		then ((if s <> "" then tr [String.sub s 0 (String.length s - 1)] else tr [""]); inp (if s <> "" then String.sub s 0 (String.length s - 1) else "")) 
		else (let ns = s^(String.make 1 k) in tr [ns]; inp ns)) else inp s (* prend une entree avec les touches *)
let tog s = (* to grid, transforme une chaine compressee en tableau de tableaux d'entiers *)
	let rec it i l n = (* pour iterer, i l'indice dans la chaine, l la ligne partiellement construite, et n le coefficient partiellement construit *)
		if i = String.length s then [||] else
		if s.[i] = '|' then Array.append [|l|] (it (i+1) [||] "") else (* une fin de ligne *)
		if s.[i+1] = '-' then it (i+2) (Array.append l (Array.make (if n = "" then 1 else int_of_string n) (int_of_char s.[i] - 48))) "" else (* fin d'une suite de cases identiques. Comme on peut le voir ici, il y a max 10 couleurs (sinon je devrais rajouter un symbole pour separer le coeff et le code couleur) *)
		it (i+1) l (n^(String.make 1 s.[i])) in
	it 0 [||] ""
let frg a = (* from grid, compresse un int array array en string *)
	let rec it i j ac ti = (* it est toujours pour iterate, i et j les indices, ac pour actuel, le dernier nombre, et ti pour times,le nombre de fois que on a vu ac de suite *)
		if i = Array.length a then "" else
		if j = Array.length a.(i) then 
			(if ti < 2 (* si le coeff est 1 (ou 0 en debut de ligne) *)
			then "" (* on ne le met pas *)
			else string_of_int ti)
			^string_of_int ac^"-|"^it (i+1) 0 (-1) 0 (*les deux dernieres valeurs sont juste là pour signifier que en debut de ligne il faut toujours ecrire *) else
		if a.(i).(j) = ac 
		then it i (j+1) ac (ti+1) 
		else (if ti < 2 then "" else string_of_int ti)^(if ac >= 0 then string_of_int ac^"-" else "")^it i (j+1) (a.(i).(j)) 1
	in if a = [|[||]|] then "|" (*cas spécial pour le tableau vide pour j*) else it 0 0 (-1) 0
let t = Hashtbl.create 50 (* t pour table (c'etait la premiere), table de hachage qui associe au nom d'une salle un enregistrement de type salle. *)
let p = Hashtbl.create 50 (* p pour population, table de hachage qui associe au nom d'une entite un enregistrement de type entite. *)
let a = Hashtbl.create 10 (* pour les alarmes, associe nom de l'alarme à une liste des trois éléments, à savoir le type de relation (dans/avec) et les deux noms *)
let rec bl (s, v) = if s <> "0" then let cord = bl (Hashtbl.find t s).c in (fst cord + fst v, snd cord + snd v) else v (* j'ai oublié pourquoi ça s'appelle bl, mais calcule les coordonnées d'affichage d'une salle en fonction de la salle par rapport à laquelle elle et placée et d'un tuple décrivant comment elle est placée par rapport à cette dernière. la salle "0" est utilisée plusieurs fois à droite à gauche, c'est le nom pour dire "aucune salle". A un moment, mais plus maintenant, elle existait vraiment dans t. Elle dénote aussi, dans le cadre de placer, l'origine de coordonnées 0 0 *)
let toc s = (* to coordinates, pas tres utile mais bon, crée les salles.c *)
    let l = String.split_on_char ' ' s in
    (List.hd l, (int_of_string (List.nth l 1), int_of_string (List.nth l 2)))
let frc tup = (* from coordinates, fait l'inverse *)
	fst tup^" "^string_of_int (fst (snd tup))^" "^string_of_int (snd(snd tup))
let tob s = (* to behaviour, analyse une chaîne donnée et renvoie la fonction de déplacement d'une entite *)
	let l = String.split_on_char ' ' s in
	if List.hd l = "random"
	then (fun s -> let a = Array.of_list (Hashtbl.find t s).v in a.(Random.int (Array.length a)))
	else if List.hd l = "match"
	then (fun s -> let rec fi l = match l with | a::b::r -> if s = a || a = "_" then (if b = "random" then (let a = Array.of_list (Hashtbl.find t s).v in a.(Random.int (Array.length a))) else b) else fi r | _ -> assert false in fi (List.tl l))
	else (fun s -> s)
let ic = open_in "salles.txt"
let pixel = int_of_string (input_line ic) (* le nombre de début de pixels par cellule *)
let etage = int_of_string (input_line ic) (* Does Exactly What It Says On The Tin (DEWISOTT) *)
let cor = let s = input_line ic in let l = String.split_on_char ' ' s in (int_of_string (List.nth l 0), int_of_string (List.nth l 1)) (* les co du scroll *)
let pop s = let rec add l = match l with | (k, v)::ta -> if v.s = s then k::add ta else add ta| [] -> [] in add (List.of_seq (Hashtbl.to_seq p)) (* renvoie la liste des entites dans une salle *)
let col_of_int x = match x with | 0 -> (0, 0, 0) | 1 -> (255, 255, 255) | 2 -> (128, 128, 128) | 3 -> (255, 0, 0) | 4 -> (255, 255, 0) | _ -> (139, 69, 19) (* renvoie le triplet de r g b à partir du code couleur *)
let trgb x = let (r, g, b) = col_of_int x in rgb r g b (*transforme un code couleur en couleur. j'ai laissée rgb parce que parfois ça peut servir *)
let display_room s z scroll tt oc show = (* affiche la salle s de la table de hachage tt avec z la taille d'une cellule en pixel et scroll DEWISOTT. écrit pour dungeonshower dans le canal oc si show *)
	let r = Hashtbl.find tt s in
	let coord = bl r.c in
	let draw i j co = set_color (trgb co); let x = ((i+fst coord)*z+fst scroll) and y = ((j+snd coord)*z+snd scroll) in if x < 0 || y < 0 || x >= 1000 || y >= 1000 || co = 0 (* on ne dessine pas le noir parce que 1 on est sur fond noir et 2 ça pourrait recouvrir d'autres trucs. je l'ai fait à d'autres endroits aussi *)then () else fill_rect x y (z-1) (z-1) in 
	let order i j co = set_color (trgb co); let x = ((i+fst coord)*z+fst scroll) and y = ((j+snd coord)*z+snd scroll) in if x < 0 || y < 0 || x >= 1000 || y >= 1000 || co = 0 then () else output_string oc ((String.concat " " (List.map string_of_int ([x; y; z]@(if co = 3 then [128; 128; 128] else if co = 4 then [255; 255; 255] else (let (r, g, b) = col_of_int co in [r; g; b])))))^"\n") in
	let rec it i j =
		if i = Array.length r.g then () else
		if j = Array.length r.g.(i) then it (i+1) 0 else
		((draw i j r.g.(i).(j)); it i (j+1))
	in let rec it_ i j =
		if i = Array.length r.j then () else
		if j = Array.length r.j.(i) then it_ (i+1) 0 else
		(order i j r.j.(i).(j); it_ i (j+1))
	in it 0 0; (if show then it_ 0 0); (* la suite affiche le nom de la salle et les entites dedans *) set_color blue; moveto (fst coord *z+fst scroll) (snd coord *z+snd scroll); (let x = fst coord *z+fst scroll+6*String.length s and y = snd coord * z + snd scroll + 6 * String.length s in if x < 0 || y < 0 || x >= 1000 || x >= 1000 then () else let po = pop s in draw_string (if po = [] then s else s^"("^String.concat ", " po^")"))
let display z eta scroll tt = (* affiche toutes les salles de tt qui sont à l'étage eta. *)
	erase(); 
	let ic_ = open_in "ordres.txt" in
	let l = input_line ic_ in
	close_in ic_; 
	let oc = open_out "ordres.txt" in
	Hashtbl.iter (fun k v -> if List.mem eta v.e then display_room k z scroll tt oc (l<>"freeze")) tt;
	(if l = "freeze" then output_string oc "freeze\n");
	close_out oc
let crop mat = (* enlève des lignes / colonnes vides sur les bords d'une matrice . *)
	let i_ = let rec it i = if Array.for_all (fun x -> x = 0) mat.(i) then it (i+1) else i in it 0 (* le i min à garder *)
	and _i = let rec it i = if Array.for_all (fun x -> x = 0) mat.(i) then it (i-1) else i in it (Array.length mat - 1) (* le i max à garder *)
	and j_ = let rec it j = if List.for_all (fun x -> x = 0) (let rec b i = if i = Array.length mat then [] else mat.(i).(j)::b (i+1) in b 0) then it (j+1) else j in it 0 (* le j min à garder *)
	and _j = let rec it j = if List.for_all (fun x -> x = 0) (let rec b i = if i = Array.length mat then [] else mat.(i).(j)::b (i+1) in b 0) then it (j-1) else j in it (Array.length (mat.(0))-1) (* le j max à garder *) in
	let rec ch i j l = (* pour choose, prend les bons morceaux de mat, l la ligne partiellement construite *)
		if i = _i+1
		then [||]
		else
			if j = _j+1
			then Array.append [|l|] (ch (i+1) j_ [||])
			else ch i (j+1) (Array.append l [|mat.(i).(j)|])
	in ch i_ j_ [||]
let edit mat z = (*l'interface de modification manuelle de la grille d'une matrice. *)
	let rec wa c = (* pour wait, c le code couleur *)
		if button_down() then (let mi = fst(mouse_pos())/z and mj = snd(mouse_pos())/z in
		mat.(mi).(mj) <- c; set_color (trgb c); fill_rect (mi*z) (mj*z) (z-1) (z-1); wa c)
		else if key_pressed() then (let k = read_key() in if k = '0' || k = '1' || k = '2' || k = '3' || k = '4' || k = '5' then wa (int_of_char k - 48 ) else if k = 's' then crop mat else wa c)
		else wa c
	in tr ["0 pour des cases noires, 1 pour des blanches, 2 pour des grises, 3 pour des rouges, 4 pour des jaunes et 5 pour des marrons.";"Appuyez sur s pour sauver quand vous avez fini."]; wa 1
let modi g = (* appelle edit en rajoutant quelques lignes de marge autour de la matrice donnée en argument *)
	set_color (rgb 4 4 4); fill_rect 0 0 1000 1000; let si = max (Array.length g) (Array.length g.(0)) + 6 (* pour transformer les matrices rectangulaires en matrices carrées *)in let z = 1000 / si in
	let mat = 
		let rec fill i j l = (* ajoute les marges *)
			if i = Array.length g 
			then Array.append (if i < Array.length g.(0) then Array.make_matrix (Array.length g.(0) - i ) (Array.length g.(0)+6) 0 else [||]) (* si il y a trop de colonnes *) (Array.make_matrix 3 (Array.length g.(0)+6) 0) (* et les lignes de marge à la fin *)
			else
  				if j = Array.length g.(0) 
				then Array.append [|Array.append l (Array.append (if j < Array.length g then Array.make (Array.length g - j) 0 else [||]) (Array.make 3 0)) (* la marge à droite *)|] (fill (i+1) 0 (Array.make 3 0) (* et la marge à gauche *)) 
				else fill i (j+1) (Array.append l [|g.(i).(j)|])
		in Array.append (Array.make_matrix 3 (Array.length g.(0)+6) 0) (* et les lignes de marges au début *) (fill 0 0 (Array.make 3 0)) in
	let rec show i j = (* il faut rafficher ce qu'on a déjà de la matrice sinon l'utilisateur ne verra que les changements *)
		if i = Array.length mat then () else
		if j = Array.length mat.(i) then show (i+1) 0 else
		(set_color (trgb mat.(i).(j)); (if mat.(i).(j) = 0 then () else fill_rect (i*z) (j*z) (z-1) (z-1)); show i (j+1))
	in show 0 0; edit mat z
let create() = (* appelle edit avec une grille vide *)
	set_color black; fill_rect 0 0 1000 1000; tr ["Entrez la taille maximale."]; let ma = int_of_string(inp "") in let z = 1000/ma in
	let mat = Array.make_matrix ma ma 0 in edit mat z
let select_room z eta scroll tt = (* permet de sélectionner une salle en cliquant dessus, renvoie le nom *)
	let rec wa eta scroll ch = (* pour wait, ch sert à dire si on a fait quelque chose qui nécessite de redessiner (ex. scroller) *)
		if ch then display z eta scroll tt;
		if button_down() then (let mi = (fst(mouse_pos())-fst scroll)/z and mj = (snd(mouse_pos())-snd scroll)/z in
			let rec find l = 
				match l with
				| [] -> "0"
				| i::s -> let r = Hashtbl.find tt i in let cord = bl r.c in if List.mem eta r.e && fst cord <= mi && snd cord <= mj && fst cord + Array.length r.g >= mi && snd cord + Array.length r.g.(0) >= mj then i else find s
			in (tr [""]; find (List.of_seq (Hashtbl.to_seq_keys tt)))) (* on cherche la salle qui est affichée à ces coordonnées-là et "0" pour aucune *)
		else (if key_pressed () (* pour permettre de naviguer dans le mode de sélection *)
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
let rec placer na r z eta scroll = (* place la salle de nom na et d'attributs r *)
	let t_  = Hashtbl.create 50 in
	let save () = (* t_ et ceci pour pouvoir afficher apres avoir enleve temporairement une salle. en effet, l'affichage des salles est souvent dépendant de l'existence d'une certaine salle. je precalcule donc les coordonnées de toutes les salles pour pouvoir les afficher même après en avoir enlevé une *)
		let rec b l = (* build je crois *)
			match l with
			| [] -> ()
			| (k, v)::s -> (if Hashtbl.mem t_ k then Hashtbl.replace t_ k {v with c = ("0", bl v.c)} else Hashtbl.add t_ k {v with c = ("0", bl v.c)}; b s) in 
		b (List.of_seq (Hashtbl.to_seq t)) in
	save(); 
	Hashtbl.remove t na; (* ne fait rien si la salle n'existe pas, mis pour compatibilité avec à la fois + et m/s/r *)
	display z eta scroll t_;
	let sa = select_room z eta scroll t_ in
	tr ["Placer en fonction de "^sa^"."; "Cliquez la où vous voulez mettre le coin en bas a gauche."]; Unix.sleepf 0.2;
	let rec wa() =
		if button_down() then
		(let mi = (fst(mouse_pos())-fst scroll)/z-fst (if sa = "0" then (0, 0) else bl (Hashtbl.find t_ sa).c) and mj = (snd(mouse_pos())-snd scroll)/z-snd(if sa = "0" then (0, 0) else bl (Hashtbl.find t_ sa).c) in
		Hashtbl.add t na {r with c = (sa, (mi, mj))}; save(); display z eta scroll t_; 
		tr ["Entree pour valider, delete pour rechoisir une salle et autre chose pour refaire."];
		let m = read_key() in if int_of_char m = 13 then tr [""] else if int_of_char m = 8 then placer na r z eta scroll else(Hashtbl.remove t na; display z eta scroll t_; wa()))
		else wa()
	in wa()
let rec di z eta scroll= (* di for do it, does main loop *)
	display z eta scroll t;
	match read_key() with
	| 'z' -> di z eta (fst scroll, snd scroll+10)
	| 'q' -> di z eta (fst scroll-10, snd scroll)
	| 's' -> di z eta (fst scroll, snd scroll-10)
	| 'd' -> di z eta (fst scroll+10, snd scroll)
	| '<' -> di z (eta+1) scroll
	| '>' -> di z (eta-1) scroll
	| 'l' -> tr["s pour salles, e pour entites ou a pour alarmes."]; (match read_key() with 
		| 's' -> tr("Liste des salles: "::List.map (fun k -> " - "^k) (List.of_seq (Hashtbl.to_seq_keys t)))
		| 'e' -> tr ("Liste des entites: "::List.map (fun k -> " - "^k) (List.of_seq (Hashtbl.to_seq_keys p)))
		| 'a' -> tr ("Liste des alarmes: "::List.map (fun (k, v) -> " - "^k^" quand "^List.nth v 1^" est "^List.nth v 0^" "^List.nth v 2^".") (List.of_seq (Hashtbl.to_seq a)))
		| _ -> tr["s pour salles, e pour entites ou a pour alarmes."]); 
		di z eta scroll 
	| '+' -> tr ["s pour salles, e pour entites ou a pour alarmes."]; (match read_key() with
		| 's' -> 
			(let na = tr ["Entrer nom."]; inp "" in
			tr ["Etage (s) ?"]; let et = List.map int_of_string (String.split_on_char ' ' (inp "")) in
			let gr = create() in
			display z eta scroll t;
			tr ["Description ?"]; let de = inp "" in
			tr ["Salles voisines ?"]; let vo = let rec wa() = let r = select_room z eta scroll t in if r = "0" then [] else r::wa() in wa() in
			placer na {e=et; g=gr; d=de; c=("0", (0, 0)); v=vo; j=[||]} z eta scroll);
		| 'e' -> 
			(let en = tr ["Entrer nom."]; inp "" in
			let sa = select_room z eta scroll t in
			let be = tr ["Comportement ?"]; inp "" in
			let de = tr ["Description ?"]; inp "" in
			if Hashtbl.mem t sa then (let b_ = tob be in Hashtbl.add p en {s=sa; b=b_; t=be; d=de}) else tr ["Salle non existante."])
		| 'a' ->
			(let s = tr ["Entrer l'identifiant, le type d'alarme et les deux noms separes par des espaces."]; String.split_on_char ' ' (inp "") in
			Hashtbl.add a (List.hd s) (List.tl s));
			di z eta scroll
		| _ -> tr ["s pous salles, e pour entites ou a pour alarmes."]);
		di z eta scroll
	| '-' -> tr ["s pour salles, e pour entites ou a pour alarmes."]; (match read_key() with 
		| 's' -> 
			(let na = select_room z eta scroll t in 
			if Hashtbl.mem t na 
			then (let co = bl (Hashtbl.find t na).c in 
				let del() = (Hashtbl.remove t na; Hashtbl.iter (fun k v -> Hashtbl.replace t k {v with c = if fst v.c <> na then v.c else ("0", (fst (snd v.c)+fst co, snd (snd v.c)+snd co))}) t; Hashtbl.iter (fun k v -> if List.mem na v then Hashtbl.remove a k else ()) a) in
				(if pop na = [] then del() else (tr ["Il y a des entites dans cette salles."; "Voulez-vous vraiment la supprimer ? [y/N]"]; if read_key() = 'y' then del() else tr ["Suppression annulee."])))
			else tr ["Salle non existante."])
		| 'e' -> 
			(let en = tr ["Entrer nom de l'entite."]; inp "" in 
			if Hashtbl.mem p en 
			then Hashtbl.remove p en 
			else tr ["Entite non existante."])
		| 'a' ->
			(let an = tr ["Entrer nom."]; inp "" in if Hashtbl.mem a an then Hashtbl.remove a an else tr ["Alarme non existante."])
		| _ -> tr ["s pour salles, e pour entites ou a pour alarmes."]); 
		di z eta scroll
	| 't' -> tr ["Entrer taille."]; di (int_of_string(inp "")) eta scroll
	| 'c' -> tr ["Les commandes disponibles sont (une commande suivie d'un * signifie que il faut choisir quel type): ";" - l* : donne les listes de salles/entites/alarmes";" - +* : cree une nouvelle salle/entite/alarme";" - -* : supprime une salle/entite/alarme";" - t : redefinit la taille d'une case en pixels";" - c : affiche ceci";" - i* : donne des informations sur une salle/entite (pour les alarmes faire l)";" - m* : modifie une salle/entite/alarme.";" - b : fait bouger toutes les entites selon leur comportement defini et affiche les alarmes le cas echeant.";" - a : va a une salle."; " - v : actualise comment les joueurs voient une salle dans dungeonshower.";"- h : cache une salle dans dungeonshower.";"- g : gèle l'affichage dans dungeonshower." ; " - = : sauvegarde."; " - f : ferme tout sans sauvegarder."; " - z, q, s, ou d pour defiler respectivement vers le haut, la gauche, le bas, et la droite.";" - < pour aller a l'etage en dessous (donc avec le numero plus grand)";" - > pour remonter un etage."]; di z eta scroll
	| 'i' -> tr ["s pour salles ou e pour entites."]; (match read_key() with
		| 's' -> (let na = select_room z eta scroll t in 
			tr (if Hashtbl.mem t na then 
				(let r = Hashtbl.find t na in 
					[na; ""; r.d; ""]@(if r.v <> [] then "Liee a :"::
					List.map (fun x -> " - "^x) r.v else [])@
					(let po = pop na in if po = [] then [] else [""; "Contient les entites : "]@
					(List.map (fun x -> " - "^x) po))@
					["Visible aux joueurs : "^if r.j <> [||] then "oui" else "non"]) 
			else ["Salle non existante."]))
		| 'e' -> 
			(let en = tr ["Entrer nom."]; inp "" in
			tr (if Hashtbl.mem p en then
				(let v = Hashtbl.find p en in
					[en; ""; v.d; ""; "Dans "^v.s; ""; "Comportement:"; v.t])
			else ["Entite non existante."]))
		| _ -> tr ["s pour salles ou e pour entites."]); 
		di z eta scroll
	| 'm' -> tr ["s pour salles, e pour entites ou a pour alarmes."]; (match read_key() with
		| 's' -> 
			(let na = select_room z eta scroll t in 
			if Hashtbl.mem t na then 
				(tr ["Modification de la salle "^na^"."; "Faites n pour changer le nom, g pour modifier la grille, d pour changer la description, r pour la replacer et v pour changer les voisins."]; 
				let r = Hashtbl.find t na in 
				(match read_key() with
				| 'n' -> tr ["Entrer le nouveau nom."]; let nn = inp "" in Hashtbl.remove t na; Hashtbl.add t nn r; Hashtbl.iter (fun k v -> Hashtbl.replace t k {v with c = if fst v.c = na then (nn, snd v.c) else v.c; v = List.map (fun x -> if x = na then nn else x) v.v}) t
				| 'g' -> Hashtbl.replace t na {r with g = modi r.g}
				| 'd' -> tr ["Entrer la description."]; Hashtbl.replace t na {r with d = inp ""}
				| 'r' -> placer na r z eta scroll
				| 'v' -> tr ["Entrer les voisins separes par des espaces."]; Hashtbl.replace t na {r with v = let rec rem l = match l with [] -> [] | i::s -> if Hashtbl.mem t i then i::rem s else rem s in rem (String.split_on_char ' ' (inp ""))}
				| _ -> tr ["n/g/d/r/v."]))
			else tr ["Salle non existante."])
		| 'e' -> 
			(let en = tr["Entrer nom."]; inp "" in
			if Hashtbl.mem p en then 
				(tr ["Modification de l'entite "^en^"."; "Faites n pour changer le nom, r pour replacer, d pour changer la description, et c pour changer le comportement."];
				let a = Hashtbl.find p en in
				(match read_key() with
				| 'n' -> tr ["Entrer le nouveau nom."]; let nn = inp "" in Hashtbl.remove p en; Hashtbl.add p nn a
				| 'r' -> let sa = select_room z eta scroll t in Hashtbl.replace p en {a with s=sa}
				| 'd' -> let de = tr ["Entrer la description."]; inp "" in Hashtbl.replace p en {a with d=de}
				| 'c' -> let be = tr ["Entrer le comportement."]; inp "" in let b_ = tob be in Hashtbl.replace p en {a with t=be; b=b_}
				| _ -> tr ["n/r/d/c."]))
			else tr ["Entite non existante."])
		| 'a' ->
			(let an = tr["Entrer nom"]; inp "" in 
			if Hashtbl.mem a an then (let s = tr ["Entrer le type d'alarme et les deux noms separes pas des espaces."]; inp "" in Hashtbl.replace a an (String.split_on_char ' ' s)) else tr ["Alarme non existante."])
		| _ -> tr ["s pour salles, e pour entites et a pour alarmes."]); 
		di z eta scroll
	| '=' -> ignore (Unix.system ("cp salles.txt salles/salles"^string_of_float (Unix.time())^".txt")); 
		let oc = open_out "salles.txt" in 
		output_string oc (string_of_int z^"\n"^string_of_int eta^"\n"^string_of_int (fst scroll)^" "^string_of_int(snd scroll)^"\n"); 
		Hashtbl.iter (fun k v -> output_string oc ("-"^k^"\n"^String.concat " " v^"\n")) a;
		Hashtbl.iter (fun k v -> output_string oc ("'"^k^"\n"^v.s^"\n"^v.t^"\n"^v.d^"\n")) p;
		Hashtbl.iter (fun k v -> output_string oc ("_"^k^"\n"^String.concat " " (List.map string_of_int v.e)^"\n"^frg v.g^"\n"^v.d^"\n"^frc v.c^"\n"^String.concat " " v.v^"\n"^frg v.j^"\n")) t; close_out oc; di z eta scroll
	| 'f' -> ()
	| 'b' -> (Hashtbl.iter (fun k v -> Hashtbl.replace p k {v with s = v.b v.s}) p; 
		let rec check l = match l with 
			| (k, ["avec"; a; b])::s -> if (Hashtbl.find p a).s = (Hashtbl.find p b).s then ("Alarme: l'entite "^a^" a rencontree l'entite "^b^"!")::check s else check s
			| (k, ["dans"; a; b])::s -> if (Hashtbl.find p a).s = b then ("Alarme: l'entite "^a^" est dans la salle "^b^"!")::check s else check s
			| _ -> []
		in tr(check (List.of_seq (Hashtbl.to_seq a)))); di z eta scroll
	| 'a' -> (let sa = tr ["Entrer nom."]; inp "" in let r = Hashtbl.find t sa in di z (List.hd r.e) (let co = bl r.c in (-(fst co)*z, -(snd co)*z)))
	| 'v' -> let sa = select_room z eta scroll t in let r = Hashtbl.find t sa in Hashtbl.replace t sa {r with j = r.g}; di z eta scroll
	| 'h' -> let sa = select_room z eta scroll t in let r = Hashtbl.find t sa in Hashtbl.replace t sa {r with j = [||]}; di z eta scroll
	| 'g' -> let ic_ = open_in "ordres.txt" in let l = input_line ic_ in close_in ic_; let oc = open_out "ordres.txt" in output_string oc (if l = "freeze" then "nothing to see here\n" else "freeze\n"); close_out oc; di z eta scroll
	| _ -> tr ["Commande inconnue. Faites c pour en avoir la liste."]; di z eta scroll
let () =
	open_graph ""; 
	resize_window 1500 1000; 
	set_window_title "Dungeon Builder";
	erase(); 
	erase_console(); 
	let rec it() = 
		try let nom = input_line ic in
		(if nom.[0] = '_' then
		(let etage = input_line ic and grille = input_line ic and desc = input_line ic and co = input_line ic and voi = input_line ic and jou = input_line ic in
		Hashtbl.add t 
		(String.sub nom 1 (String.length nom - 1)) 
		{e=List.map int_of_string (String.split_on_char ' ' etage); 
		g=tog grille;
		d=desc;
		c=toc co;
		v=String.split_on_char ' ' voi;
		j=tog jou})
		else if nom.[0] = '\'' then
		(let sall = input_line ic and behaviour = input_line ic and desc = input_line ic in
		Hashtbl.add p
		(String.sub nom 1 (String.length nom - 1))
		{s=sall;
		b=tob behaviour;
		t=behaviour;
		d=desc})
		else
		(let l = String.split_on_char ' ' (input_line ic) in Hashtbl.add a (String.sub nom 1 (String.length nom - 1)) l));
		it ()
		with End_of_file -> close_in ic
	in it ();
	di pixel etage cor
