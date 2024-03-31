// TODO:
//		reoptimiser l'histoire, pistes:
//			histoire des changements
//			avec positions relatives
//			et valeurs par defaut
//		compresser encore plus les grilles, pas sur que ca soit possible
// initialisation de variables #ini (les balises en # permettent de venir avec une recherche)
const canvas = document.querySelector("canvas");
const ctx = canvas.getContext("2d");
kill = false; // si le CA devrait etre arrete
fileurl = null;
groupes = [canvas, Options, Menu, Info, Settings, Save, Upload, Create]; // (groupes d')elements pour l'affichage
CAs = ["gol", "bosco", "daynight", "marine", "sparks", "eiii"]; // liste des CA disponibles
pauseonlybuttons = [abtn, plbtn, mobtn, rbtn, mbtn, nbtn, ebtn, sbtn, cbtn];
key_funcs = []; // cles : codes, valeurs : fonctions
url = new URL (window.location.href);
stateid = (new URLSearchParams(url.search)).get("id");
black = "0 0 0";
red = "255 0 0";
green = "0 255 0";
blue = "0 0 255";
white = "255 255 255"; // quelques couleurs predefinies
paused = true;
filecontent = "";
toadd = "";
ntable = []; // un tableau de carrés gris avec celui du mileu en blanc, en cliquant dessus ils deviennent noirs et on les ajoute au voisinage
for (let i = 0; i <= 10; i++) {
	toadd += "<tr>";
	nl = [];
	for (let j = 0; j <= 10; j++) {
		if (i != 5 || j != 5) {
			toadd += '<td style="background-color:#AAAAAA;width:20px;height:20px" id="n'+i.toString()+"I"+j.toString()+'t" onclick="flip('+i.toString()+","+j.toString()+')"><span style="visibility:hidden">&nbsp;</span></td>';
			nl[j] = false;
		} else {
			toadd += '<td style="background-color:white;width:20px;height:20px" id="n5I5t" onclick="flip(5, 5)"><span style="visibility:hidden">&nbsp;<span></td>';
			nl[j] = false;
		}
	}
	ntable[i] = dcopy(nl);
	toadd += "</tr>"
}
neighborhood.innerHTML += toadd;
// quelques fonctions de base #basics
function inarray(l, x) {
	for (let i = 0; i < l.length; i++) {
		if (l[i] == x) { return true }
	}
	return false;
}
function hextorgb(hex) { return [hex.slice(1, 3), hex.slice(3, 5), hex.slice(5, 7)].map((x) => Number("0x"+x)).join(" ") } // "#HHHHHH" en "DDD DDD DDD"
function tob64(n) { // en base 64, cad 0 .. 9 + A .. Z + a .. z + _ + . (Assume, dans son implementation actuelle, que il y a moins de 2**12 couleurs.)
	if (n < 10) {
		return n.toString() // chiffres
	} else if (n < 36) {
		return String.fromCharCode(n-10+65) // A-Z
	} else if (n < 62) {
		return String.fromCharCode(n-36+97) // a-z
	} else if (n == 62) {
		return "-"
	} else if (n == 63) {
		return "."
	} else if (n > 63) { // un nombre a max deux chiffres (le 2**12), donc on met ' pour dire un nombre a deux chiffres
		return ("'"+tob64(Math.floor(n / 64)) + tob64(n % 64))
	}
}
function fromb64(s) { // ne prend que les chiffres, decompress pretraite les '
	if (s >= "0" && s <= "9") {
		return Number(s)
	} else if (s >= "A" && s <= "Z") {
		return (s.charCodeAt()-65+10)
	} else if (s >= "a" && s <= "z") {
		return (s.charCodeAt()-97+36)
	} else if (s == "-") {
		return 62
	} else {
		return 63
	}
}
function moore(r) { // voisinage de moore de rayon r
	res = [];
	for (let i = -r; i <= r; i++) {
		for (let j = -r; j <= r; j++) {
			if (i != 0 || j != 0) { res.push([i, j]) }
		}
	}
	return res
}
function randint(n) { return Math.floor(Math.random()*n) }
function equ(a) { return (() =>a[randint(a.length)]) } // renvoie une fonction qui choisit un element de a avec equiprobabilite
function pondered(caps) { // renvoie une fonction, qui prend un tableau de couleurs et renvoie une fonction qui en choisit une au hasard avec une ponderation definie dans caps.
	return (a) => {
		return () => {
			r = Math.random();
			for (let i = 0; i < a.length; i++) {
				if (r < caps[i]) {
					return a[i]
				}
			}
		}
	}
}
function countof(x, a) { // nombre de fois x dans a
	count = 0; 
	for (let i = 0; i < a.length; i++) {
		if (a[i] == x) {
			count += 1
		}
	}
	return count
}
function dcopy(aa) { return JSON.parse(JSON.stringify(aa)); }
function sleep(ms) { return new Promise(resolve => setTimeout(resolve, ms)); } // renvoie une promesse qui attend ms milisecondes avant de se résoudre
function draw (x, y, c) { // dessine un carre de couleur c en x, y (z le cote en pixels d'une cellule)
	ctx.fillStyle = "rgb("+ c + ")";
	ctx.fillRect((x*z), (y*z), z, z);
}
function w(k) { // w(rap) (k parce que i, j, k, puisque c'est en i ou en j)
	if (k >= si) {
		return k - si;
	} else if (k < 0) {
		return si + k;
	} else {
		return k;
	}
}
function b() { // b(uild), construit un tableau de tableaux de si*si rempli avec les resultats de e(col)
	element = e(col); // fun fact: le e vient de element
	res = [];
	for (let i = 0; i<si; i++) {
		ligne = [];
		for (let j = 0; j<si; j++) {
			ligne[j] = element();
		}
		res[i] = ligne;
	}
	return res;
}
function bind(f, b, c) { // attache au bouton b et a la touche c la fonction f
	b.onclick = f;
	if (c) {
		key_funcs[c.charCodeAt(0)] = (() => {if (b.style.display != "none" && Options.style.display != "none") {f.call()} })
	}
}
function keyListener(event) { // ce qu'on fait quand une touche est pressee
	key = event.keyCode;
	if (key in key_funcs) {
		key_funcs[key].call()
	}
}
function rgbtotd (s) { // "DDD DDD DDD" en un td (case de tableau) avec pour font la couleurcorrespondante
	return ('<td style="background-color:rgb('+s+')">&nbsp;</td>')
}
function bs (bi, su) { // interprete la notation classique B/S, pour quand il y a deux types de vie. voir rulestring sur lifewiki
	bi = bi.split(",");
	su = su.split(",");
	bi = bi.map((e) => { if (e.indexOf("-") > -1) { return e.split("-").map(Number) } else { return Number(e) } } )
	su = su.map((e) => { if (e.indexOf("-") > -1) { return e.split("-").map(Number) } else { return Number(e) } } )
	return ( (a, v) => {
		count = countof(white, a);
		if (v == black) {
			for (let j = 0; j < bi.length; j++) {
				if (typeof(bi[j]) == "number") {
					if (count == bi[j]) {
						return white
					}
				} else {
					if (count >= bi[j][0] && count <= bi[j][1]) {
						return white
					}
				}
			}
			return black
		} else {
			for (let j = 0; j < su.length; j++) {
				if (typeof(su[j]) == "number") {	
					if (count == su[j]) {
						return white
					}
				} else {
					if (count >= su[j][0] && count <= su[j][1]) {
						return white
					}
				}
			}
			return black
		}
	} )
}
// pour montrer et cacher des trucs #display
function hide_(e) { e.style.display = "none" }
function hide(a) { for (let i = 0; i < a.length; i++) { hide_(a[i]) } }
function show_(e) { e.style.display = ""  }
function show(a) { for (let i = 0; i < a.length; i++) { show_(a[i]) } }
function only(a) {
	hide(groupes);
	show(a);
}
function choose() { // efface canvas (au cas ou) et passe au menu de selection
	ctx.fillStyle = "rgb(0 0 0)";
	ctx.fillRect(0, 0, 1000, 1000);
	move([Menu]);
	CA.value = "none";
}
function move(x) { whereami = x; only(x) }
// modifications #edit
bind(() => { 
	g_ = b(); 
	for (let i = 0; i < si; i++) {
		for (let j = 0; j < si; j++) {
				draw(i, j, g_[i][j])
		}
	}
	h = []
	},
	nbtn, "N"); // nouveau
bind(() => {
	hide(pauseonlybuttons);
	show_(qbtn);
	canvas.onclick = (e) => {
		if (qbtn.style.display != "none") {
			rect = e.target.getBoundingClientRect();
			mx = Math.floor((e.clientX - rect.left)/z);
			my = Math.floor((e.clientY - rect.top)/z);
			old_c = g_[mx][my];
			g_[mx][my] = col[(coltoi[old_c]+1)%col.length];
			draw(mx, my, g_[mx][my]) 
		}
	}
	}, 
	mbtn, "M"); // modifier
bind(() => {
	for (let i = 0; i < si; i++) {
		for (let j = 0; j < si; j++) {
				g_[i][j] = "0 0 0";
		}
	}
	ctx.fillStyle = "rgb(0 0 0)";
	ctx.fillRect(0, 0, 1000, 1000);
	},
	ebtn, "E") // effacer
// autres boutons et elements html #other
bind(() => { hide_(qbtn); show(pauseonlybuttons) }, qbtn, "Q"); // quitter (modifier)
bind((() => { kill = true; choose() }), xbtn, "X"); // fermer
bind(() => only([Info]), ibtn, "I"); // informations
settingsvalid.onclick = () => {
	if ( 1000 % size.value == 0 ) { only (whereami) } 
	else { alert("Entree invalide. La taille doit etre un diviseur de 1000.") }
}
window.addEventListener("keydown", keyListener);
// creation de CAs #create
addcolor.onclick = () =>  { // ajoute un etat a col
	if (!inarray(col, hextorgb(colorpicker.value))) {
		col.push(hextorgb(colorpicker.value)); 
		actualisetable()
	}
}
rmstate.onclick = () => { // retire un etat de col
	i = Number(statetorm.value)
	if (0 <= i && i < col.length) {
		col.splice(i, 1)
	}
	actualisetable()
}
addcase.onclick = () => { // affiche le menu de creation de cas
	show_(caseincreation);
	hide_(addcase);
	result.innerHTML = "";
	for (let i = 0; i < col.length; i++) {
		result.innerHTML += '<option value="'+i.toString()+'">'+i.toString()+'</option>'; // liste des etats
	}
	currentstate.innerHTML = result.innerHTML;
}
casevalid.onclick = () => { // tente d'ajouter un cas a la regle
	ok = true;
	for (let i = 0; i < condition.value.length; i++) {
		if (!inarray([" ", "(", ")", "<", ">", "=", "&", "|", "c", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "!", "t", "r", "u", "e"], condition.value.charAt(i))) {
			ok = false;
			alert("Condition invalide");
			break
		}
	};
	if (ok) {
		if (nocase) {
			nocase = false;
			show_(cases);
		} else {
			f_ += "else"
		}
		f_ += " if ( v == col[" + currentstate.value + "] && (" + condition.value + ") ) { return col[" + result.value + "] } ";
		cases.innerHTML += '<tr>'+rgbtotd(col[Number(currentstate.value)])+'<td>'+condition.value+'</td><td>&emsp;→&emsp;</td>'+rgbtotd(col[Number(result.value)])+"</tr>"
		hide_(caseincreation);
		show_(addcase);
	}
}
createvalid.onclick = () => { // pour lancer avec le n, col, et f construits
	if (bsrule.style.display == "none") {
		for (let i = 0; i < col.length; i++) {
			f_ = " c"+i.toString() +" = countof('"+col[i]+"', a);" + f_
		};
		f_ = "(a, v) => { " + f_ + "}";
	} else {
		f_ = 'bs("'+birthcounts.value+'", "'+survivalcounts.value+'")';
	}
	go(f_+"."+JSON.stringify(n)+"."+JSON.stringify(col)+"_"+size.value+"_"+delay.value+"_"+wrap.checked+"_rand")
}
tobs.onclick = () => {hide_(manualrule); show_(bsrule)};
frombs.onclick = () => {hide_(bsrule); show_(manualrule)};
function create() { // aller au menu de creation
	move([Create]);
	col = [black];
	actualisetable();
	n = [];
	condition.value = "";
	hide([caseincreation, cases, bsrule]);
	show_(manualrule)
	f_ = "";
	nocase = true;
}
function actualisetable() { // actualise le tableau de col
	colorlist.innerHTML = '<thead><tr><td scope="col">État</td><td scope="col">Couleur</td><td scope="col">RGB</td></tr></thead>';
	for (let i = 0; i < col.length; i++) {
		colorlist.innerHTML = colorlist.innerHTML + "<tr>"+"<td>"+i.toString() + '</td>' + rgbtotd(col[i]) + '<td><code>'+col[i]+'</code></td></tr>';
	}
}
function flip(i, j) { // pour n, met une case blanche/grise en noir et vice-versa
	nv = !(ntable[Number(i)][Number(j)]);
	ntable[Number(i)][Number(j)] = nv;
	idshouldbe = "n" + i.toString() + "I" + j.toString()+"t"
	eval(idshouldbe+".style.backgroundColor = (nv) ? '#000000' : ((i == 5 && j == 5) ? '#FFFFFF' : '#AAAAAA')");
	if (nv) {
		n.push([i-5, j-5])
	} else {
		n.splice(n.indexOf([i-5, j-5]), 1)
	}
}
// sauvegarde #save
bind((() => { // sauvegarder
	text = totext();
	file = new Blob([text]);
	if (fileurl) {
		window.URL.revokeObjectURL(fileurl)
	}
	fileurl = window.URL.createObjectURL(file);
	Download.href = fileurl;
	Download.download = "AC-"+ Math.floor(Date.now()/1000).toString() + ".txt";
	Download.innerHTML = "Telecharger "+ Download.download;
	stateidout.innerHTML = totext();
	hide_(copied);
	show_(stateidout);
	only([Save]);
	}), 
	sbtn, "S")
stateidout.onclick = () => { // le bouton "copier l'ID"
	navigator.clipboard.writeText(totext());
	hide_(stateidout);
	show_(copied);
}
function totext() { // renvoie une chaine decrivant etape actuelle, parametres et CA
	return (
	running + "_" + 
	si.toString() + "_" + 
	delay.value.toString() + "_" + 
	wrap.checked + "_" + 
	compress(g_))
}
function compress (aa) { // tableau de tableau -> chaine
	res = "";
	lt = -1;
	ti = 0;
	for (let i = 0; i < si; i++) {
		if (i > 0 && JSON.stringify(aa[i]) == JSON.stringify(aa[i-1])) {
			if (lt == "$") {
				ti += 1;
			} else {
				lt = "$";
				ti = 1
			}
		} else {
			if (lt == "$") {
				res += "$" + tob64(ti);
			}
			lt = -1;
			ti = 0;
			for (let j = 0; j < si; j++) {
				k = coltoi[aa[i][j]];
				if (k != lt) {
					if (lt != -1) {
						res += tob64(lt) + tob64(ti);
					}
					lt = k;
					ti = 1;
				} else {
					ti += 1
				}
			}
		}
		if (lt != "$") {
			res += tob64(lt) + tob64(ti);
		}
	}
	if (lt == "$") {
		res += lt + tob64(ti);
	}
	return res
}
// chargement d'états #load
bind((() => {only([Upload])}), cbtn, "C") // charger
function waitforfile() { // pour ne pas que load s'execute avant que filecontent existe
	if (filecontent == "") {
		setTimeout(waitforfile, 100) 
	} else {
		go(filecontent)
	}
}
loadvalid.onclick = () => { // charger depuis textarea ou file input
	if (stateidin.value == "") {
		files = filename.files;
		file = files[0];
		reader = new FileReader();
		reader.onload = ((a) => {filecontent = a.target.result})
		reader.readAsText(file);
		waitforfile()
	} else {
		go(stateidin.value)
	}
}
function decompress (s) { // chaine -> tableau de tableau (a besoin des parametres)
	res = [];
	l = s.split("");
	l1 = [];
	i = 0;
	while (i < l.length) {
		if (l[i] == "'") {
			l1.push(fromb64(l[i+1])*64+fromb64(l[i+2]));
			i += 3
		} if (l[i] == "$") {
			l1.push("$");
			l1.push(fromb64(l[i+1]));
			i += 2
		} else {
			l1.push(fromb64(l[i]));
			i ++
		}
	}
	l2 = [];
	c = 0;
	i = 0;
	while (i < l1.length) {
		if (l1[i] == "$") {
			for (let j = 0; j < l1[i+1]; j++) {
				res.push(dcopy(res.slice(-1)[0]));
			}
		} else {
			for (let j = 0; j < l1[i+1]; j++) {
				l2.push(col[l1[i]]);
				c += 1;
			}
		}
		i += 2;
		if (c == si) {
			c = 0;
			res.push(dcopy(l2));
			l2 = [];
		}
	}
	return res
}
// faire vraiment tourner le simulateur #run
CAvalid.addEventListener("click", (() => {
	switch (CA.value) { 
		case "none" : 
			break
		case "import" : 
			only([Upload]);
			break
		case "create" : 
			create();
			break
		default : 
			go()
	} 
}));
function spacebar () { // pour faire a la fois play et pause
	if (pbtn.style.display != "none") {
		paused = true
	} else {
		restart(d, false)
	}
}
key_funcs[" ".charCodeAt(0)] = spacebar; // cas special, pas lie a un bouton.
bind(() => restart(true, false), abtn, String.fromCharCode(39)); // fleche de droite
bind(() => restart(true, true), plbtn, String.fromCharCode(38)); // d'en haut
bind(() => restart(false, false), rbtn, String.fromCharCode(37)); // de gauche
bind(() => restart(false, true), mobtn, String.fromCharCode(40)); // d'en bas
function restart(vd, vo) {
	d = vd;
	o = vo;
	paused = false;
	if (!o) { // pas la peine de cacher les boutons si c'est pour les rafficher juste apres
		hide(pauseonlybuttons);
		show_(pbtn);
	} else if (!d && h.length == 0) {
		hide([mobtn, rbtn]);
	}
	di()
}
bind((() => paused = true ), pbtn); // pause
function di() { // Do It, fait l'iteration principale 
	if (!kill) {
		if (!paused) {
			if (!d) {
				if (h.length > 0) {
					g = dcopy(g_);
					g_ = decompress(h.pop(), si);
					for (let i = 0; i < si; i++) {
						for (let j = 0; j < si; j++) {
							if (g[i][j] != g_[i][j]) {
								draw(i, j, g_[i][j])
							}
						}
					}
				} else { // pas d'histoire
					paused = true;
				}
			} else {
				g = dcopy(g_);
				for (let i = 0; i < si; i++) {
					for (let j = 0; j < si; j++) { 
						co = f(n.map((a) => { // nouvelle couleur
							i_ = i+a[0];
							j_ = j+a[1];
							if (i_ < 0 || i_ >= si || j_ < 0 || j_ >= si) {
								if (wrap.checked === true) { // si il faut prendre de l'autre cote
									return g[w(i_)][w(j_)]
								} else { // si il faut compter une bordure de cases vides
									console.log(wrap.checked);
									return black
								}
							} else {
								return g[i_][j_]
							}
							}), g[i][j]);
						changed = (co != g[i][j]);
						if (changed) {
							g_[i][j] = co;
							draw(i, j, co);
						}
					}
				}
				h.push(compress(g));
			}
			if (o) { 
				o = false;
				paused = true;
			}
			sleep(delay.value*1000).then(() => {di()}) // meme si delai nul, remedie a la synchronalite de javascript, cad l'empeche d'essayer de tout calculer avant d'afficher, ce qui est problematique avec la boucle infinie intentionelle
		} else {
			hide_(pbtn);
			show(pauseonlybuttons);
			if (h.length == 0) {
				hide([rbtn, mobtn]);
			}
		}
	}
}
function getpars(na) { // donne les parametres correspondant au nom na
	switch (na) {
		case "gol" :
			return [
			bs("3", "2,3"),
			moore(1),
			[black, white],
			equ]
		case "bosco" :
			return [
			bs("34-45", "33-57"),
			moore(5),
			[black, white],
			equ]
		case "daynight" :
			return [
			bs("3,6-8", "3-4,6-8"),
			moore(1),
			[black, white],
			equ]
		case "marine" :
			return [
			bs("6-8", "4,6-9"),
			moore(1).concat([[-2, 1], [-2, 0], [-2, -1], [-2, -2], [-1, -2], [0, -2], [1, -2]]),
			[black, white],
			equ]
		case "sparks"  :
			return [( (a, v) => {
				orth = a.slice(0, 4);
				diag = a.slice(4, 8);
				switch (v) {
					case black:
						if (inarray(orth,green) && (!(inarray(diag,red)) || (inarray(diag,blue) && inarray(orth,blue)))) { 
							return green
						} else {
							return black
						}
					case red:
						return black
					case green:
						return red
					case blue:
						return blue
				}
			} ),
			[[0, 1],[0,-1],[1,0],[-1,0],[-1,-1],[-1,1],[1,-1],[1,1]],
			[black, red, green, blue],
			pondered([0.45, 0.65, 0.85, 1])]
		case "eiii" :
			return [
				( (a, v) => {
					v1 = countof(red, a);
					v2 = countof(green, a);
					v3 = countof(blue, a);
					if ((v1 > 0 && v2 > 0) || v3 > 1) {
						if (v1 > v2) { return red }
						else if (v2 > v1) { return green }
						else { return blue }
					} else { return black }
				} ),
				moore(1).concat([[0, 0]]),
				[black, red, green, blue],
				equ]
		case "rand" :
			CA.value = CAs[randint(CAs.length)];
			return getpars(CA.value);
		default: // c'est que ca vient de create
			a = na.split(".");
			return [
				eval(a[0]), // normalement dangereux, mais avec les restrictions de caracteres ca devrait passer
				JSON.parse(a[1]),
				JSON.parse(a[2]),
				equ]
	}
}
function go(s) { // lance le simulateur avec id s (si s vide, depuis CA)
	move([canvas, Options]);
	if (s) {
		lines = s.split("_");
		kill = true;
		pars = getpars(lines[0]);
		size.value = lines[1];
		delay.value = lines[2];
		wrap.checked = (lines[3] == "true");
		running = lines[0];
	} else {
		pars = getpars(CA.value);
		running = CA.value;
	}
	si = Number(size.value);
	f = pars[0];
	n = pars[1];
	col = pars[2];
	e = pars[3];
	coltoi = []; // pour ne pas avoir a chercher bcp de fois
	for (let i = 0; i < col.length; i++) {
		coltoi[col[i]] = i
	}
	if (s && lines[4] != "rand") { // pour permetre (notamment avec create) d'avoir l'aleatoire avec un id
		g = decompress(lines[4])
	} else {
		g = b();
	}
	z = Math.floor(1000)/si; // taille d'une cellule en pixels
	for (let i = 0; i < si; i++) {
		for (let j = 0; j < si; j++) {
			draw(i, j, g[i][j])
		}
	}
	hide([mobtn, rbtn, qbtn]);
	d = true; // d(irection), true pour avant et false pour arriere
	o = false; // o(nce), si on ne fait qu'une generation
	h = []; // h(istoire) (pile en fait) des grilles, pour aller en arriere (optimisation supplementaire : faire l'histoire des changements (exprimes en coordonees relatives) a la place.)
	g_ = dcopy(g); // grille d'apres
	paused = true;
	kill = false;
	di()
}
// le seul endroit ou on fait vraiment des choses
if (stateid) {
	go(stateid)
} else {
	choose();
}
