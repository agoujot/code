// TODO:
//		réparer la modification
//		création de CA
//		réoptimiser l'histoire
//		compresser encore plus l'histoire (huffman ?)
const canvas = document.querySelector("canvas");
const ctx = canvas.getContext("2d");
kill = false; // si le CA devrait etre arrete
CAlist = document.getElementById("CAlist");
info = document.getElementById("info");
fileurl = null;
var whereami; // les indices dans groupes des groupes d'elements qu'on veut montrer
groupes = [canvas, Options, CAlist, info, Settings, Save, Upload];
CAs = ["gol", "bosco", "daynight", "marine", "sparks", "eiii"];
pauseonlybuttons = [abtn, plbtn, mobtn, rbtn, mbtn, nbtn, ebtn, sbtn, cbtn];
key_funcs = []; // cles : touches, valeurs : fonctions
key_funcs[" ".charCodeAt(0)] = spacebar; // cas spécial, pas lié à un bouton.
settingsvalid.onclick = () => {
	if ( 1000 % size.value == 0 ) { only (whereami) } 
	else { alert("Entree invalide. La taille doit etre un diviseur de 1000.") }
}
bind(() => restart(true, false), abtn, String.fromCharCode(39)); // fleche de droite
bind(() => restart(true, true), plbtn, String.fromCharCode(38)); // d'en haut
bind(() => restart(false, false), rbtn, String.fromCharCode(37)); // de gauche
bind(() => restart(false, true), mobtn, String.fromCharCode(40)); // d'en bas
bind(() => { 
	g_ = b(e(col), si); 
	for (let i = 0; i < si; i++) {
		for (let j = 0; j < si; j++) {
				draw(i, j, g_[i][j])
		}
	};
	h = []
	},
	nbtn, "N"); // nouveau
bind(() => { alert("Editing temporarily disabled.")
//	hide(pauseonlybuttons);
//	show_(vbtn);
//	canvas.onclick = (e) => {
//		if (vbtn.style.display != "none") {
//			rect = e.target.getBoundingClientRect();
//			mx = Math.floor((e.clientX - rect.left)/z);
//			my = Math.floor((e.clientY - rect.top)/z);
//			old_c = g[mx][my];
//			g[mx][my] = col[(fi(old_c)+1)%col.length];
//			draw(mx, my, g[mx][my]) 
//		}
//	};
	}, 
	mbtn, "M"); // modifier
bind(() => { alert("Editing temporarily disabled.")
//	for (let i = 0; i < si; i++) {
//		for (let j = 0; j < si; j++) {
//				g_[i][j] = "0 0 0";
//		}
//	};
//	ctx.fillStyle = "rgb(0 0 0)";
//	ctx.fillRect(0, 0, 1000, 1000);
//	h = [];
	},
	ebtn, "E") // effacer
bind(() => { hide_(vbtn); show(pauseonlybuttons) }, vbtn, "V"); // valider (modifier)
bind((() => paused = true ), pbtn); // pause
bind((() => {
	text = totext();
	file = new Blob([text]);
	if (fileurl) {
		window.URL.revokeObjectURL(fileurl)
	}
	fileurl = window.URL.createObjectURL(file);
	Download.href = fileurl;
	Download.download = "AC-"+ Math.floor(Date.now()/1000).toString() + ".txt";
	Download.innerHTML = "Télécharger "+ Download.download;
	stateidout.innerHTML = totext();
	only([Save]);
	}), 
	sbtn, "S") // sauvegarder
bind((() => {stateidout.style="background-color:white";  only([Upload])}), cbtn, "C") // charger
bind((() => { kill = true; choose() }), xbtn, "X"); // fermer
bind(() => only([info]), ibtn, "I"); // informations
stateidout.onclick = () => { 
	navigator.clipboard.writeText(totext());
	stateidout.style = "background-color:darkgrey";
}
url = new URL (window.location.href);
hide([rbtn, mobtn, vbtn]);
save = (new URLSearchParams(url.search)).get("save");
black = "0 0 0";
red = "255 0 0";
green = "0 255 0";
blue = "0 0 255";
white = "255 255 255"; // quelques couleurs predefinies
if (save) {
	load(save)
} else {
	choose();
}
paused = true;
filecontent = "";
loadvalid.addEventListener("click", loadlistener);
window.addEventListener("keydown", keyListener);
CAvalid.addEventListener("click", (() => {
	switch (CA.value) { 
		case "none" : 
			break
		case "import" : 
			only([Upload]);
			break
		default : 
			go(true)
	} 
}));
function totext() { // renvoie une chaine decrivant etape actuelle, parametres et CA
	return (
	CA.value + "-" + 
	si.toString() + "-" + 
	col.map((s) => s.split(" ").join(".")).join() + "-" + 
	delay.value.toString() + "-" + 
	wrap.checked + "-" + 
	compress(g_))
}
function spacebar () { // pour faire à la fois play et pause
	if (pbtn.style.display != "none") {
		paused = true
	} else {
		restart(d, false)
	}
}
function loadlistener () {
	if (stateidin.value == "") {
		files = filename.files;
		file = files[0];
		reader = new FileReader();
		reader.onload = ((a) => {filecontent = a.target.result})
		reader.readAsText(file);
		waitforfile()
	} else {
		load(stateidin.value)
	}
}
function waitforfile() { // pour ne pas que load s'exécute avant que filecontent existe
	if (filecontent == "") {
		setTimeout(waitforfile, 100) 
	} else {
		load(filecontent)
	}
}
function load(s) {
	lines = s.split("-");
	kill = true;
	CA.value = lines[0];
	size.value = lines[1];
	col = lines[2].split(",").map((s) => s.split(".").join(" "));
	delay.value = Number(lines[3]);
	wrap.checked = lines[4];
	g = decompress(lines[5]);
	go(false)
}
function tob62(n) { // en base 62, cad 0 .. 9 + A .. Z + a .. z. Assume, dans son implementation actuelle, que il y a moins de 62*62 couleurs.
	res = ""
	if (n == 0) {
		return "0"
	} else if (n < 10) {
		return n.toString()
	} else if (n < 36) {
		return String.fromCharCode(n-10+65)
	} else if (n < 62) {
		return String.fromCharCode(n-36+97)
	} else {
		return ("'"+tob62(Math.floor(n / 62)) + tob62(n % 62))
	}
}
function fromb62(s) {
	for (let i = 0; i < s.length; i++) {
		if (s >= "0" && s <= "9") {
			return Number(s)
		} else if (s >= "A" && s <= "Z") {
			return (s.charCodeAt()-65+10)
		} else {
			return (s.charCodeAt()-97+36)
		}
	};
}
function compress (aa) { // tableau de tableau -> chaine
	res = "";
	coltoi = []; // pour ne pas refaire beaucoup de fois fi
	for (let i = 0; i < col.length; i++) {
		coltoi[col[i]] = i
	};
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
				res += "$" + tob62(ti)
			}
			lt = -1;
			ti = 0;
			for (let j = 0; j < si; j++) {
				k = coltoi[aa[i][j]];
				if (k != lt) {
					if (lt != -1) {
						res += tob62(lt) + tob62(ti);
					};
					lt = k;
					ti = 1;
				} else {
					ti += 1
				}
			}
			res += tob62(lt) + tob62(ti);
		}
	};
	return res
}
function decompress (s) { // chaine -> tableau de tableau (a besoin des parametres)
	res = [];
	l = s.split("");
	l1 = [];
	i = 0;
	while (i < l.length) {
		if (l[i] == "'") {
			l1.push(fromb62(l[i+1])*62+fromb62(l[i+2]));
			i += 3
		} if (l[i] == "$") {
			l1.push("$");
			l1.push(fromb62(l[i+1]));
			i += 2
		} else {
			l1.push(fromb62(l[i]));
			i ++
		}
	};
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
			};
		}
		i += 2;
		if (c == Number(size.value)) {
			c = 0;
			res.push(dcopy(l2));
			l2 = [];
		}
	}
	return res
}
function moore(r) { // voisinage de moore de rayon r
	res = [];
	for (let i = -r; i <= r; i++) {
		for (let j = -r; j <= r; j++) {
			if (i != 0 || j != 0) { res.push([i, j]) }
		}
	};
	return res
}
function hide_(e) { e.style.display = "none" }
function hide(a) { for (let i = 0; i < a.length; i++) { hide_(a[i]) } }
function show_(e) { e.style.display = ""  }
function show(a) { for (let i = 0; i < a.length; i++) { show_(a[i]) } }
function only(a) {
	hide(groupes);
	show(a);
}
function choose() { // efface canvas et passe au menu de selection
ctx.fillStyle = "rgb(0 0 0)";
	ctx.fillRect(0, 0, 1000, 1000);
	move([CAlist]);
	CA.value = "none";
}
function randint(n) { return Math.floor(Math.random()*n) }
function equ(a) { return (() =>a[randint(a.length)]) } // renvoie une fonction qui choisit un element de a avec equiprobabilite
function pondered(caps) { // renvoie une fonction, qui prend un tableau de couleurs et renvoie une fonction qui en choisit une au hasard avec une pondération définie dans caps.
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
function b_s (b, s) { // interprete la notation classique B/S, pour quand il y a deux types de vie. voir rulestring sur lifewiki
	b = b.split(",");
	s = s.split(",");
	b = b.map((e) => { if (e.indexOf("-") > -1) { return e.split("-").map(Number) } else { return Number(e) } } )
	s = s.map((e) => { if (e.indexOf("-") > -1) { return e.split("-").map(Number) } else { return Number(e) } } )
	return ( (a, v) => {
		count = countof(white, a);
		if (v == black) {
			for (let j = 0; j < b.length; j++) {
				if (typeof(b[j]) == "number") {
					if (count == b[j]) {
						return white
					}
				} else {
					if (count >= b[j][0] && count <= b[j][1]) {
						return white
					}
				};
			};
			return black
		} else {
			for (let j = 0; j < s.length; j++) {
				if (typeof(s[j]) == "number") {	
					if (count == s[j]) {
						return white
					}
				} else {
					if (count >= s[j][0] && count <= s[j][1]) {
						return white
					}
				};
			};
			return black
		}
	} )
}
function b(e, si) { // b(uild), construit un tableau de tableaux de si*si rempli avec les resultats de e()
	res = [];
	for (let i = 0; i<si; i++) {
		ligne = [];
		for (let j = 0; j<si; j++) {
			ligne[j] = e();
		}
		res[i] = ligne;
	}
	return res;
}
function dcopy(aa) { return JSON.parse(JSON.stringify(aa)); }
function sleep(ms) { return new Promise(resolve => setTimeout(resolve, ms)); };
function draw (x, y, c) { // dessine un carre de couleur c en x, y (z le cote en pixels d'une cellule)
	ctx.fillStyle = "rgb("+ c + ")";
	ctx.fillRect((x*z), (y*z), z, z);
};
function w(k) { // w(rap) de k(oordinates)
	if (k >= si) {
		return k - si;
	} else if (k < 0) {
		return si + k;
	} else {
		return k;
	}
}
function fi(co) { // Find Index, renvoie la position dans col où se trouve co
	i = -1;
	for (let j = 0; j < col.length; j++) {
		if (col[j] == co) {
			i = j;
			break;
		}
	}
	if (i == -1) {
		return Incoherent_colors()
	} else {
			return i
	}
}
function move(x) { whereami = x; only(x) }
function bind(f, b, c) { // attache au bouton b et a la touche c la fonction f
	b.onclick = f;
	if (c) {
		key_funcs[c.charCodeAt(0)] = (() => {if (b.style.display != "none" && Options.style.display != "none") {f.call()} })
	}
};
function keyListener(event) {
	key = event.keyCode;
	if (key in key_funcs) {
		key_funcs[key].call()
	}
};
function restart(vd, vo) {
	if (vd && !d) { // changement de sens d'arrière en avant nécessite de retourner
		tmp = dcopy(g);
		g = dcopy(g_);
		g_ = tmp
	};
	d = vd;
	o = vo;
	paused = false;
	if (!o) { // pas la peine de cacher les boutons si c'est pour les rafficher juste apres
		hide(pauseonlybuttons);
		show_(pbtn);
	} else if (!d && h.length == 0) {
		hide([mobtn, rbtn]);
	};
	di()
}
function di() { // Do It, fait l'itération principale 
	if (!kill) {
		if (!paused) {
			if (!d) {
				if (h.length > 0) {
					g = dcopy(g_);
					g_ = JSON.parse(h.pop());
					for (let i = 0; i < si; i++) {
						for (let j = 0; j < si; j++) {
							if (g[i][j] != g_[i][j]) {
								draw(i, j, g_[i][j])
							}
						}
					}
				} else {
					paused = true;
				};
			} else {
				for (let i = 0; i < si; i++) {
					for (let j = 0; j < si; j++) { 
						co = f(n.map((a) => { // nouvelle couleur
							i_ = i+a[0];
							j_ = j+a[1];
							if (i_ < 0 || i_ >= si || j_ < 0 || j_ >= si) {
								if (wrap.checked) { // si il faut prendre de l'autre coté
									return g[w(i_)][w(j_)]
								} else { // si il faut compter une bordure de cases vides
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
				};
				h.push(JSON.stringify(g));
				g = dcopy(g_);
			};
			if (o) { 
				o = false;
				paused = true;
			};
			sleep(delay.value*1000).then(() => {di()}) // meme si delai nul, remedie a l'asynchronalite de javascript, cad l'empeche d'essayer de tout calculer avant d'afficher, ce qui est problematique avec la boucle infinie intentionelle
		} else {
			hide_(pbtn);
			show(pauseonlybuttons);
			if (h.length == 0) {
				hide([rbtn, mobtn]);
			};
		}
	}
};
function inarray(l, co) {
	for (let i = 0; i < l.length; i++) {
		if (l[i] == co) { return true }
	}
	return false;
};
function getpars(n) { // donne les parametres correspondant au nom n
	switch (n) {
		case "gol" :
			return [
			b_s("3", "2,3"),
			moore(1),
			[black, white],
			equ]
		case "bosco" :
			return [
			b_s("34-45", "33-57"),
			moore(5),
			[black, white],
			equ]
		case "daynight" :
			return [
			b_s("3,6-8", "3-4,6-8"),
			moore(1),
			[black, white],
			equ]
		case "marine" :
			return [
			b_s("6-8", "4,6-9"),
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
						if (!(inarray(orth,blue)) && !(inarray(diag,green)) && inarray(orth,red)) {
							return green
						} else {
							return blue
						}
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
			next =  CAs[randint(CAs.length)];
			return getpars(next);
		case "create":
		default: 
			alert(n)
	}
}
function go(rand) { // lance le simulateur, avec une grille aléatoire si rand
	pars = getpars(CA.value);
	f = pars[0];
	n = pars[1];
	col = pars[2];
	e = pars[3];
	move([canvas, Options]);
	si = Number(size.value);
	if (rand) {
		g = b(e(col), si);
	}
	z = Math.floor(1000)/si; // taille d'une cellule en pixels
	for (let i = 0; i < si; i++) {
		for (let j = 0; j < si; j++) {
			draw(i, j, g[i][j])
		}
	};
	d = true; // d(irection), true pour avant et false pour arriere
	o = false; // o(nce), si on ne fait qu'une generation
	h = []; // h(istoire) (pile en fait) des grilles, pour aller en arriere (optimisation supplementaire : faire l'histoire des changements a la place.)
	g_ = dcopy(g); // grille d'apres
	paused = true;
	kill = false;
	di()
};
