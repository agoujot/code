// TODO:
//		vars dans URL
//		création de CA
//		réoptimiser l'histoire
const canvas = document.querySelector("canvas");
const ctx = canvas.getContext("2d");
kill = false; // si le CA devrait etre arrete
CAlist = document.getElementById("CAlist");
Infobox = document.getElementById("info");
Options = document.getElementById("Options");
Settings = document.getElementById("Settings");
Download = document.getElementById("Download");
pbtn = document.getElementById("p"); // p(ause)
abtn = document.getElementById("a"); // a(vancer)
plbtn = document.getElementById("+"); // pl(us)
rbtn = document.getElementById("r"); // r(eculer)
mobtn = document.getElementById("-"); // mo(ins)
mbtn = document.getElementById("m"); // m(odifier)
nbtn = document.getElementById("n"); // n(ouveau)
sbtn = document.getElementById("s"); // s(auver)
ebtn = document.getElementById("e"); // e(ffacer)
dbtn = document.getElementById("d"); // d(ownload)
ubtn = document.getElementById("u"); // u(pload)
xbtn = document.getElementById("x");
ibtn = document.getElementById("i");
delay = document.getElementById("delay");
size = document.getElementById("size");
CA = document.getElementById("CA");
wrap = document.getElementById("wrap");
fileurl = null;
var whereami; // les indices dans groupes des groupes d'elements qu'on veut montrer
groupes = [canvas, Options, CAlist, Infobox, Settings, Download, Upload];
CAs = ["gol", "bosco", "daynight", "marine", "sparks", "eiii"];
pauseonlybuttons = [abtn, plbtn, mobtn, rbtn, mbtn, nbtn, ebtn, dbtn, ubtn];
key_funcs = []; // cles : touches, valeurs : fonctions
key_funcs[" ".charCodeAt(0)] = spacebar; // cas spécial, pas lié à un bouton.
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
	},
	nbtn, "N");
bind(() => {
	hide([mbtn, abtn, plbtn, rbtn, mobtn, nbtn, ebtn]);
	show_(sbtn);
	canvas.onclick = function(e) {
		if (sbtn.style.display != "none") {
			rect = e.target.getBoundingClientRect();
			mx = Math.floor((e.clientX - rect.left)/z);
			my = Math.floor((e.clientY - rect.top)/z);
			old_c = g_[mx][my];
			g_[mx][my] = col[(fi(old_c)+1)%col.length];
			draw(mx, my, g_[mx][my]) 
		}
	};
	}, 
	mbtn, "M");
bind(() => {
	for (let i = 0; i < si; i++) {
		for (let j = 0; j < si; j++) {
			g_[i][j] = "0 0 0";
		}
	};
	ctx.fillStyle = "rgb(0 0 0)";
	ctx.fillRect(0, 0, 1000, 1000);
	},
	ebtn, "E")
bind(() => { hide_(sbtn); show([mbtn, abtn, plbtn, rbtn, mobtn, nbtn, ebtn]) }, sbtn, "S");
function spacebar () {
	if (pbtn.style.display != "none") {
		paused = true
	} else {
		restart(d, false)
	}
}
bind((() => paused = true ), pbtn);
bind((() => {
	text = CA.value + "\n" + si.toString() + "\n" + col.join() + "\n" + delay.value.toString() + "\n" + wrap.checked + "\n" + compress(g_);
	file = new Blob([text]);
	if (fileurl) {
		window.URL.revokeObjectURL(fileurl)
	}
	fileurl = window.URL.createObjectURL(file);
	Download.href = fileurl;
	Download.download = "AC-"+ Math.floor(Date.now()/1000).toString() + ".txt";
	only([Download]);
	}), 
	dbtn, "D")
bind((() => {
	only([Upload])}), ubtn, "U")
bind((() => { kill = true; choose() }), xbtn, "X");
bind(info, ibtn, "I");
hide([rbtn, mobtn, sbtn, dbtn, ubtn]);
black = "0 0 0";
red = "255 0 0";
green = "0 255 0";
blue = "0 0 255";
white = "255 255 255"; // quelques couleurs predefinies
choose();
paused = true;
filecontent = "";
nameofcurrent = "";
filename.addEventListener("change", fileListener);
window.addEventListener("keydown", keyListener);
CA.addEventListener("change", (() => {if ( CA.value != "none") { go(true) } } ));
function fileListener (e) {
	only(whereami);
	files = e.target.files;
	file = files[0];
	reader = new FileReader();
	reader.onload = ((a) => { filecontent = a.target.result } );
	reader.readAsText(file);
	sleep(100).then(load)
}
function load() {
	lines = filecontent.split("\n");
	kill = true;
	CA.value = lines[0];
	size.value = lines[1];
	col = lines[2].split(",");
	delay.value = Number(lines[3]);
	wrap.checked = lines[4];
	g = decompress(lines[5]);
	go(false)
}
function compress (aa) {
	res = "";
	coltoi = []; // pour ne pas refaire beaucoup de fois fi
	for (let i = 0; i < col.length; i++) {
		coltoi[col[i]] = i
	};
	for (let i = 0; i < si; i++) {
		lt = -1;
		ti = 0;
		for (let j = 0; j < si; j++) {
			k = coltoi[aa[i][j]];
			if (k != lt) {
				if (lt != -1) {
					res += "," + lt.toString() + "," + ti.toString();
				};
				lt = k;
				ti = 1;
			} else {
				ti += 1
			}
		}
		res += "," + lt.toString() + "," + ti.toString();
		res += "|"
	};
	return res
}
function decompress (s) {
	res = s.split("|");
	res = res.map((l) => {
		l1 = l.split(",");
		l1.shift();
		l2 = [];
		for (let i = 0; i < l1.length; i+=2) {
			for (let j = 0; j < l1[i+1]; j++) {
				l2.push(col[l1[i]])
			}
		};
		return l2 
		});
	return res
}
function moore(r) {
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
function countof(x, a) {
	count = 0; 
	for (let i = 0; i < a.length; i++) {
		if (a[i] == x) {
			count += 1
		}
	}
	return count
}
function b_s (b, s) { // interprete la notation classique B/S, pour quand il y a deux types de vie.
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
function b(e, si) { // construit un tableau de tableaux de si*si rempli avec les resultats de e()
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
function info() { if (!paused) { paused = true }; only([Infobox]) }
function move(x) { whereami = x; only(x) }
function validate() { // Pour les parametres
	if ( 1000 % size.value == 0 ) { only (whereami) } 
	else { alert("Entree invalide. La taille doit etre un diviseur de 1000.") }
}
function bind(f, b, c) { // attache au bouton b et a la touche c la fonction f
	b.onclick = f;
	if (c) {
		key_funcs[c.charCodeAt(0)] = (() => {if (b.style.display != "none") {f.call()} })
	}
};
function keyListener(event) {
	key = event.keyCode;
	if (key in key_funcs) {
		key_funcs[key].call()
	}
};
function restart(vd, vo) {
	if (vd && !d) { // changement de sens
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
function di() { // Do It 
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
		//		change = ""; // pour l'instant pas utilise
		//		x = 0;
		//		y = 0;
		//		cc = 0; // derniere c(ouleur a)c(hanger)
				for (let i = 0; i < si; i++) {
					for (let j = 0; j < si; j++) { 
						co = f(n.map((a) => { 
						i_ = i+a[0];
						j_ = j+a[1];
						if (i_ < 0 || i_ >= si || j_ < 0 || j_ >= si) {
							if (wrap.checked) {
								return g[w(i_)][w(j_)]
							} else {
								return black
							}
						} else {
							return g[i_][j_]
						}
						}), g[i][j]); // nouvelle couleur
						changed = (co != g[i][j]);
						if (changed) {
							g_[i][j] = co;
							draw(i, j, co);
			//				change = change.concat(",");
			//				if (g[i][j] != cc) {
			//					change = change.concat((fi(g[i][j])).toString())
			//				};
			//				change = change.concat(",");
			//				if (j != y) {
			//					change = change.concat((j-y).toString())
			//				};
			//				change = change.concat(",");
			//				if (i != x+1) {
			//					change = change.concat((i-y).toString())
			//				};
			//				x = i;
			//				y = j;
			//				cc = g[i][j];
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
			sleep(delay.value*1000).then(() => {di()}) // meme si delai nul, remedie a l'asynchronalite de javascript, cad l'empeche d'essayer de tout calculer avant d'afficher
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
function getpars() {
	switch (CA.value) {
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
			pondered([0.5, 0.7, 0.9, 1])]
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
			return getpars();
		case "create":
		case "import":
	}
}
function go(rand) {
	pars = getpars();
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
	o = false; // o(ne), si on ne fait qu'une generation
	h = []; // h(istoire) des changements, pour aller en arriere
	g_ = dcopy(g); // grille d'apres
	paused = true;
	kill = false;
	di()
};
