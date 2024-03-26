// TODO:
//		création de CA
//		importation
//		réoptimiser l'histoire
const canvas = document.querySelector("canvas");
const ctx = canvas.getContext("2d");
kill = false; // si le CA devrait etre arrete
CAlist = document.getElementById("CAlist");
Infobox = document.getElementById("info");
Options = document.getElementById("Options");
Settings = document.getElementById("Settings");
pbtn = document.getElementById("p"); // p(ause)
abtn = document.getElementById("a"); // a(vancer)
plbtn = document.getElementById("+"); // pl(us)
rbtn = document.getElementById("r"); // r(eculer)
mobtn = document.getElementById("-"); // mo(ins)
mbtn = document.getElementById("m"); // m(odifier)
nbtn = document.getElementById("n"); // n(ouveau)
sbtn = document.getElementById("s"); // s(auver)
ebtn = document.getElementById("e"); // e(ffacer)
xbtn = document.getElementById("x");
ibtn = document.getElementById("i");
delay = document.getElementById("delay");
size = document.getElementById("size");
CA = document.getElementById("CA");
wrap = document.getElementById("wrap");
var whereami; // les indices dans groupes des groupes d'elements qu'on veut montrer
groupes = [canvas, Options, CAlist, Infobox, Settings];
CAs = ["gol", "bosco", "daynight", "marine", "diamoeba", "wireworld", "eiii"];
key_funcs = [];
bind(() => restart(true, false), abtn, "a");
bind(() => restart(true, true), plbtn, "+");
bind(() => restart(false, false), rbtn, "r");
bind(() => restart(false, true), mobtn, "-");
bind(() => { 
	g_ = b(e, si); 
	for (let i = 0; i < si; i++) {
		for (let j = 0; j < si; j++) {
			draw(i, j, g_[i][j])
		}
	};
	di() },
	nbtn, "n");
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
	mbtn, "m");
bind(() => {
	for (let i = 0; i < si; i++) {
		for (let j = 0; j < si; j++) {
			g_[i][j] = "0 0 0";
		}
	};
	ctx.fillStyle = "rgb(0 0 0)";
	ctx.fillRect(0, 0, 1000, 1000);
	},
	ebtn, "e")
bind(() => { hide_(sbtn); show([mbtn, abtn, plbtn, rbtn, mobtn, nbtn, ebtn]) }, sbtn, "s");
bind(pause, pbtn, "p");
bind(stopca, xbtn, "x");
bind(info, ibtn, "i");
bind(() => { 
	g_ = b(e(col), si);
	for (let i = 0; i < si; i++) {
		for (let j = 0; j < si; j++) {
			draw(i, j, g_[i][j])
		}
	}
	}, nbtn, "n")
hide([rbtn, mobtn, sbtn]);
black = "0 0 0";
red = "255 0 0";
green = "0 255 0";
blue = "0 0 255";
white = "255 255 255"; // quelques couleurs predefinies
choose();
paused = true;
window.addEventListener("keydown", keyListener);
CA.addEventListener("change", (() => {if ( CA.value != "none") { go() } } ));
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
function only(a) { // cache tout sauf j
	hide(groupes);
	show(a);
}
function choose() { // efface canvas et passe au menu de selection
	ctx.fillStyle = "rgb(0 0 0)";
	ctx.fillRect(0, 0, 1000, 1000);
	move([CAlist]);
	CA.value = "none";
}
function pause() { paused = true }
function Incoherent_colors() { return new Error("Incoherent colors") }
function randint(n) { return Math.floor(Math.random()*n) }
// une couleur est une chaine de la forme "r g b"
function equ(a) { return (() =>a[randint(a.length)]) } // renvoie une fonction qui choisit un element de a avec equiprobabilite
function b_s (b, s) {
	b = b.split(",");
	s = s.split(",");
	b = b.map((e) => { if (e.indexOf("-") > -1) { return e.split("-").map(Number) } else { return Number(e) } } )
	s = s.map((e) => { if (e.indexOf("-") > -1) { return e.split("-").map(Number) } else { return Number(e) } } )
	return ( (a, v) => {
		count = 0;
		for (let i = 0; i < a.length; i++) {
			if (a[i] == white) {
				count += 1
			}
		};
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
	ctx.fillStyle = "rgb(".concat(c, ")");
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
function stopca() { kill = true; choose() }
function info() { if (!paused) { pause() }; only([Infobox]) }
function settings() { only([Settings]) }
function back() { only(whereami) } // depuis les parametres ou info
function move(x) { whereami = x; back() }
function validate() { // Pour les parametres
	if ( 1000 % size.value == 0 ) { back()} 
	else { alert("Entree invalide. La taille doit etre un diviseur de 1000.") }
}
function bind(f, b, c) {
	b.onclick = f;
	key_funcs[c] = (() => {if (b.style.display != "none") {f.call()} })
};
function keyListener(event) {
	event = event || window.event;
	key = event.key || event.which || event.keyCode;
	if (key in key_funcs) {
		key_funcs[key].call()
	}
};
function update_values () {
	if (d) { // iteration d'apres si on va a l'endroit
		h.push(JSON.stringify(g));
		g = dcopy(g_)
	}
};
function restart(vd, vo) {
	if (vd && !d) {
		tmp = dcopy(g);
		g = dcopy(g_);
		g_ = tmp
	};
	d = vd;
	o = vo;
	paused = false;
	if (!o) {
		hide([abtn, plbtn, rbtn, mobtn, mbtn, nbtn, ebtn]);
		show_(pbtn);
	} else if (!d && h.length == 0) {
		hide([mobtn, rbtn]);
	};
	di()
}
function di() { // Fonction auxiliaire, itere 
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
					pause();
				};
			} else {
				change = "";
				x = 0;
				y = 0;
				cc = 0; // derniere c(ouleur a)c(hanger)
				for (let i = 0; i < si; i++) {
					for (let j = 0; j < si; j++) { 
						co = f(n.map((a) => { 
						i_ = i+a[0];
						j_ = j+a[1];
						if (i_ < 0 || i_ >= si || j_ < 0 || j_ >= si) {
							if (wrap.checked) {
								return g[w(i_)][w(j_)]
							} else {
								console.log(wrap.checked);
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
							change = change.concat(",");
							if (g[i][j] != cc) {
								change = change.concat((fi(g[i][j])).toString())
							};
							change = change.concat(",");
							if (j != y) {
								change = change.concat((j-y).toString())
							};
							change = change.concat(",");
							if (i != x+1) {
								change = change.concat((i-y).toString())
							};
							x = i;
							y = j;
							cc = g[i][j];
						}
					}
				};
				h.push(JSON.stringify(g));
				g = dcopy(g_);
			};
			if (o) { 
				o = false;
			pause()
			};
			sleep(delay.value*1000).then(() => {di()}) // meme si delai nul, remedie a l'asynchronalite de javascript, cad l'empeche d'essayer de tout calculer avant d'afficher
		} else {
			hide_(pbtn);
			show([abtn, plbtn, mbtn, nbtn, ebtn]);
			if (h.length > 0) {
				show([rbtn, mobtn]);
			};
		}
	}
};
function test(l, co) {
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
				if (v == black) {
					if (test(orth,green) && (!(test(diag,red)) || (test(diag,blue) && test(orth,blue)))) { 
						return green
					} else {
						return black
					}
				} else if (v == red) {
					return black
				} else if (v == green) {
					return red
				} else {
					if (!(test(orth,blue)) && !(test(diag,green)) && test(orth,red)) {
						return green
					} else {
						return blue
					}
				}
			} ),
			[[0, 1],[0,-1],[1,0],[-1,0],[-1,-1],[-1,1],[1,-1],[1,1]],
			[black, black, black, black, red, green, blue],
			equ]
		case "wireworld" :
			return []
		case "eiii" :
			return [
				( (a, v) => {
					v1 = 0;
					v2 = 0;
					v3 = 0;
					for (let i = 0; i < a.length; i++) {
						if (a[i] == red) { v1 = v1 + 1 } 
						else if (a[i] == green) { v2 = v2 + 1 }
						else if (a[i] == blue) { v3 = v3 + 1 }
					};
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
function go() {
	pars = getpars();
	f = pars[0];
	n = pars[1];
	col = pars[2];
	e = pars[3];
	move([canvas, Options]);
	si = Number(size.value);
	g = b(e(col), si);
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
	pause();
	kill = false;
	di()
};
