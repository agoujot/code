var ctx = canv.getContext("2d"); // drawing context
var scroll = [0, 0];
var z, scroll, eta, freeze
var fr = (x, y, w, h, co, vis = true) => { // fill rect
	ctx.fillStyle = (co=="transparent")?co:("#"+co);
	ctx.fillRect(x, y, w, h);
	if (dm && !freeze && vis && players) {
		if (co == "F00") co = "7F7F7F"; // red appears as floor to the players, for traps
		if (co == "FF0") co = "FFF"; // yellow appears as wall, for passages
		buffer += JSON.stringify([x, y, w, h, co]) + "|"; // could have gone for better encoding, too lazy to
	}
};
var invert = () => {
	if (canv.style.filter.includes("1")) {
		canv.style.filter = "invert(0)";
	} else {
		canv.style.filter = "invert(1)";
	}
	if (dm) {
		players.postMessage("invert", "*");
	}
};
if (dm) {
players = null;
canv.addEventListener("click", (e) => {
	let rect = canv.getBoundingClientRect(); // get the coordinates of the click relative to canvas
	let x = e.clientX - rect.left;
	let y = e.clientY - rect.top;
	if (0 <= x && x < 900 && 0 <= y && y < 900) {
		x = Math.floor((x-scroll[0])/z);
		y = Math.floor((y-scroll[1])/z);
		if (picking) { // resolves some promises
			picked([x, y]);
		} else if (!done) { // editing mode 0
			colors[x+" "+y] = c;
			display();
		}
	}
})
addEventListener("keydown", (e) => {
	if (["0", "1", "2", "3", "4", "5", "6"].includes(e.key)) { // color
		c = Number(e.key);
		ccs.style.backgroundColor = "#" + cols(c);
	} else if (!done && ["w", "x", "t", "r", "b"].includes(e.key)) {
		eval(e.key+"btn.onclick()");
	} else if (bordering && ["z", "q", "s", "d"].includes(e.key)) {
		borderchosen(ktod[e.key]);
	} else if (document.activeElement.nodeName != "INPUT") {
		switch (e.key) {
			case("ArrowUp"): sets(1, "-=20"); break
			case("ArrowDown"): sets(1, "+=20"); break
			case("ArrowLeft"): sets(0, "-=20"); break
			case("ArrowRight"): sets(0, "+=20"); break
			case("+"): setz("+=2"); break
			case("-"): setz("-=2"); break
			case("PageUp"): setl("+=1"); break
			case("PageDown"): setl("-=1"); break
		}
	}
}, true)
var ktod = {z:1, q:0, s:3, d:2};
var salles = {};
var groups = {};
var fileurl = null;
var done = true;
var picking = false;
var quad = false;
var bordering = false;
var nextid = 1;
var nextgroupid = -1;
var cols = (s) => 
	(s==0)?"000":
	(s==1)?"7f7f7f":
	(s==2)?"F00":
	(s==3)?"FF0":
	(s==4)?"8B4513":
	(s==5)?"FF0000":
	(s==6)?"FFFF00":
	s.slice(1);
c = 1;
var buffer = "";
commands = {
	"list":"listrooms", 
	"h":"help", "help":"help", 
	"add":"addroom", "a":"addroom",
	"edit":"editroom", "e":"editroom",
	"level":"gotolevel",
	"zoom":"gotozoom",
	"freeze":"freez", "f":"freez",
	"goto":"gotoroom", "g":"gotoroom",
	"o":"openp", "open":"openp",
	"s":"save", "save":"save",
	"load":"load", "l":"load",
	"remove":"removeroom",
	"view":"view",
	"hide":"hide",
	"rotate":"rotate",
	"copy":"copyroom",
	"set":"makegroup",
	"grid":"togglegrid",
	"invert":"invert",
};
var openp = () => { 
	players = window.open("http://htmlpreview.github.io/?https://github.com/agoujot/code/blob/main/dp.html", "Players", "popup"); setTimeout(display, 1000);
	show(`Window opened.
If it didn't make sure you allowed pop-ups for this website (your browser probably gave you a notice).
Click on the player window to make it go fullscreen.`)
}
var needsroom = (callback, list=false) => (arg) => { // metafunction to make commands
	if (arg) { // command-line argument
		if (arg[0] == "+" && arg.slice(1) in salles) {
			callback(arg.slice(1));
			return;
		}
		if (arg[0] == "-" && arg in groups) {
			if (list) {
				for (x of groups[arg].l) {
					callback(x);
				}
			} else {
				write("This command does not accept sets.", "error");
			}
			return
		}
		for (id of Object.keys(salles)) {
			if (salles[id].n == arg) {
				callback(id);
				return;
			}
		}
		for (id of Object.keys(groups)) {
			if (groups[id].n == arg) {
				if (list) {
					for (x of groups[id].l) {
						callback(x)
					}
				} else {
					write("This command does not accpt sets.", "error");
				}
				return
			}
		}
		if (arg == "all") {
			if (list) {
				for (x of Object.keys(salles)) {
					callback(x);
				}
			} else {
				write("This command does not accept sets.", "error");
			}
			return;
		}
		return write("No room or set of that name exists.", "error")
	} else { // go fetch from selectroom
		selectroom().then( (arg) => {
			if (Number(arg) > 0) {
				callback(Number(arg).toString()); // eliminate unary +s
			} else {
				if (list) {
					for (x of groups[arg].l) {
						callback(x);
					}
				} else {
					write("This command does not accept sets.", "error");
				}
			}
		}) 
	}
};
var gotoroom = needsroom((id) =>  { // more precisely, put it in top left
	let co = bl(salles[id].c);
	setl("="+salles[id].f[0].toString());
	sets("0", "="+(-co[0]*z).toString());
	sets("1", "="+(-co[1]*z).toString());
})
var hide = needsroom( (id) => { // from players
	salles[id].v = false;
	display();
	show(salles[id].n + " was hidden to the players.");
}, true)
var view = needsroom( (id) => { // also players
	salles[id].v = true;
	display();
	show(salles[id].n + " was shown to the players.");
}, true)
var bl = ([s, x, y]) => // build location maybe? it was a while ago
	(s==0)?[x, y]:[bl(salles[s].c)[0]+x, bl(salles[s].c)[1]+y]
var draw = (i, j, co, show=true) => fr(i*z+scroll[0], j*z+scroll[1], z, z, co, show); // draw a cell
var displayroom = (id) => {
	let s = salles[id];
	let coord = bl(s.c);
	let wallwidth = Math.round(z/10);
	for (let i=0;i<s.g.length;i++) {
		for (let j=0;j<s.g[i].length;j++) {
			if (s.g[i][j]) { // don't draw black to allow some kerning
				let l = s.g[i][j];
				if (typeof(l) != "object") {
					l = [l];
				}
				draw(i+coord[0], j+coord[1], cols(l[0]), s.v);
				for (let [x, y, m] of [[0, 1, 3], [1, 0, 2], [0, -1, 1], [-1, 0, 0]]) {
					let [i_, j_] = [i+x, j+y];
					let here = null;
					for (let n=1;n<l.length;n+=2) {
						if (l[n] == m) {
							here = n;
							break;
						}
					}
					if (here != null) {
						var co = cols(l[here+1]);
					} else {
						var co = "FFF";
					}
					if (0 > i_ || i_ >= s.g.length || 0 > j_ || j_ >= s.g[0].length || s.g[i_][j_] == 0) {
						if (m == 0) {
							fr((i+coord[0])*z+scroll[0], (j+coord[1])*z+scroll[1], wallwidth, z, co, s.v);
						} else if (m == 1) {
							fr((i+coord[0])*z+scroll[0], (j+coord[1])*z+scroll[1], z, wallwidth, co, s.v);
						} else if (m == 2) {
							fr((i+coord[0]+1)*z+scroll[0]-wallwidth, (j+coord[1])*z+scroll[1], wallwidth, z, co, s.v);
						} else if (m == 3) {
							fr((i+coord[0])*z+scroll[0], (j+coord[1]+1)*z+scroll[1]-wallwidth, z, wallwidth, co, s.v);
						}
					}
				}
			}
		}
	}
	let args = [s.n, coord[0]*z+scroll[0], coord[1]*z+scroll[1]+12];
	ctx.font = "12px serif"; // make a black shadow beneath the white text. the shadow's cut, so not very pretty, but allows to see it on any background
	ctx.fillStyle = "black";
	ctx.fillText(args[0], args[1], args[2]);
	ctx.fillStyle = "white";
	ctx.fillText(args[0], args[1]+2, args[2]+2);
}
var erase = () => fr(0, 0, 900, 900, "000"); // the canvas
var display = () => { // all rooms on this floor || what's being edited && grid
	erase();
	if (done) {
		for (id of Object.keys(salles)) {
			if (salles[id].f.includes(eta)) {displayroom(id)};
		}
		if (quad) {
			drawgrid();
		}
	} else {
		let wallwidth = Math.ceil(z/10);
		for (p of Object.keys(colors)) {
			let [i, j] = p.split(" ").map(Number);
			if (colors[p]) {
				draw(i, j, cols(colors[p]));
				let l = colors[p];
				if (typeof(l) != "object") {
					l = [l];
				}
				for (let [x, y, m] of [[0, 1, 3], [1, 0, 2], [0, -1, 1], [-1, 0, 0]]) {
					let [i_, j_] = [i+x, j+y];
					let here = null;
					for (let n=1;n<l.length;n+=2) {
						if (l[n] == m) {
							here = n;
							break;
						}
					}
					if (here != null) {
						var co = cols(l[here+1]);
					} else {
						var co = "FFF";
					}
					if (!((i_+" "+j_) in colors) || colors[i_+" "+j_] == 0) {
						if (m == 0) {
							fr(i*z+scroll[0], j*z+scroll[1], wallwidth, z, co);
						} else if (m == 1) {
							fr(i*z+scroll[0], j*z+scroll[1], z, wallwidth, co);
						} else if (m == 2) {
							fr((i+1)*z+scroll[0]-wallwidth, j*z+scroll[1], wallwidth, z, co);
						} else if (m == 3) {
							fr(i*z+scroll[0], (j+1)*z+scroll[1]-wallwidth, z, wallwidth, co);
						}
					}
				}
			}
		}
		drawgrid();
	}
	flush();
}
var drawgrid = () => {
	let [imin, jmin] = scroll.map((x) => (x % z) - z);
	for (let i=imin;i<imin+900+z;i+=z) {
		for (let j=jmin;j<jmin+900+z;j+=z) {
			fr(i, j, z, 1, "FFF");
			fr(i, j, 1, z, "FFF");
		}
	}
}
var togglegrid = () => {
	quad = !quad;
	if (quad) {
		show("Grid activated.")
	} else {
		show("Grid deactivated.")
	}
	display();
}
var setz = (s) => { // set zoom
	eval("z"+s);
	z = Math.abs(z);
	zoomspan.innerHTML = z.toString() + " px/sq";
	display();
}
var setl = (s) => { // set level
	eval("eta"+s);
	levelspan.innerHTML = eta.toString();
	display();
}
var sets = (a, b) => { // set scroll
	eval("scroll["+a+"]"+b);
	scrollspan.innerHTML = scroll.map((x) => x.toString()).join("px,") + "px";
	display();
}
var flush = () => {if (buffer) { players.postMessage(buffer, "*") } buffer = "" };
var freez = () => { // toggle freeze
	if (freeze) {
		freeze = false;
		display();
		freezespan.innerHTML = "no";
	} else {
		freeze = true;
		freezespan.innerHTML = "yes";
	}
}
var gotolevel = (s) => setl("= Number("+s+")");
var gotozoom = (s) => setz("= Number("+s+")");
var escap = (s) => s // escape for HTML
	.replaceAll('"', "&quot;")
	.replaceAll("'", "&apos;")
	.replaceAll("&", "&amp;")
	.replaceAll("<", "&lt;")
	.replaceAll(">", "&gt;");
var ssl = (s) => { // show single line (multiple lines in one tr)
	show(s.replaceAll("\n", "<br>"));
}
rmlastline = () => { // technically last tr, which is where ssl comes in
	log.lastChild.remove();
}
var cs = (co) => '<button onclick="c='+co.toString()+`;ccs.style.backgroundColor = '#'+cols(c);" style="background-color:#` + cols(co) + '">&emsp;</button>' // colored span
var inp = () => { // input
	log.innerHTML = log.innerHTML.replaceAll(/<\/?(butto|inpu).*?[^=]>/g, ""); // remove interface from last commands
	[picking, done] = [false, true];
	let l = input.value; // input's the ID of the input element
	write(l, "in");
	input.value = "";
	let com = Object.keys(commands).find((comm) => l.startsWith(comm+" ") || l == comm); // match a command. the +" " and the trim at 223 might be duplicates but not sure
	let f = commands[com];
	if (f) {
		eval(f+"('" + l.slice(com.length).trim() + "')");
	}
	else write("Unknown command", "error");
};
input.addEventListener("keyup", (e) => { if (e.keyCode == "13" && !e.shiftKey) inp() }); // send on enter
var write = (s, clas) => log.innerHTML += ("<tr><td class='" + clas + "'>" + s.trim().split("\n").join("</td></tr><tr><td class='" + clas + "'>") + "</td></tr>"); //  add a tr to log, with clas
commandlist.innerHTML = Object.keys(commands).map((x) => '<option>' + x + '</option>').join(""); // datalist of commands for autosuggest
var show = (s) => s.split("\n").map((l) => (l)?l:"&nbsp;").forEach((s) => write(s, "out")); // show text as "out"
var makegroup = (s) => { // technically also update, but behaves the same
	new Promise((y, n) => gnf = y).then((na) => {
	ide = 0
	for (id of Object.keys(groups)) {
		if (groups[id].n == na) {
			ide = id;
			break;
		}
	}
	if (!ide) {
		ide = nextgroupid;
		nextgroupid --;
		groups[ide] = { n:na, l:new Set() };
	}
	ssl(`Current rooms: <ul id="grouplist"></ul>
Empty the set to destroy it.
<button onclick="selectroom().then((id) => { groups[ide].l.add(id); updategrouplist() } )">Add a room</button><button onclick="selectroom().then((id) => { groups[ide].l.delete(id); updategrouplist() })">Remove a room</button><button onclick="savegroup()">Save</button>`);
	updategrouplist = () => {
		if (groups[ide].l.size == 0) {
			grouplist.innerHTML = "(None)";
		} else {
			grouplist.innerHTML = "";
			for (iden of groups[ide].l) {
				grouplist.innerHTML += "<li>"+salles[iden].n+"</li>"
			}
		}
	}
	savegroup = () => { // my save buttons are often a sham, everything's already saved, I'm just making sure you finished to try and not resort to inp's ugly methods
		rmlastline();
		if (groups[ide].l.size == 0) {
			delete groups[ide];
			show("Set deleted as empty.");
		} else {
			show("Set saved.");
		}
	};

	updategrouplist();
	})
	if (s) { // can't rely on needsroom and selectroom here
		gnf(s); // group, name, and something in f (finished?) I forgot very quickly
	} else {
		show(`<input type="text" id="nameinp" placeholder="Enter name"/> and then <button onclick="if (!(nameinp.value.includes('+') || nameinp.value.includes('-'))) { let val = nameinp.value; rmlastline(); gnf(val); } else { alert('+ and - are reserved for IDs.') }">continue</button>`)
	}
}
function tob64(n) { // en base 64, cad 0 .. 9 + A .. Z + a .. z + - + . (Assume, dans son implementation actuelle, que il y a moins de 2**12 couleurs et qu'aucune salle ne fait plus de 2**12 cases de large)
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
function fromb64(s) { // ne prend que les chiffres, tog pretraite les '
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
var dcopy = (aa) => JSON.parse(JSON.stringify(aa)); // deepcopy of array array. worst part is it's apparently the best way
var frg = (aa) => { // tableau de tableau -> chaine
	let res = tob64(aa[0].length); // encode length of a line for later
	let lt = -1; // pointedly not a color id
	let ti = 0;
	let customs_ = []; // custom colors. my underscores are a bad habit
	aa.forEach((l) => l.forEach((val) => {
		if (typeof(val) != "object") {
			val = [val];
		}
		for (let i=0;i<val.length;i+=2) {
			if (!(val[i] in customs_) && typeof(val[i]) == "string") {
				customs_.push(val[i])
			}
		}
	}));
	let customs = {};
	customs_.map((c, i) => customs[c] = i+10);
	res += customs_.join("")+"|";
	for (let i = 0; i < aa.length; i++) {
		if (i > 0 && JSON.stringify(aa[i]) == JSON.stringify(aa[i-1])) {
			if (lt == "$") { // already same line
				ti += 1;
			} else {
				lt = "$"; // switch to same-line
				ti = 1
			}
		} else {
			if (lt == "$") { // flush the same-line's
				res += "$" + tob64(ti);
			}
			lt = -1;
			ti = 0;
			for (let j = 0; j < aa[0].length; j++) {
				k = aa[i][j];
				if (typeof(k) != "object") {
					k = [k]
				}
				for (let n=0;n<k.length;n++) {
					if (typeof(k[n]) == "string") { // -> not a number -> custom color
						k[n] = customs[k[n]];
					}
					k[n] = tob64(k[n]);
				}
				if (k.length == 1) {
					k = k[0];
				} else {
					k = k[0] + "&" + k.slice(1).join("") + "&";
				}
				if (k != lt) {
					if (lt != -1) {
						res += lt + tob64(ti); 
					}
					lt = k;
					ti = 1;
				} else {
					ti += 1
				}
			}
		}
		if (lt != "$") {
			res += lt + tob64(ti);
		}
	}
	if (lt == "$") {
		res += lt + tob64(ti);
	}
	return res
}
var tog = (s) => { // chaine -> tableau de tableaux
	let res = [];
	if (s[0] == "'") { // decode the length of a line
		si = fromb64(s[1])*64 + fromb64(s[2]);
		s = s.slice(3);
	} else {
		si = fromb64(s[0]);
		s = s.slice(1);
	}
	s = s.split("|");
	let customs_ = s[0];
	let customs = [];
	for (let i=0;i<customs_.length;i+=7) {
		customs[Math.floor(i/7) + 10] = customs_.slice(i, i+7);
	}
	s = s[1];
	let l = s.split("");
	let l1 = [];
	let i = 0;
	while (i < l.length) {
		if (l[i] == "'") {
			l1.push(fromb64(l[i+1])*64+fromb64(l[i+2]));
			i += 3
		} else if (l[i] == "$") {
			l1.push("$");
			i += 1
		} else if (l[i] == "&") {
			let sides = [];
			i ++;
			while (l[i] != "&") {
				let [d, c] = [fromb64(l[i]), fromb64(l[i+1])];
				if (typeof(l1[l1.length-1]) != "object") {
					l1[l1.length-1] = [l1[l1.length-1], d, c];
				} else {
					let co = l1[l1.length-1];
					here = false;
					for (let i=1;i<co.length;i+=2) {
						if (co[i]==d) {
							co[i+1] = c;
							here = true
						}
					}
					if (!here) {
						co.push(d, c);
					}
				}
				i += 2;
			}
			i++;
		} else {
			l1.push(fromb64(l[i]));
			i ++;
		}
	}
	let l2 = [];
	let n = 0;
	i = 0;
	while (i < l1.length) {
		if (l1[i] == "$") {
			for (let j = 0; j < l1[i+1]; j++) {
				res.push(dcopy(res.slice(-1)[0]));
			}
		} else {
			for (let j = 0; j < l1[i+1]; j++) {
				if (typeof(l1[i]) != "object") {
					if (l1[i] >= 10) {
						l1[i] = customs[l1[i]];
					}
				} else {
					for (let m=0;m<l1[i].length;m++) {
						if (l1[i][m] > 10) {
							l1[i][m] = customs[l1[i][m]];
						}
					}
				}
				l2.push(l1[i]);
				n += 1;
			}
		}
		i += 2;
		if (n == si) {
			n = 0;
			res.push(dcopy(l2));
			l2 = [];
		}
	}
	return res
}
var save = () => { // save settings & salles & groups to file
	let txt = Math.abs(z).toString()+"\n"+eta.toString()+"\n"+scroll[0].toString() + " " + scroll[1].toString() + "\n" + ((freeze)?"yes":"no") + "\n";
	for (let id of Object.keys(salles).sort()) { // to put them in the right order because groups mention IDs
		let s = salles[id];
		txt += id + "\n" + s.n + "\n" + s.f.join(" ") + "\n" + frg(s.g) + "\n"+ s.d + "\n" + JSON.stringify(s.c) + "\n" + (s.v?"yes":"no") + "\n";
	}
	txt += "\t\n";
	for (let id of Object.keys(groups).sort((a, b) => b-a)) {
		let g = groups[id];
		txt += id + "\n" + g.n + "\n" + Array.from(g.l).join(" ") + "\n";
	}
	let file = new Blob([txt]);
	if (fileurl) {
		window.URL.revokeObjectURL(fileurl);
	}
	fileurl = window.URL.createObjectURL(file);
	let a = document.createElement("A");
	a.href = fileurl;
	a.download = "DM-" + Math.floor(Date.now()/1000).toString() + ".txt";
	a.target = "_blank";
	document.body.appendChild(a);
	a.click();
	document.body.removeChild(a);
	show("File downloaded.");
}
var load = () => { // load settings & salles from file
	show(`Enter file: <input type="file" id="filegetter" accept="text/txt"/> <button id="filevalid" onclick="take()">Submit</button>`);
	take = () => { // take from filegetter
		salles = {};
		files = filegetter.files;
		file = files[0];
		reader = new FileReader();
		reader.onloadend = () => { filedone() };
		reader.readAsText(file);
		new Promise((y, n) => filedone = y).then(() => {
			let l = reader.result.trim().split("\n");
			setz("=Number("+l[0]+")");
			setl("=Number("+l[1]+")");
			sets("0", "=Number("+l[2].split(" ")[0]+")");
			sets("1", "=Number("+l[2].split(" ")[1]+")");
			(((freeze)?"yes":"no")==l[3])?undefined:freez();
			l = l.slice(4);
			while (l.length > 0 && l[0] != "\t") { // took \t because it can't be typed in input fields, so not a valid room/group name/whatever
				salles[l[0]] = { n:l[1], f:l[2].split(" ").map(Number), g:tog(l[3]),  d:l[4], c:JSON.parse(l[5]), v:(l[6]=="yes") };
				nextid = Number(l[0])+1;
				l = l.slice(7);
			};
			l = l.slice(1);
			while (l.length > 0) {
				groups[l[0]] = { n:l[1], l:new Set(l[2].split(" ")) };
				nextgroupid = Number(l[0])-1;
				l = l.slice(3);
			}
			rmlastline();
			show("Loaded successfully.");
			display();
		})
	}
}
var help = () => show(`Help page.

Commands available: 
` + 
`<ul><li>help/h : shows this message
list : lists the rooms
add/a : add a room
edit/e : edit a room
level [number] : go to a specific level
zoom [number] : go to a specific zoom
freeze/f : toggle whether the players see changes
goto : go to a room
open/o : open the player window
save/s : download the file
load/l : load a file
view: show a room to the players
hide : hide a room to the players
remove : remove a room
rotate : turn a room 90, 180, or 270°
set : create a set of rooms that can be used in commands
copy : create a new room that is a copy of another
invert : inver the brightness of the canvas
grid : toggle whether the grid always displays</li></ul>`.replaceAll("\n", "</li><li>") +
`Rooms can be chosen by entering their name, their ID (starting with a +), a set name, or the ID of a set (starting with a -).

Also some shortcuts when not typing:
` +
`<ul><li>the arrows : navigate in the map
+ and - : zoom more/less
pageup and pagedown : change floor</li></ul>`.replaceAll("\n", "</li><li>")
); // info message, maybe a way to automatize this
var listrooms = () => { // and groups
	if (nextid == 1) {
		var mess = "No rooms.";
	} else {
		var mess = "Rooms:\n<table class='out'><tr><td>Id</td><td>Name</td><td>Size</td><td>Level</td><td>Visible?</td><td>Description</td></tr>";
		for (let id of Object.keys(salles).sort()) {
			let s = salles[id];
			mess += "<tr><td>+" + [id, s.n, s.g.length.toString() + "x" + s.g[0].length.toString(), s.f.toString(), (s.v)?"Yes":"No", s.d].join("</td><td>") + "</td></tr>";
		}
	}
	show(mess);
	if (nextgroupid == -1) {
		mess = "No sets.";
	} else {
		mess = "Sets:\n<table class='out'<tr><td>Id</td><td>Name</td><td>Rooms</td></tr>";
		for (let id of Object.keys(groups).sort((a, b) => b-a)) {
			let g = groups[id];
			mess += "<tr><td>" + [id, g.n, Array.from(g.l).join(',')].join("</td><td>") + "</td></tr>";
		}
	}
	show(mess)
};
var selectroom = () => { // pick a room/group by clicking on it/entering it in the input
	display();
	show(`Selection: Enter the <input list="selelist" type="text" id="senainp" placeholder="name or ID"/><datalist id="selelist">` + Object.keys(salles).map((x) => `<option>` + salles[x].n + `</option>`) + Object.keys(groups).map((x) => "<option>" + groups[x].n + "</option>") + `</datalist> of a room or set and then <button onclick="let val = senainp.value; rmlastline(); for (id of Object.keys(salles)) { if (salles[id].n == val) { selected(id) } }; for (id of Object.keys(groups)) { if (groups[id].n == val) { selected(id) } }; if (val[0] == '+' && Number(val) < nextid && val[1] != '0') {selected(val)}; if (val[0] == '-' && Number(val) > nextgroupid && val[1] != '0') {selected(val)}">submit</button> or click on a room on the left.`);
	picking = true;
	new Promise ((yes, no) => { picked = yes })
	.then(([x, y]) => {
		picking = false;
		for (id in salles) {
			let s = salles[id];
			[i, j] = bl(s.c);
			if (i <= x && x < i + s.g.length && j <= y && y < j + s.g[0].length && s.f.includes(eta)) {
				rmlastline();
				selected(id);
				break
			}
		}
		selected(0);
	});
	return new Promise ((yes, no) => { selected = yes })
}
var place = () => {
	show(`First select a room to anchor on (or in the void for independent positioning).`);
	selectroom().then((anchor) => {
		rmlastline();
		show(`Anchored on: ` + ((anchor=="0")?"(0, 0)":salles[anchor].n) + `. Click where the top-left corner should go.`);
		picking = true;
		new Promise ((yes, no) => picked = yes)
		.then(([x, y]) => { 
			picking = false;
			let co = (anchor=="0")?[0, 0]:bl(salles[anchor].c);
			x -= co[0];
			y -= co[1];
			show("Room placed.");
			placed([anchor, x, y]);
		}) 
	});
	return new Promise ((yes, no) => {placed = yes})
}
var create = () => { // create the grid (coming from addroom's buttons)
	rmlastline();
	editgrid([]).then((grid) => {
		place().then((co) => {
		new Promise ((y, n) => {
			salles[nextid] = {n:tmp[0], d:tmp[1], f:tmp[2], v:false, g:grid, c:co};
			nextid++;
			y();
		}).then(() => {
		show("Room successfully created.");
		display();
		})})
	})
}
var copyroom = needsroom( (id) => {
	rmlastline();
	show(`<input type="text" placeholder="enter new name" id="nameinp"/> and then <button onclick="if (!(nameinp.value.includes('+') || nameinp.value.includes('-'))) { let val = nameinp.value; rmlastline(); copyto(val) } else { alert('+ and - are reserved for IDs.') }">submit</button> (or <button onclick="rmlastline();">cancel</button>).`);
	copyto = (dest) => { // dest is only the name
		salles[nextid] = structuredClone(salles[id]);
		salles[nextid].n = dest;
		ide = nextid
		nextid++;
		rmlastline();
		display();
		show("Room copied.");
		askmove(); // because also cloned the coordinates, so overlap.
	};
})
var rotatearray = (g) => { // 90° clockwise
	let res = new Array(g[0].length);
	for (let j=0;j<res.length;j++) {
		res[j] = new Array(g.length);
	}
	for (let i=0;i<res.length;i++) {
		for (let j=0;j<res[0].length;j++) {
			res[i][j] = g[j][res.length-1-i];
		}
	}
	return res
}
var rotate = needsroom( (id) => { // rotate a room, with the form and all
	quarterturn = () => {
		salles[id].g = rotatearray(salles[id].g);
		for (let i=0;i<salles[id].g.length;i++) {
			for (let j=0;j<salles[id].g[i].length;j++) {
				if (typeof(salles[id].g[i][j]) == "object") {
					for (let k=1;k<salles[id].g[i][j].length;k+=2) {
						salles[id].g[i][j][k] ++;
						salles[id].g[i][j][k] %= 4;
					}
				}
			}
		}
		display();
	}
	ssl(`<button onclick="quarterturn()">Rotate 90° clockwise</button>
<button onclick="rmlastline()">Validate</button>`)
})
var update = () => { // update grid/position (coming from editroom's buttons)
	rmlastline();
	let s = salles[ide];
	editgrid(s.g).then((grid) => { 
		salles[ide].g = grid;
		display();
		askmove();
	})
}
var askmove = () => show(`Move? <button onclick="rmlastline();roomfloor = salles[ide].f; salles[ide].f = []; place().then((co) => { salles[ide].c = co; salles[ide].f = roomfloor; endedit()})">Yes</button> <button onclick="endedit()">No</button>`); // ask and maybe move the room named ide
endedit = () => { // used by askmove's buttons
	rmlastline();
	show("Saved.");
	display();
}
var editgrid = (grid) => {
	oldmat = grid;
	colors = {}; // keys coordinates, values colors. Allows for unlimited size, scroll, zoom, etc.
	for (let i=0;i<grid.length;i++) {
		for (let j=0;j<grid[i].length;j++) {
			if (grid[i][j]) {
				colors[i+" "+j] = grid[i][j]; // get the colors already there
			}
		}
	}
	oldfreeze = freeze;
	if (!freeze) freez();
	oldscroll = dcopy(scroll);
	sets("0", "=0");
	sets("1", "=0");
	editrect = () => {
		picking = true;
		show("Waiting for first corner.");
		new Promise((y, n) => picked = y).then(([aa, bb]) => {
			rmlastline();
			show("Waiting for second corner.");
			new Promise((y, n) => picked = y).then(([cc, dd]) => {
				rmlastline();
				picking = false;
				for (let i=aa;i<=cc;i++) {
					for (let j=bb;j<=dd;j++) {
						colors[i+" "+j] = c;
						draw(i, j, cols(c));
					}
				}
				display();
				editrect();
			})
		})
	}
	editborder = () => {
		show("Waiting for tile");
		picking = true;
		new Promise((y, n) => {picked = y}).then(([x, y]) => {
			rmlastline();
			picking = false;
			bordering = true;
			show(`Select a border:<button onclick="borderchosen(0)">←</button><button onclick="borderchosen(1)">↑</button><button onclick="borderchosen(3)">↓</button><button onclick="borderchosen(2)">→</button>`);
			borderchosen = (d) => {
				bordering = false;
				rmlastline();
				if (colors[x+" "+y]) {
					let co = colors[x+" "+y]
					if (typeof(co) != "object") {
						colors[x+" "+y] = [co].concat(d, c);
					} else {
						here = false;
						for (let i=1;i<co.length;i+=2) {
							if (co[i]==d) {
								co[i+1] = c;
								here = true
							}
						}
						if (!here) {
							co.push(d, c);
						}
					}
					display();
				} else {
					alert("Cannot change border of empty tile.")
				}
			}
		})
	}
	ssl(`Editing mode.
The room is opened at the left.
Current color is:<span id="ccs" style="background-color:#`+cols(c)+`">&emsp;</span>
Press ` + [0, 1, 2, 3, 4, 5, 6].map((n) => n.toString() + " for " + cs(n)).join(", ") + `, or pick a custom color:<input type="color" id="colinp" style="height:1em;width:2em;border:none" onchange="c = colinp.value; ccs.style.backgroundColor = '#' + cols(c);"/>.
Modes: <button id="tbtn" onclick="picking=false">Single tiles</button><button id="rbtn" onclick="editrect()">Rectangles</button><button id="bbtn" onclick="editborder()">Edit a border</button>
For rectangles, click on the top left corner and then the bottom right one.
<button id="wbtn" onclick="let mat = constructmat(); done = true; rmlastline(); if (!oldfreeze) freez(); sets('0', '='+oldscroll[0].toString()); sets('1', '='+oldscroll[1].toString()); finished(mat)">Save</button><button id="xbtn" onclick="done = true; if (!oldfreeze) freez(); sets('0', '='+oldscroll[0].toString()); sets('1', '='+oldscroll[1].toString()); rmlastline(); finished(oldmat)">Discard changes</button>`);
	done = false;
	display();
	return new Promise ((yes, no) => {finished = yes})
}
var removeroom = needsroom( (id) => {
	let coord = bl(salles[id].c);
	delete salles[id];
	for (let ide of Object.keys(salles)) { // sort out the dependencies
		if (salles[ide].c[0] == id) {
			salles[ide].c = [0, coord[0]+salles[ide].c[1], coord[1]+salles[ide].c[2]]
		}
	}
	for (let ide of Object.keys(groups)) {
		groups[ide].l.delete(id);
	}
	show("Room deleted. Rooms depending on it are now independent and it has been removed from all sets.");
	display();
})
var constructmat = () => { // construct a cropped matrix from edigrid's object
	var [imin, jmin, imax, jmax] = [Infinity, Infinity, 0, 0];
	var tmp = [];
	for (p of Object.keys(colors)) {
		if (colors[p]) {
			let [i, j] = p.split(" ");
			imin = Math.min(imin, i);
			jmin = Math.min(jmin, j);
			imax = Math.max(imax, i);
			jmax = Math.max(jmax, j);
			tmp.push([i, j, colors[p]]);
		}
	}
	tmp = tmp.map((a) => [a[0] - imin, a[1] - jmin, a[2]]);
	let mat = Array(imax-imin+1).fill().map(() => Array(jmax-jmin+1).fill(0));
	tmp.forEach((a) => mat[a[0]][a[1]] = a[2])
	return mat
}
var addroom = () => {
	ssl(`Adding a room (lists must be separated by commas with no spaces):
<input type="text" id="nameinp" placeholder="name"/>
<input type="text" id="descinp" placeholder="description"/>
<input type="text" id="storinp" placeholder="level list"/>
<button onclick="if (!(nameinp.value.includes('+') || nameinp.value.includes('-'))) { tmp = [nameinp.value.trim(), escap(descinp.value.trim()), storinp.value.split(',').map(Number)]; create() } else { alert('+ and - are reserved for IDs.') }">Create grid</button> (or <button onclick="rmlastline();">cancel</button>)`);
}
var editroom = needsroom( (id) => {
	ide = id;
	let s = salles[id];
	ssl(`Editing: (lists must be separated by commas with no spaces):
<input type="text" id="nameinp" placeholder="name" value="`+s.n+`"/>
<input type="text" id="descinp" placeholder="description" value="`+s.d+`"/>
<input type="text" id="storinp" placeholder="level list" value="`+s.f.join(",")+`"/>
<button onclick="if (!(nameinp.value.includes('+') || nameinp.value.includes('-'))) { salles[ide].n = nameinp.value; salles[ide].d = escap(descinp.value.trim()), salles[ide].f = storinp.value.split(',').map(Number); update() } else { alert('+ and - are reserved for IDs.') } ">Next</button> (or <button onclick="rmlastline()">cancel</button>)`)
});
freez();
freez(); // do it twice to keep it false at the end of the day
setz("=50");
sets("0", "=0");
setl("=1"); // these are the default values, will be overriden by any load
display();
} else {
window.addEventListener("click", () => parenthtml.requestFullscreen()); // can't make it automatic because of some security thing or other
window.addEventListener("message", (e) => parsemessage(e.data));
let parsemessage = (text) => {
	if (text == "invert") {
		invert();
	} else {
		for (s of text.split("|").slice(0, -1)) {
			let [x, y, w, h, co] = JSON.parse(s);
			fr(x, y, w, h, co);
		}
	}
}
}
