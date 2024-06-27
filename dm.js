// #init2
var cname = "cookie-dm-dp=";
var ctx = canv.getContext("2d");
var scroll = [0, 0];
var z, scroll, eta, freeze
var fr = (x, y, w, h, co, vis = true) => {
	ctx.fillStyle = "#"+co;
	ctx.fillRect(x, y, w, h);
//	console.log(x, y, w, h, co);
	if (dm && !freeze && vis) {
		buffer += JSON.stringify([x, y, w, h, co]) + "|";
	}
};
if (dm) { //  #dm #event
canv.addEventListener("click", (e) => {
	let rect = canv.getBoundingClientRect();
	let x = e.clientX - rect.left;
	let y = e.clientY - rect.top;
	if (0 <= x && x < 900 && 0 <= y && y < 900) {
		x = Math.floor((x-scroll[0])/z);
		y = Math.floor((y-scroll[1])/z);
		if (picking) {
//			console.log("picked",  x, y);
			picked([x, y]);
		} else if (!done) {
			colors[x+" "+y] = c;
			draw(x, y, cols[c]);
		}
	}
})
addEventListener("keydown", (e) => {
	if (/^\d$/.test(e.key) && !done) {
		c = Number(e.key);
	} else if (e.key == "s" && !done) {
		done = true;
		rmlastline();
		finished(constructmat());
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
// #init
var salles = {};
var fileurl = null;
var done = true;
var picking = false;
var cols = ["000", "FFF", "7f7f7f", "F00", "FF0", "8B4513"];
var c = 1;
var buffer = "";
commands = {
	"list":"listrooms", 
	"h":"help", "help":"help", 
	"addroom":"addroom", "add":"addroom", 
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
};
var openp = () => { window.open("http://htmlpreview.github.io/?https://github.com/agoujot/code/blob/main/dp.html", "Players", "popup"); setTimeout(display, 500); };
var setCookie = (value) => {
	document.cookie = cname + value + "; path=/";
	return true;
}
// #graphic
var gotoroom = () => {
	selectroom().then((na) => {
		let co = bl(salles[na].c);
		sets("0", "="+(-co[0]*z).toString());
		sets("1", "="+(-co[1]*z).toString());
	})
}
var hide = () => {
	selectroom().then((na) => {
		salles[na].v = false;
		display();
		show(na + " was hidden to the players.");
	})
}
var view = () => {
	selectroom().then((na) => {
		salles[na].v = true;
		display();
		show(na + " was shown to the players.");
	})
}
var bl = ([s, x, y]) => 
	(s=="0")?[x, y]:[bl(salles[s].c)[0]+x, bl(salles[s].c)[1]+y]
var draw = (i, j, co, show=true) => fr(i*z+scroll[0], j*z+scroll[1], z, z, co, show);
var displayroom = (nom) => {
	let s = salles[nom];
	let coord = bl(s.c);
	for (i=0;i<s.g.length;i++) {
		for (j=0;j<s.g[i].length;j++) {
			if (s.g[i][j]) {draw(i+coord[0], j+coord[1], cols[s.g[i][j]], s.v)};
		}
	}
	let args = [nom, coord[0]*z+scroll[0], coord[1]*z+scroll[1]+12];
	ctx.font = "12px serif";
	ctx.fillStyle = "black";
	ctx.fillText(args[0], args[1], args[2]);
	ctx.fillStyle = "white";
	ctx.fillText(args[0], args[1]+2, args[2]+2);
}
var erase = () => fr(0, 0, 900,900, "000");
var display = () => {
	erase();
	if (done) {
		for (nom of Object.keys(salles)) {
			if (salles[nom].f.includes(eta)) {displayroom(nom)};
		};
	} else {
		for (p of Object.keys(colors)) {
			let [i, j] = p.split(" ").map(Number);
			if (colors[p]) {draw(i, j, cols[colors[p]])}
		}
	}
	flush();
}
var setz = (s) => {
	eval("z"+s);
	zoomspan.innerHTML = z.toString();
	display();
}
var setl = (s) => {
	eval("eta"+s);
	levelspan.innerHTML = eta.toString();
	display();
}
var sets = (a, b) => {
	eval("scroll["+a+"]"+b);
	scrollspan.innerHTML = scroll.map((x) => x.toString()).join(" ");
	display();
}
var flush = () => {setCookie(buffer); buffer = "" };
var freez = () => {
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
// #console
var escap = (s) => s
	.replaceAll('"', "&quot;")
	.replaceAll("'", "&apos;")
	.replaceAll("&", "&amp;")
	.replaceAll("<", "&lt;")
	.replaceAll(">", "&gt;");
var ssl = (s) => { // show single line
	show(s.replaceAll("\n", "<br>"));
}
rmlastline = () => {
	log.lastChild.remove();
}
var cs = (co) => '<span style="background-color:#' + cols[co] + '">&emsp;</span>'
var inp = () => {
	log.innerHTML = log.innerHTML.replaceAll(/<\/?(button|input).*?>/g, ""); // remove interface from last commands
	[picking, done] = [false, true];
	let l = input.value;
	write(l, "in");
	input.value = "";
	let com = Object.keys(commands).find((comm) => l.startsWith(comm+" ") || l == comm);
	let f = commands[com];
	if (f) {
		eval(f+"('" + l.slice(com.length).trim() + "')");
	}
	else write("Unknown command", "error");
};
input.addEventListener("keyup", (e) => { if (e.keyCode == "13" && !e.shiftKey) inp() });
var write = (s, clas) => log.innerHTML += ("<tr><td class='" + clas + "'>" + s.trim().split("\n").join("</td></tr><tr><td class='cont " + clas + "'>") + "</td></tr>");
commandlist.innerHTML = Object.keys(commands).map((x) => '<option>' + x + '</option>').join("");
var show = (s) => s.split("\n").map((l) => (l)?l:"&nbsp;").forEach((s) => write(s, "out"));
// #file
function tob64(n) { // en base 64, cad 0 .. 9 + A .. Z + a .. z + - + . (Assume, dans son implementation actuelle, que il y a moins de 2**12 couleurs.)
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
var dcopy = (aa) => JSON.parse(JSON.stringify(aa));
var frg = (aa) => { // tableau de tableau -> chaine
	let res = tob64(aa[0].length);
	let lt = -1;
	let ti = 0;
	for (let i = 0; i < aa.length; i++) {
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
			for (let j = 0; j < aa[0].length; j++) {
				k = aa[i][j];
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
var tog = (s) => { // chaine -> tableau de tableau (a besoin des parametres)
	let res = [];
	if (s[0] == "'") {
		si = fromb64(s[1])*64 + fromb64(s[2]);
		s = s.slice(3);
	} else {
		si = fromb64(s[0]);
		s = s.slice(1);
	}
	let l = s.split("");
	let l1 = [];
	let i = 0;
	while (i < l.length) {
		if (l[i] == "'") {
			l1.push(fromb64(l[i+1])*64+fromb64(l[i+2]));
			i += 3
		} if (l[i] == "$") {
			l1.push("$");
			i += 1
		} else {
			l1.push(fromb64(l[i]));
			i ++
		}
	}
	let l2 = [];
	let c = 0;
	i = 0;
	while (i < l1.length) {
		if (l1[i] == "$") {
			for (let j = 0; j < l1[i+1]; j++) {
				res.push(dcopy(res.slice(-1)[0]));
			}
		} else {
			for (let j = 0; j < l1[i+1]; j++) {
				l2.push(l1[i]);
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
var save = () => {
	let txt = Math.abs(z).toString()+"\n"+eta.toString()+"\n"+scroll[0].toString() + " " + scroll[1].toString() + "\n" + ((freeze)?"yes":"no") + "\n";
	for (let na in salles) {
		let s = salles[na];
		txt += na + "\n" + s.f.join(" ") + "\n" + frg(s.g) + "\n"+ s.d + "\n" + JSON.stringify(s.c) + "\n" + (s.v?"yes":"no") + "\n";
	}
	let file = new Blob([txt]);
	if (fileurl) {
		window.URL.revokeObjectURL(fileurl)
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
var load = () => {
	show(`Enter file: <input type="file" id="filegetter" accept="text/txt"/> <button id="filevalid" onclick="take()">Submit</button>`);
	take = () => {
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
			((freeze)?"yes":"no"==l[3])?undefined:freez();
			l = l.slice(4);
			while (l.length > 0) {
				salles[l[0]] = { f:l[1].split(" ").map(Number), g:tog(l[2]),  d:l[3], c:JSON.parse(l[4]), v:(l[5]=="yes") };
				l = l.slice(6);
			};
			rmlastline();
			show("Loaded successfully.");
			display();
		})
	}
}
// #help
var help = () => show(`
Help page:

Commands available: 
` + 
`<ul><li>help/h : shows this message
list : lists the rooms
add/a : add a room
edit/e : edit a room
level [number] : go to a specific level
zoom [number] : go to a specific zoom, minimum 1
freeze/f : toggle whether the players see changesf
goto : go to a room
open/o : open the player window
save/s : download the file
load/l : load a file
view: show a room to the players
hide : hide a room to the players
remove : remove a room
rotate : turn a room 90, 180, or 270°
copy : create a new room that is a copy of another</li></ul>`.replaceAll("\n", "</li><li>") +
`
Also some shortcuts when not typing:
` +
`<ul><li>the arrows : navigate in the map
+ and - : zoom more/less
pageup and pagedown : change floor</li></ul>`.replaceAll("\n", "</li><li>")
);
var listrooms = () => {
	let mess = "Rooms:\n<table class='out'><tr><td>Name&emsp;</td><td>Size&emsp;</td><td>Level&emsp;</td><td>Visible?</td></tr>";
	for (let nom in salles) {
		let s = salles[nom];
		mess += "<tr><td>" + [nom, s.g.length.toString() + "x" + s.g[0].length.toString(), s.f.toString(), (s.v)?"Yes":"No"].join("</td><td>") + "</td></tr>";
	}
	show(mess);
};
// #edit #add
var selectroom = () => {
	display();
	show(`Selection: <input list="selelist" type="text" id="senainp" placeholder="enter a name"/><datalist id="selelist">` + Object.keys(salles).map((x) => `<option>` + x + `</option>`) + `</datalist> and then <button onclick="if (Object.keys(salles).includes(senainp.value)) {let val = senainp.value; rmlastline(); selected(val)} else { alert('This room does not exist.') }">submit</button> or click on a room on the left.`);
	picking = true;
	new Promise ((yes, no) => { picked = yes })
	.then(([x, y]) => {
		picking = false;
		for (na in salles) {
			let s = salles[na];
			[i, j] = bl(s.c);
			if (i <= x && x < i + s.g.length && j <= y && y < j + s.g[0].length) {
				rmlastline();
				selected(na);
				break
			}
		}
		selected("0");
	});
	return new Promise ((yes, no) => { selected = yes })
}
var place = () => {
	show(`First select a room to anchor on (or in the void for independent positioning).`);
	selectroom().then((anchor) => {
		rmlastline();
		show(`Anchored on: ` + ((anchor=="0")?"(0, 0)":anchor) + `. Click where the top-left corner should go.`);
		picking = true;
		new Promise ((yes, no) => picked = yes)
		.then(([x, y]) => { 
			picking = false;
			let co = bl(salles[anchor].c);
			x -= co[0];
			y -= co[1];
			show("Room placed.");
			placed([anchor, x, y]);
		}) 
	});
	return new Promise ((yes, no) => {placed = yes})
}
var create = () => {
	rmlastline();
	editgrid([]).then((grid) => {
		place().then((co) => {
		new Promise ((y, n) => {
			salles[tmp[0]] = {d:tmp[1], f:tmp[2], v:false, g:grid, c:co};
			y();
		}).then(() => {
		roomform.innerHTML = "Room successfully created.";
		display();
		})})
	})
}
var copyroom = () => {
	show("Choose the room to copy.");
	selectroom().then((na) => {
		rmlastline();
		show(`<input type="text" placeholder="enter new name" id="nameinp"/> and then <button onclick="let val = nameinp.value; rmlastline(); copyto(val)">submit</button> (or <button onclick="rmlastline();">cancel</button>).`);
		copyto = (dest) => {
			salles[dest] = structuredClone(salles[na]);
			rmlastline();
			display();
			show("Room copied.");
			nam = dest;
			askmove(); // because also cloned the coordinates, so overlap.
		};
	})
}
var rotatearray = (g) => {
	let res = [];
	for (let j=0;j<g[0].length;j++) {
		let line = [];
		for (let i=g.length-1;i>-1;i--) {
			line.push(g[i][j]);
		}
		res.push(line);
	}
	return res
}
var rotate = () => {
	selectroom().then((na) => {
		quarterturn = () => {
			salles[na].g = rotatearray(salles[na].g);
			display();
		}
		ssl(`<button onclick="quarterturn()">Rotate 90° clockwise</button>
<button onclick="rmlastline()">Validate</button>`)
	})
}
var update = () => {
	rmlastline();
	let s = salles[nam];
	editgrid(s.g).then((grid) => { 
		salles[nam].g = grid;
		askmove();
	})
}
var askmove = () => show(`Move? <button onclick="roomfloor = salles[nam].f; salles[nam].f = []; place().then((co) => { rmlastline(); salles[nam].c = co; salles[nam].f = roomfloor;  endedit()})">Yes</button> <button onclick="endedit()">No</button>`);
endedit = () => {
	rmlastline();
	show("Saved.");
	display();
}
var editgrid = (grid) => {
	colors = {};
	for (i=0;i<grid.length;i++) {
		for (j=0;j<grid[i].length;j++) {
			if (grid[i][j]) { 
				colors[i+" "+j] = grid[i][j];
			}
		}
	}
	ssl(`Editing mode.
The room is opened at the left.
Current drawing color is: ` + cs(c) + `
Press ` + [0, 1, 2, 3, 4, 5].map((n) => n.toString() + " for " + cs(n)).join(", ") + `
Press s when you are satisfied.`);
	done = false;
	display();
	return new Promise ((yes, no) => {finished = yes})
}
var removeroom = () => {
	selectroom().then((na) => {
		let coord = bl(salles[na].c);
		delete salles[na];
		for (let nam of Object.keys(salles)) {
			if (salles[nam].c[0] == na) {
				salles[nam].c = ["0", coord[0]+salles[nam].c[1], coord[1]+salles[nam].c[2]]
			}
		}
		show("Room deleted. Rooms depending on it are now independent.");
		display();
	})
}
var constructmat = () => {
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
<button onclick="tmp = [nameinp.value.trim(), escap(descinp.value.trim()), storinp.value.split(',').map(Number)]; create()">Create grid</button> (or <button onclick="rmlastline();">cancel</button>)`);
}
var editroom = () => {
	selectroom().then((na) => {
	nam = na;
	let s = salles[na];
	ssl(`Editing `+na+` (lists must be separated by commas with no spaces):
<input type="text" id="descinp" placeholder="description" value="`+s.d+`"/>
<input type="text" id="storinp" placeholder="level list" value="`+s.f.join(",")+`"/>
<button onclick="salles[nam].d = escap(descinp.value.trim()), salles[nam].f = storinp.value.split(',').map(Number); update()">Next</button> (or <button onclick="rmlastline()">cancel</button>)`)
	}); 
}
freez();
freez();
setz("=50");
sets("0", "=0");
setl("=1");
display();
} else { // #dp
clearcookie();
var getCookie = () => {
	let res = "";
	let ca = document.cookie.split(";");
	for (c of ca) {
		if (c.indexOf(cname) == 0) {
			let val = c.slice(cname.length)
			if (val.length > 0) { 
				res += val
			}
		}
	}
	if (res) clearcookie(); // defined in dp.html because vim syntax coloring doesn't like it.
	return res
}
let updateMessage = () => {
	let text = getCookie();
	if (text) {
		console.log(text);
		for (s of text.split("|").slice(0, -1)) {
			let [x, y, w, h, co] = JSON.parse(s);
			fr(x, y, w, h, co);
		}
	}
	setTimeout(updateMessage, 100);
}
updateMessage();
}
