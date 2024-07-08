var fileurl = null;
commands = {
	"list":"list", 
	"h":"help", "help":"help", 
	"add":"addent", "a":"addent",
	"edit":"editent", "e":"editent",
	"next":"next", "n":"next",
	"hp":"edhp",
	"save":"save", "s":"save",
	"load":"load", "l":"load",
	"r":"roll", "roll":"roll",
};
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
var inp = () => { // input
	log.innerHTML = log.innerHTML.replaceAll(/<\/?(butto|inpu).*?[^=]>/g, ""); // remove interface from last commands
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
var dcopy = (aa) => JSON.parse(JSON.stringify(aa)); // deepcopy of array array. worst part is it's apparently the best way
var save = () => { // save settings & salles & groups to file
	txt = JSON.stringify(ent);
	let file = new Blob([txt]);
	if (fileurl) {
		window.URL.revokeObjectURL(fileurl);
	}
	fileurl = window.URL.createObjectURL(file);
	let a = document.createElement("A");
	a.href = fileurl;
	a.download = "Mobs-" + Math.floor(Date.now()/1000).toString() + ".txt";
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
			ent = JSON.parse(l);
			rmlastline();
			show("Loaded successfully.");
		})
	}
}
var help = () => show(`Help page.

Commands available: 
` + 
`<ul><li>help/h : shows this message
list : list the entities
add/a : add an entity
edit/e : edit an entity
next/n : says which entity is next in initiative
hp : add a number to the HP of an entity
save/s : download the file
load/l : load a file
roll/r : roll initiative</li></ul>`.replaceAll("\n", "</li><li>") +
`Entities can be chosen by entering their ID.`
); // info message, maybe a way to automatize this
var ent = {};
var nextid = 1;
var list = () => {
	show("<table><tr><td>Id</td><td>Name</td><td>HP</td><td>Initiative</td><td>AC</td><td>Perception</td></tr>" +
	Object.keys(ent).map((x) => "<tr><td>" + [x, ent[x].n, ent[x].h, ent[x].i, ent[x].a, ent[x].p].join("</td><td>") + "</tr>" ).join("") + 
	"</table>");
}
var addent = () => {
	ssl(`<input id="nameinp" type="text" /> Name
<input id="hpinp" type="number" /> HP
<input id="acinp" type="number" /> AC
<input id="peinp" type="number" /> Perception
<input id="ininp" type="number" value="0"/> Initiative (optional)
<input id="nuinp" type="number" value="1" /> Number (optional)
<button onclick="addval()">Validate</button>`);
	addval = () => {
		for (let i=0;i<nuinp.value;i++) {
			ent[nextid++] = {n:nameinp.value, h:Number(hpinp.value), a:Number(acinp.value), p:Number(peinp.value), i:Number(ininp.value), d:false, f:false};
		}
		rmlastline();
		show("Entities successfully added.");
	}
}
var editent = (n) => {
	n = Number(n);
	ssl(`<input id="nameinp" type="text" value="`+ent[n].n+`"/> Name
<input id="hpinp" type="number" value="`+ent[n].h+`"/> HP
<input id="acinp" type="number" value="`+ent[n].a+`"/> AC
<input id="peinp" type="number" value="`+ent[n].p+`"/> Perception
<input id="ininp" type="number" value="`+ent[n].i+`"/> Initiative
<button onclick="editval()">Validate</button>`);
	editval = () => {
		ent[n].n = nameinp.value;
		ent[n].h = hpinp.value;
		ent[n].a = acinp.value;
		ent[n].p = peinp.value;
		rmlastline();
		show("Entity modified.");
	}
}
var next = () => {
	let mini = -1;
	let ini = -Infinity;
	let per = 0;
	for (let i of Object.keys(ent)) {
		if ((ent[i].i > ini || (ent[i].i == ini && ent[i].p > per) ) && !ent[i].d && !ent[i].f) {
						mini = i;
			ini = ent[i].i;
			per = ent[i].p;
		}
	}
	if (mini == -1) {
		for (let i of Object.keys(ent)) {
			ent[i].f = false;
		}
		next();
	} else {
		show("<table><tr><td>Id</td><td>Name</td><td>HP</td><td>Initiative</td><td>AC</td><td>Perception</td></tr><tr><td>" + 
		[mini, ent[mini].n, ent[mini].h, ent[mini].i, ent[mini].a, ent[mini].p].join("</td><td>") + "</tr></table>");
		ent[mini].f = true;
	}

}
var edhp = (n) => {
	n = Number(n);
	ssl(`<input id="hpinp" type="number" />HP variation (possibly negative)
<button onclick="hpval()">Validate</button>`);
	hpval = () => {
		ent[n].h += Number(hpinp.value);
		if (ent[n].h <= 0) {
			ent[n].d = true;
		}
		rmlastline();
	}
}
var roll = () => {
	for (let i of Object.keys(ent)) {
		ent[i].i = window.crypto.getRandomValues(new Uint32Array(1)) % 20 + 1 + ent[i].p;
	}
	list();
}
