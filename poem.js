em = "\u2014";
layers = [{ // multiple layers of OCR errors because some need to be checked for before others, for example ]3 before ].
	" 1 ":		" I ",
	"$":		"S",
	"]3":		"B", 
	"]B":		"B", 
	"I3":		"B", 
	"0 ":		"O ", 
	"I-I":		"H", 
	"/t":		"H", 
	"iug":		 "ing", 
	"5V":		"W", 
	"Sd":		"èd", 
	"5d":		"èd", 
	"I)":		"D", 
	"liim":		"Him", 
	"liis":		"His", 
	"lng":		"ing", 
	"5I":		"M", 
	"tile":		"the", 
	"xv":		"w",  
	"\u007fv":	"w", 
	";v":		"w", 
	",v":		"w", 
	"sv":		"w",
	"XV":		"W", 
	"\u007fV":	"W", 
	"Xv":		"W", 
	",V":		"W", 
	";V":		"W", 
	"tnv":		"ow",
	"mv":		"ow",
"\n\u007fnd":	"\nAnd", 
	"])":		"D", 
	"]D": 		"D", 
	"]N":		"N", 
	"/I":		"A",
	".A":		"A"}, {
	"\u007f "	:", ", 
	"]":		"!", 
	"%":		"e,", 
	"“":		'"',
	"”":		'"',
	"‘":		"'",
	"’":		"'",
	" --- ":	em,
	"--- ":		em, 
	" ---":		em, 
	" -- ":		em }, { 
	"-- ":		em, 
	" --":		em, 
	" - ":		em }, {
	" -":		em, 
	"- ":		em, 
	"---"		:em}, {
	"--":		em, 
	"\u007f":	"", 
	" !":		"!", 
	" ?":		"?", 
	" ;":		";"
	}]
function opsplit(s, o) {
	r = [];
	i = 0;
	par = 0; 
	for (let j = 0; j < s.length; j++) {
		if (s.charAt(j) == "(") {
			par += 1
		} else if (s.charAt(j) == ")") {
			par -= 1
		} if (par == 0 && s.charAt(j) == o) {
			r.push(s.slice(i, j));
			i = j+1
		}
	}
	r.push(s.slice(i, s.length));
	return r
}
function op(s, o) {
	par = 0;
	for (let i = 0; i < s.length; i++) {
		c = s.charAt(i);
		if (c == "(") {
			par += 1
		} else if (c == ")") {
			par -= 1
		} if (par == 0 && c == o) {
			return true
		}
	}
	return false
}
function concat(ll) {
	r = "";
	for (l of ll) {
		if (typeof(l) == "string") {
			r += l
		} else {
			r += concat(l)
		}
	}
	return r
}
multiplyArray = (arr, length) => 
  Array.from({ length }, () => arr).flat()
function pr(s) {
	if (s.charAt(0) == "(" && s.slice(-1) == ")") {
		return pr(s.slice(1, -1))
	} else if (op(s, "+")) {
		return concat(opsplit(s, "+").map(pr))
	} else if (op(s, "*")) {
		l = opsplit(s,  "*");
		return multiplyArray(pr(l.slice(1).join("*")), Number(l[0]))
	} else {
		return s.split("")
	}
}
rhyme = ["0"];
textin.onchange = () => {
	l = textin.value;
	a = l;
	for (di of layers) {
		a = "";
		ci = 0;
		while (ci < l.length) {
			found = false;
			for (key of Object.keys(di)) {
				if (l.slice(ci, ci+key.length) == key) {
					a += di[key];
					ci += key.length;
					console.log("'"+key + "' -> '"+di[key]+"'");
					found = true;
					break
				}
			}
			if (!found) {
				a += l[ci];
				ci += 1
			}
		}
		l = a;
	}
	t = [];
	l = l.split("\n");
	ri = 0;
	for (ll of l) {
		if (ll.slice(0, 3) == "=r ") {
			rhyme = pr(ll.slice(3));
		} else if (ll.slice (0, 3) == "=i ") {
			ri = Number(ll.slice(3));
		} else if (ll != "") {
			t.push(ll)
		}
	}
	res = "";
	li = 0;
	while (li < t.length) {
		if (rhyme[ri] == "_") {
			res += "\n"
		} else {
			res += ":".repeat(Number(rhyme[ri]))+t[li]+"\n";
			li += 1
		}
		ri = (ri+1) % rhyme.length;
	}
	textin.value = "";
	navigator.clipboard.writeText(res);
}
