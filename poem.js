em = "\u2014";
err = [
	"“","\"", 
	"”", "\"",
	" ;",";",
	" !","!",
	" ?", "?",
	"’","'",
	" ;", ";",
	"‘","'",
	"–","—",
	"―","—",
	" — ", "—",
	"— ", "—",
	" —", "—",
	" :",":",
	"  ", " "
]
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
	r = [];
	for (l of ll) {
		r = r.concat(l);
	}
	return r
}
multiplyArray = (arr, length) => 
  Array.from({ length }, () => arr).flat()
function pr(s) {
	if (s.charAt(0) == "(" && s.slice(-1) == ")") {
		return pr(s.slice(1, -1))
	} else if (op(s, ",")) {
		return concat(opsplit(s, ",").map(pr))
	} else if (op(s, "*")) {
		l = opsplit(s,  "*");
		return multiplyArray(pr(l.slice(1).join("*")), Number(l[0]))
	} else if (s.charAt(0) == "'") {
		return [s.slice(1)]
	} else if (s.length > 1) {
		return [s.charAt(0)].concat(pr(s.slice(1)))
	} else {
		return [s]
	}
}
function clean(s) {
	for (i = 0; i < err.length; i+=2) {
		s = s.replaceAll(err[i], err[i+1]);
	}
	return s
}
rhyme = ["0"];
textin.onchange = () => {
	l = textin.value;
	l = l.split("\n");
	l = l.map(clean);
	t = [];
	ri = 0;
	for (ll of l) {
		if (ll.slice(0, 3) == "=r ") {
			rhyme = pr(ll.slice(3));
			console.log(ll, rhyme);
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
