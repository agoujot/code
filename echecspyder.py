import copy
b = [["R","N", "B", "Q", "K", "B", "N", "R"], ["P", "P", "P", "P", "P", "P", "P", "P"], [".", ".", ".", ".", ".", ".", ".", "."], [".", ".", ".", ".", ".", ".", ".", "."], [".", ".", ".", ".", ".", ".", ".", "."], [".", ".", ".", ".", ".", ".", ".", "."], ["p", "p", "p", "p", "p", "p", "p", "p"],  ["r","n", "b", "q", "k", "b", "n", "r"]]
nt = [[2, 1], [2, -1], [1, -2], [-1, -2], [-2, -1], [-2, 1], [-1, 2], [1, 2]]
p = [[1, 0], [1, 1], [1, 2], [1, 3], [1, 4], [1, 5], [1, 6], [1, 7], [6, 0], [6, 1], [6, 2], [6, 3], [6, 4], [6, 5], [6, 6], [6, 7]]
r = [[0, 0], [0, 4],  [0, 7], [7, 0], [7, 4], [7, 7]]
im = {"p":[[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0],[0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0]], "r":[[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0], [0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0]], "n":[[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0], [0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0], [0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0]], "b":[[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0]], "k":[[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0], [0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0], [0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0]], "q":[[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0], [0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0], [ 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0]]}
k = {10:[0, 4], 20:[7, 4]}
tour = 10
fin = False
row = 0
der = {}
def afficher():
	print("       A           B           C           D           E           F           G           H       \n")
	for i in range(7, -1, -1):
		for z in range(6):
			if z != 3:
				print("  ", end = "")
			else:
				print(i+1, end = " ")
			for j in range(8):
				if b[i][j] == ".":
					if (i+j) % 2 == 0:
						print("XXXXXXXXXXXX", end = "")
					else:
						print("%%%%%%%%%%%%", end = "")
				else:
					for l in im[b[i][j].lower()][z]:
						if l == 0:
							if (i+j) % 2 == 0:
								print("X", end = "")
							else:
								print("%", end = "")
						else:
							if 64 < ord(b[i][j]) < 91:
								print("█", end = "")
							else:
								print(" ", end = "")
			if z != 2:
				print("  ", end = "")
			else:
				print(" " + str(i+1), end = "")
			print()
	print("\n       A           B           C           D           E           F           G           H       ")
def move(xd, yd, xe, ye):
	if [xe, ye] in p:
		p.remove([xe, ye])
	b[xe][ye] = b[xd][yd]
	b[xd][yd] = "."
def ltc(xd, yd, xe, ye, tour, v):
	global b
	b_ = copy.deepcopy(b)
	move(xd, yd, xe, ye)
	if check(k[tour][0], k[tour][1], (20 if tour == 10 else 10), True) == 0:
		if v:
			res = 1
		else:
			b = copy.deepcopy(b_)
			res = 2
	else:
		b = copy.deepcopy(b_)
		if v:
			res = 3
		else:
			res = 4
	return(res)
def legal(xd, yd, xe, ye, tour, v):
	global b
	if tour == 10:
		a = [96, 123]
	else:
		a = [64, 91]
	if v:
		for bbb in p:
			if bbb[0] == tour:
				p.remove(bbb)
	if not (a[0] < ord(b[xd][yd]) < a[1] or b[xd][yd] == ".") and (a[0] < ord(b[xe][ye]) < a[1] or b[xe][ye] == "."):
		if b[xd][yd] == "p" or b[xd][yd] == "P":
			if tour == 10:
				d = 1
			else:
				d = -1
			if xd + d == xe:
				if yd == ye and b[xe][ye] == ".":
					myl = ltc(xd, yd, xe, ye, tour, v)
					if myl == 1:
						if [xd, yd] in p:
							p.remove([xd, yd])
						return
					elif myl == 2:
						return(True)
					elif myl == 4:
						return(False)
				elif yd-2 < ye < yd+2:
					if a[0] < ord(b[xe][ye]) < a[1]:
						myl = ltc(xd, yd, xe, ye, tour, v)
						if myl == 1:
							if [xd, yd] in p:
								p.remove([xd, yd])
							return
						elif myl == 2:
							return(True)
						elif myl == 4:
							return(False)
					elif [(10 if tour == 20 else 20), xd, ye] in p and b[xe][ye] == ".":
						myl = ltc(xd, yd, xe, ye, tour, v)
						if myl == 1:
							if [xd, yd] in p:
								p.remove([xd, yd])
							move(xe, ye, xd, ye)
							return
						elif myl == 2:
							return(True)
						elif myl == 4:
							return(False)
			elif xd + 2*d == xe and yd == ye and [xd, yd] in p and b[xd+d][yd] == "." and b[xe][ye] == ".":
				myl = ltc(xd, yd, xe, ye, tour, v)
				if myl == 1:
					p.remove([xd, yd])
					p.append([tour, xe, ye])
					return
				elif myl == 2:
					return(True)
				elif myl == 4:
					return(False)
		elif b[xd][yd] == "R" or b[xd][yd] == "r":
			ok = True
			if xd == xe:
				if yd < ye:
					d = 1
				elif ye < yd:
					d = -1
				for c in range(yd+d, ye, d):
					if b[xd][c] != ".":
						ok = False
			elif yd == ye:
				if xd < xe:
					d = 1
				elif xe < xd:
					d = -1
				for c in range(xd+d, xe, d):
					if b[c][yd] != ".":
						ok = False
			else:
				ok = False
			if ok:
				myl = ltc(xd, yd, xe, ye, tour, v)
				if myl == 1:
					if [xd, yd] in r:
						r.remove([xd, yd])
					return
				elif myl == 2:
					return(True)
				elif myl == 4:
					return(False)
		elif b[xd][yd] == "N" or b[xd][yd] == "n":
			for c in nt:
				if xe == xd + c[0] and ye == yd + c[1]:
					myl = ltc(xd, yd, xe, ye, tour, v)
					if myl == 1:
						return
					elif myl == 2:
						return(True)
					elif myl == 4:
						return(False)
		elif b[xd][yd] == "B" or b[xd][yd] == "b":
			if abs(xe-xd) == abs(ye-yd):
				ok = True
				d = []
				if xe > xd:
					d.append(1)
				elif xd > xe:
					d.append(-1)
				else:
					ok = False
				if ye > yd:
					d.append(1)
				elif yd > ye:
					d.append(-1)
				else:
					ok = False
				e = yd + d[1]
				if ok:
					for c in range(xd+d[0], xe, d[0]):
						if b[c][e] != ".":
							ok = False
						e+=d[1]
				if ok:
					myl = ltc(xd, yd, xe, ye, tour, v)
					if myl == 1:
						return
					elif myl == 2:
						return(True)
					elif myl == 4:
						return(False)
		elif b[xd][yd] == "Q" or b[xd][yd] == "q":
			ok = True
			if abs(xe-xd) == abs(ye-yd):
				d = []
				if xe > xd:
					d.append(1)
				elif xd > xe:
					d.append(-1)
				else:
					ok = False
				if ye > yd:
					d.append(1)
				elif yd > ye:
					d.append(-1)
				else:
					ok = False
				if ok:
					e = yd + d[1]
					for c in range(xd+d[0], xe, d[0]):
						if b[c][e] != ".":
							ok = False
						e+=d[1]
			elif xd == xe:
				if yd < ye:
					d = 1
				elif ye < yd:
					d = -1
				for c in range(yd+d, ye, d):
					if b[xd][c] != ".":
						ok = False
			elif yd == ye:
				if xd < xe:
					d = 1
				elif xe < xd:
					d = -1
				for c in range(xd+d, xe, d):
					if b[c][yd] != ".":
						ok = False
			else:
				ok = False
			if ok:
				myl = ltc(xd, yd, xe, ye, tour, v)
				if myl == 1:
					return
				elif myl == 2:
					return(True)
				elif myl == 4:
					return(False)
		elif b[xd][yd] == "K" or b[xd][yd] == "k":
			if xd-2 < xe < xd+2 and yd-2 < ye < yd+2:
				b_ = copy.deepcopy(b)
				move(xd, yd, xe, ye)
				if check(xe, ye, (20 if tour == 10 else 10), True) == 0:
					if v:
						k[tour] = [xe, ye]
						if [xd, yd] in r:
							r.remove([xd, yd])
						return
					else:
						b = copy.deepcopy(b_)
						return(True)
				else:
					b = copy.deepcopy(b_)
					if not v:
						return(False)
			elif xd == xe and abs(yd-ye) == 2 and [xd, yd] in r and ([xd, 0]if ye < yd else[xd, 7]) in r:
				ok = True
				if ye < yd:
					for c in range(1, 3):
						if b[xd][c] != "." or check(xd, c, (20 if tour == 10 else 10), True) == 1:
							ok = False
				if yd < ye:
					for c in range(4, 7):
						if b[xd][c] != "." or check(xd, c, (20 if tour == 10 else 10), True) == 1:
							ok = False
				if ok:
					if check(xe, ye, (20 if tour == 10 else 10), True) == 0:
						b_ = copy.deepcopy(b)
						move(xd, yd, xe, ye)
						if v:
							move(xd, (0 if ye < yd else 7), xd, (ye+1 if ye < yd else ye-1))
							r.remove([xd, yd])
							k[tour] = [xe, ye]
							return
						else:
							b = copy.deepcopy(b_)
							return(True)
					else:
						b = copy.deepcopy(b_)
						if not v:
							return(False)
	if v:
		return("interdit")
	else:
		return(False)
def check(xr, yr, tour, rep):
	c = 0
	for i in range(8):
		for j in range(8):
			if legal(i, j, xr, yr, tour, False):
				c = 1
	if c == 1 and not rep:
		for vx in range(-1, 2):
			for vy in range(-1, 2):
				if not (vy == 0 and vx == 0):
					xf, yf = xr+vx, yr+vy
					if -1 < xf < 8 and -1 < yf < 8:
						if legal(xr, yr, xf, yf, (20 if tour == 10 else 10), False):
							c+=1
		if c == 1:
			g = False
			for i in range(8):
				for j in range(8):
					for x in range(8):
						for y in range(8):
							if legal(i, j, x, y, (20 if tour == 10 else 10), False):
								c = 2
								g = True
								break
							if g:
								break
						if g:
							break
					if g:
						break
				if g:
					break
	if c == 0:
		return(0)
	elif c == 1:
		return(2)
	elif c > 1:
		return(1)
while not fin:
	afficher()
	t = input("Tour des Blancs. Entrez votre coup. "if tour == 10 else"Tour des Noirs. Entrez vore coup.")
	m = [int(t[1])-1, ord(t[0])-65, int(t[3])-1, ord(t[2])-65]
	if b[m[0]][m[1]] != "p" and b[m[0]][m[1]] != "P" and b[m[2]][m[3]] == ".":
		row += 1
	else:
		row == 0
	if row == 50:
		print("Match nul par cinquante coups.")
		afficher()
		fin = True
	l = legal(m[0], m[1], m[2], m[3], tour, True)
	while l == "interdit":
		t = input("Coup interdit. Entrez-en un autre. ")
		m = [int(t[1])-1, ord(t[0])-65, int(t[3])-1, ord(t[2])-65]
		l = legal(m[0], m[1], m[2], m[3], tour, True)
	for i in range(8):
		for j in range(2):
			if j == 0:
				l = 0
			else:
				l = 7
		if b[l][i] == ("p" if l == 0 else "P"):
			o = input("Promotion du pion en" + chr(i+65)+str(l+1)+". Entrez le nom de la pièce que vous voulez.").lower()
			if o == "reine":
				b[l][i] = ("q" if l == 0 else "Q")
			elif o == "fou":
				b[l][i] = ("b" if l == 0 else "B")
			elif o == "tour":
				b[l][i] = ("r" if l == 0 else "R")
			elif o == "cavalier":
				b[l][i] = ("n" if l == 0 else "N")
	kk = k[(20 if tour == 10 else 10)]
	ch = check(kk[0], kk[1], tour, False)
	if ch == 1:
		print("Noirs en échec." if tour == 10 else "Blancs en échec.")
	elif ch == 2:
		afficher()
		print("Echec et mat, les Blancs gagnent."if tour else"Echec et mat, les Noirs gagnent.")
		fin = True
	b_ = ""
	for x in range(8):
		for y in range(8):
			b_+=b[x][y]
	if b_ in der.keys():
		der[b_] += 1
		if der[b_] == 3:
			print("Match nul par triple répétition.")
			afficher()
			fin = True
	else:
		der[b_] = 1
	dead = True
	for i in range(8):
		for j in range(8):
			if b[i][j] != "." and b[i][j] != "K" and b[i][j] != "k":
				dead = False
	if dead:
		print("Match nul par manque de pièces.")
		afficher()
		fin = True
	pat = True
	g = False
	for i in range(8):
		for j in range(8):
			for x in range(8):
				for y in range(8):
					if legal(i, j, x, y, (20 if tour == 10 else 10), False):
						pat = False
						g = True
					if g:
						break
				if g:
					break
			if g:
				break
		if g:
			break
	if pat and check(k[(20 if tour == 10 else 10)][0], k[(20 if tour == 10 else 10)][1], tour, True) == 0:
		print("Match nul par pat.")
		afficher()
		fin = True
	if tour == 10:
		tour = 20
	else:
		tour = 10