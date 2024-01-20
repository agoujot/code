# the game of chess with complete rules
import copy
b = [["R","N", "B", "Q", "K", "B", "N", "R"], ["P", "P", "P", "P", "P", "P", "P", "P"], [".", ".", ".", ".", ".", ".", ".", "."], [".", ".", ".", ".", ".", ".", ".", "."], [".", ".", ".", ".", ".", ".", ".", "."], [".", ".", ".", ".", ".", ".", ".", "."], ["p", "p", "p", "p", "p", "p", "p", "p"],  ["r","n", "b", "q", "k", "b", "n", "r"]] # initiaise le plateau aux positions de départ
nt = [[2, 1], [2, -1], [1, -2], [-1, -2], [-2, -1], [-2, 1], [-1, 2], [1, 2]] # les mouvements du cavalier
p = [[1, 0], [1, 1], [1, 2], [1, 3], [1, 4], [1, 5], [1, 6], [1, 7], [6, 0], [6, 1], [6, 2], [6, 3], [6, 4], [6, 5], [6, 6], [6, 7]] # les pions pouvant encore avancer de 2
r = [[0, 0], [0, 4],  [0, 7], [7, 0], [7, 4], [7, 7]] # les tours et les rois pour le roque
im = {"p":[[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0],[0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0]], "r":[[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0], [0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0]], "n":[[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0], [0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0], [0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0]], "b":[[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0]], "k":[[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0], [0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0], [0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0]], "q":[[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0], [0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0], [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0], [ 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0]]} # les motifs pour l'affichage
k = {10:[0, 4], 20:[7, 4]} # les rois pour l'echec
tour = 10 # 10 pour les blancs et 20 pour les noirs au lieu de 1 et 0 pour éviter une confusion l.80 dans la prise en passant
fin = False 
row = 0 # pour les cinquantes coups
der = {} # pour la triple repetition
val = {"p":1, "r":5, "b":3, "n":3, "q":9, "k":99999999} # pour plus tard(ppt)
def afficher():
	"""affiche le plateau"""
	print("       A           B           C           D           E           F           G           H       \n") # les numéros des colonnes en haut
	for i in range(7, -1, -1): 
		for z in range(6): # == pour chaque ligne du tableau et pour chaque ligne des motifs de cette ligne, la range de i est pck le plateau est à l'envers dans le système, on l'inverse à l'affichage
			if z != 3:
				print("  ", end = "")
			else:
				print(i+1, end = " ") # les numéros des lignes à gauche
			for j in range(8):
				if b[i][j] == ".":
					if (i+j) % 2 == 0:
						print("░░░░░░░░░░░░", end = "")
					else:
						print("▒▒▒▒▒▒▒▒▒▒▒▒", end = "") # le motif de la case vide est le caractere de fond
				else:
					for l in im[b[i][j].lower()][z]:
						if l == 0:
							if (i+j) % 2 == 0:
								print("░", end = "")
							else:
								print("▒", end = "") # quand il y a un 0 dans le motif, c'est qu'il faut aficher le caractère de fond
						else:
							if 64 < ord(b[i][j]) < 91:
								print("█", end = "")
							else:
								print(" ", end = "") # quand il y a un 1 dans le motif, c'est qu'il faut afficher le caractère de pièce
			if z != 2:
				print("  ", end = "")
			else:
				print(" " + str(i+1), end = "") # les lignes à droite
			print()
	print("\n       A           B           C           D           E           F           G           H       ") # les colonnes en bas
def move(xd, yd, xe, ye):
	"""déplace vraiment les pièces dans le tableau"""
	if [xe, ye] in p:
		p.remove([xe, ye]) # un pion mangé ne peut plus avancer de 2
	b[xe][ye] = b[xd][yd] # on remplace la case de fin par la pièce que l'on bouge
	b[xd][yd] = "." # et on laisse une case vide derrière
def ltc(xd, yd, xe, ye, tour, v): # pour v, voir legal
	"""vérifier si un coup met en échec (et est donc illegal)"""
	global b
	b_ = copy.deepcopy(b)
	move(xd, yd, xe, ye)
	if check(k[tour][0], k[tour][1], (20 if tour == 10 else 10), True) == 0: #si ne mène pas à l'échec
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
			res = 4 # si pas v ou mène à un échec on annule le coup
	return(res) # une valeur pour indiquer le résultat
def legal(xd, yd, xe, ye, tour, v):
	"""retourne si un coup est légal""" # avec xd, yd les co de départ, xe, ye les co de fin, tour le tour, et v qui dit si on veut vraiment le faire ou si c'est juste pour tester
	global b
	if tour == 10:
		a = [96, 123] 
	else:
		a = [64, 91] # les ord des pièces ennemies
	if v:
		for bbb in p:
			if bbb[0] == tour:
				p.remove(bbb) # on enleve de p les pions qui ont avance de 2 il y a 2 tours, car on ne peut plus les prendre en passant
	if not (a[0] < ord(b[xd][yd]) < a[1] or b[xd][yd] == ".") and (a[0] < ord(b[xe][ye]) < a[1] or b[xe][ye] == "."): # si la case de fin est vide ou ennemie
		if b[xd][yd] == "p" or b[xd][yd] == "P": # si la case de depart est un pion
			if tour == 10:
				d = 1
			else:
				d = -1 # une case, car les 10 et les 20 n'avancent pas dans le même sens
			if xd + d == xe: # si le pion avance d'une case
				if yd == ye and b[xe][ye] == ".": # si c'est dans la même colonne et la case de fin(cf) est vide
					myl = ltc(xd, yd, xe, ye, tour, v) # on appelle ltc pour vérifier si ça mène à un échec
					if myl == 1: # pour les résultats de myl, voir ltc
						if [xd, yd] in p:
							p.remove([xd, yd]) # un pion qui avance d'une case ne peut plus être pris en passant
						return # pour ne pas retomber sur le return False tout à la fin. Info, au cas ou: l est avec ceci égal à None != "interdit" (peut être bon à savoir)
					elif myl == 2: # c'est bon donc on return True
						return(True)
					elif myl == 4:
						return(False) # sinon False
				elif yd-2 < ye < yd+2: # si on va une case en diagonale
					if a[0] < ord(b[xe][ye]) < a[1]: # si cf est ennemi car le pion ne va en diagonale que pour manger
						myl = ltc(xd, yd, xe, ye, tour, v)
						if myl == 1:
							if [xd, yd] in p:
								p.remove([xd, yd])
							return
						elif myl == 2:
							return(True)
						elif myl == 4:
							return(False) # comme au-dessus
					elif [(10 if tour == 20 else 20), xd, ye] in p and b[xe][ye] == ".": # la prise en passant: si cf est vide et en passant il y a un pion qui vient d'avancer de 2
						myl = ltc(xd, yd, xe, ye, tour, v)
						if myl == 1:
							b[xd][ye] = "." # pour manger en passant sans y aller
							if [xd, yd] in p:
								p.remove([xd, yd])
							return
						elif myl == 2:
							return(True)
						elif myl == 4:
							return(False) # bla bla je commenterai plus myl 
			elif xd + 2*d == xe and yd == ye and [xd, yd] in p and b[xd+d][yd] == "." and b[xe][ye] == ".": # sinon, si on avance de 2 cases (et donc que on change pas de colonne et les deux cases sont vides)
				myl = ltc(xd, yd, xe, ye, tour, v)
				if myl == 1:
					p.remove([xd, yd]) # on enleve ce pion de p
					p.append([tour, xe, ye]) # mais on le rajoute avec le tour auquel il a avancé devant, pour la prise en passant. c'est pour ça que tour est 10 ou 20 car si c'etait 0 ou 1, les pions à la 2eme colonne disparaitraient instant de p à la ligne 80 
					return
				elif myl == 2:
					return(True)
				elif myl == 4:
					return(False)
		elif b[xd][yd] == "R" or b[xd][yd] == "r": # si c'est une tour (j'utilise les abréviations anglaises)
			ok = True # si les cases sur le chemin sont vides (il faut vérifier avec plusieurs cas):
			if xd == xe: # pour aller a l'horizontale: 
				if yd < ye: # si on va a droite
					d = 1
				elif ye < yd: # et à gauche
					d = -1
				for c in range(yd+d, ye, d):
					if b[xd][c] != ".":
						ok = False 
			elif yd == ye: # à la verticale
				if xd < xe: # en haut
					d = 1
				elif xe < xd: # en bas
					d = -1
				for c in range(xd+d, xe, d):
					if b[c][yd] != ".":
						ok = False
			else: # si on ne va ni a la verticale ni à l'horizontale. il pourrait encore avoir quelques trous dans mon code avec des mouvement illégaux auquels je n'ai pas penseé
				ok = False 
			if ok:
				myl = ltc(xd, yd, xe, ye, tour, v)
				if myl == 1:
					if [xd, yd] in r:
						r.remove([xd, yd]) # une tour qui bouge ne peut plus faire un roque
					return
				elif myl == 2:
					return(True)
				elif myl == 4:
					return(False)
		elif b[xd][yd] == "N" or b[xd][yd] == "n": # si c'est un cavalier
			for c in nt: # pour chaque mouvement de cavalier
				if xe == xd + c[0] and ye == yd + c[1]: # si ça correspond
					myl = ltc(xd, yd, xe, ye, tour, v)
					if myl == 1:
						return
					elif myl == 2:
						return(True)
					elif myl == 4:
						return(False)
		elif b[xd][yd] == "B" or b[xd][yd] == "b": # si c'est un fou
			if abs(xe-xd) == abs(ye-yd): # si on va bien en diagonale
				ok = True # pareil que tour
				d = [] # comme d pour la tour mais une liste pck il y en a 2: en x et en y
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
					ok = False # on construit d
				e = yd + d[1] # deuxième iterateur de boucle pour les y
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
		elif b[xd][yd] == "Q" or b[xd][yd] == "q": # si c'est une reine
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
				ok = False # tout ça c'est juste la tour + le fou
			if ok:
				myl = ltc(xd, yd, xe, ye, tour, v)
				if myl == 1:
					return
				elif myl == 2:
					return(True)
				elif myl == 4:
					return(False)
		elif b[xd][yd] == "K" or b[xd][yd] == "k": # si c'est un roi (side note: on ne peut pas utiliser ltc avec le roi pour je ne sais plus quelle raison mais ça s'enmêle
			if xd-2 < xe < xd+2 and yd-2 < ye < yd+2: # si il ne fait que bouger d'une case
				b_ = copy.deepcopy(b) # une copie de b pour tester un coup
				move(xd, yd, xe, ye)
				if check(xe, ye, (20 if tour == 10 else 10), True) == 0:  # si ce coup ne mene pas a echec
					if v:
						k[tour] = [xe, ye] # on change les cos dans k
						if [xd, yd] in r:
							r.remove([xd, yd]) # on enleve les cos du roque
						return
					else:
						b = copy.deepcopy(b_) # on annule le coup, vu que pas v
						return(True)
				else:
					b = copy.deepcopy(b_) # on annule le coup dans tous les cas pck on arrive ici que si la case ou on veut aller est menacée
					if not v:
						return(False)
			elif xd == xe and abs(yd-ye) == 2 and [xd, yd] in r and ([xd, 0]if ye < yd else[xd, 7]) in r: # le roque: si le roi va à l'horizontale, de exactement 2 cases et que le roi n'a pas bougé et que la tour vers laquelle il va n'a pas bougé
				ok = True
				if ye < yd: # si roque à gauche
					for c in range(1, 4):
						if b[xd][c] != "." or check(xd, c, (20 if tour == 10 else 10), True) == 1:
							ok = False # pour chaque case entre la tour et le roi, on vérifie que elle est vide et pas menacée
				if yd < ye: # si roque à droite
					for c in range(5, 7):
						if b[xd][c] != "." or check(xd, c, (20 if tour == 10 else 10), True) == 1:
							ok = False # pareil, mais avec des colonnes différentes
				if ok:
					if check(xd, yd, (20 if tour == 10 else 10), True) == 0: # pck on peut pas faire un roque quand on est en échec
						if v:
							move(xd, yd, xe, ye)
							move(xd, (0 if ye < yd else 7), xd, (ye+1 if ye < yd else ye-1)) # on bouge la tour et le roi
							r.remove([xd, yd]) # on n'enlève que le roi du roque pck de toute façon les tours ne peuvent pas faire de roque sans roi
							k[tour] = [xe, ye] # on change les co dans k
							return
						else:
							return(True)
					else:
						if not v:
							return(False)
	if v:
		return("interdit") # pck on arrive ici que si on n'a trouve aucun coup legal pour xd yd xe ye
	else:
		return(False)
def check(xr, yr, tour, rep):
	"""dit si une case est menacée, avec rep faux si on veut aussi vérifier les échec et mat"""
	c = 0 # à la fin, 0 pour rien, si échec 1, si échec et mat 2 
	for i in range(8):
		for j in range(8):
			if legal(i, j, xr, yr, tour, False):
				c = 1
				break # pour chaque case, si elle peut prendre la case xr yr, on met c a 1
		if c == 1:
			break
	if c == 1 and not rep: # parce que ça c'est pour l'échec et mat
		c = 2
		for vx in range(-1, 2):
			for vy in range(-1, 2): # pr chaque coup possible du roi
				if not (vy == 0 and vx == 0):
					xf, yf = xr+vx, yr+vy
					if -1 < xf < 8 and -1 < yf < 8:
						if legal(xr, yr, xf, yf, (20 if tour == 10 else 10), False):
							c = 1
							break# si ce coup sort le roi d'échec, c'est bon
			if c == 1:
				break
		if c == 2: # si le roi lui-même ne peut sortir d'échec, il faut se donner la peine de vérifier les autres pièces alliées
			for i in range(8):
				for j in range(8):
					for x in range(8):
						for y in range(8): # pour chaque couple de co
							if legal(i, j, x, y, (20 if tour == 10 else 10), False):
								c = 1
								break # si aller des premières co aux deuxièmes sort le roi d'échec, c'est bon (et oui c'est légèrement bourrin et après ne vous étonnez pas que je mette quatre break)
						if c == 1:
							break
					if c == 1:
						break
				if c == 1:
					break
	return(c)
while not fin:
	afficher()
	t = input("Tour des Blancs. Entrez votre coup. "if tour == 10 else"Tour des Noirs. Entrez votre coup. ")
	m = [int(t[1])-1, ord(t[0])-65, int(t[3])-1, ord(t[2])-65] # pck pr moi les x, la premiere coordonnee, c'est les lignes, or aux échecs on dit la colonne d'abord, qui d'ailleurs est une lettre d'où les ord
	if (b[m[0]][m[1]] != "p" and b[m[0]][m[1]] != "P") or b[m[2]][m[3]] != ".": # si on a bougé une autre pièce qu'un pion ou si on va capturer une pièce
		ppt = 0
	else:
		ppt = 1 # Pour Plus Tard (pour les 50 coups)
	l = legal(m[0], m[1], m[2], m[3], tour, True)
	while l == "interdit": # tant que ce coup, pour une raison ou pour une autre, est illégal
		t = input("Coup interdit. Entrez-en un autre. ")
		m = [int(t[1])-1, ord(t[0])-65, int(t[3])-1, ord(t[2])-65]
		if (b[m[0]][m[1]] != "p" and b[m[0]][m[1]] != "P") or b[m[2]][m[3]] != ".":
			ppt = 0
		else:
			ppt = 1
		l = legal(m[0], m[1], m[2], m[3], tour, True) # on repart pour un tour
	for i in range(8): # (pour les promotions) pour chaque colonne:
		for j in range(0, 8, 7): # pour les deux lignes qui nous intéressent, 0 et 7
			if b[j][i] == ("p" if j == 0 else "P"):
				o = input("Promotion du pion en" + chr(i+65)+str(j+1)+". Entrez le nom de la pièce que vous voulez.").lower()
				if o == "reine":
					b[j][i] = ("q" if j == 0 else "Q")
				elif o == "fou":
					b[j][i] = ("b" if j == 0 else "B")
				elif o == "tour":
					b[j][i] = ("r" if j == 0 else "R")
				elif o == "cavalier":
					b[j][i] = ("n" if j == 0 else "N") # DEWISOTT
	kk = k[(20 if tour == 10 else 10)] # les coordonnées du roi ennemi
	ch = check(kk[0], kk[1], tour, False)
	if ch == 1: 
		print("Noirs en échec." if tour == 10 else "Blancs en échec.")
	elif ch == 2:
		afficher()
		print("Echec et mat, les Blancs gagnent."if tour == 10 else"Echec et mat, les Noirs gagnent.")
		fin = True
	b_ = "" 
	for x in range(8):
		for y in range(8):
			b_+=b[x][y] # on forme b_ copie de b sauf que c'est une string pour pouvoir l'utiliser comme clé de dictionnaire
	if b_ in der.keys(): # si on a déjà vu ce plateau on le dit dans der
		der[b_] += 1
		if der[b_] == 3: # et si ça fait 3, c'est bon
			afficher()
			print("Match nul par triple répétition.")
			fin = True
	else:
		der[b_] = 1 # et sinon, on l'ajoute en disant que on l'a vu une fois
	dead = True 
	for i in range(8):
		for j in range(8):
			if b[i][j] != "." and b[i][j] != "K" and b[i][j] != "k": # s'il reste autre chose que des rois, ce n'est pas un manque de pièces
				dead = False
	if dead:
		afficher()
		print("Match nul par manque de pièces.")
		fin = True
	pat = True
	for i in range(8):
		for j in range(8):
			for x in range(8):
				for y in range(8):
					if legal(i, j, x, y, (20 if tour == 10 else 10), False):
						pat = False
					if not pat:
						break
				if not pat:
					break
			if not pat:
				break
		if not pat:
			break # tout ça pour dire: si l'adversaire n'aura aucun coup légal
	if pat and check(k[(20 if tour == 10 else 10)][0], k[(20 if tour == 10 else 10)][1], tour, True) == 0: # pck il faut aussi ne pas être en échec et mat pour être en pat. ça fait peut-être doublon mais on sait jamais
		afficher()
		print("Match nul par pat.")
		fin = True
	if ppt == 0:
		row = 0
	else:
		row += 1
	if row == 50:
		afficher()
		print("Match nul par cinquante coups.")
		fin = True # on utilise notre ppt
	if tour == 10:
		tour = 20
	else:
		tour = 10 # et, pour finir, ou change le tour.
