# the game of go
import copy
l = int(input("Plateau de côté: "))
b = [[2 for _ in range(l)] for _ in range(l)] # b pour Board. définition du plateau vide
pa = 0 # pa pour passé. le nombre de joueaurs ayant passé à la suite
t = 0 # le tour, 0 pour noir et 1 pour blanc. Les pierres noires et blanches seront représentées avec les même chiffres.
v = [[1, 0], [-1, 0], [0, 1], [0, -1]] # v pour voisin. les coordonnées des pièces adjacentes
ko = [] # ko pour la règle du même nom. la liste des plateaux déja vus
def lib(x, y, f, mem):
	"""retourne si la pierre aux co x et y a des libertés""" # mem pour mémoïsation, ou le concept de noter les valeurs des résultats qu'on devrait sinon calculer plusieurs fois. ceci accélère beaucoup le calcul de libertés de grandes structures. note: actuellement la mémoïsation est partielle car il ne s'en souvient que pour le calcul de cette case, et on pourrait faire qu'à chaque fois il nous donne les résultats pour toute la structure, vu que si il return True pour une des pierres, c'est bon pour toutes. en cours
	if (x, y) in mem.keys():
		return mem[(x, y)]
	for p in v:
		if 0 <= x+p[0] <= l-1 and 0 <= y+p[1] <= l-1: 
			if b[x+p[0]][y+p[1]] == 2:
				mem[(x, y)] = True
				return True   #retourne True si la pierre en question a une liberté directe
	for p in v:
		if 0 <= x+p[0] <= l-1 and 0 <= y+p[1] <= l-1 and [x, y] not in f: 
			if b[x+p[0]][y+p[1]] == b[x][y]:
				if lib(x+p[0], y+p[1], f+[[x, y]], mem): # f sert à éviter une boucle infinie, y appparaissent les pierres de cette structure déjà vues et qui n'ont pas de liberté directes
					mem[(x, y)] = True
					return True   #retourne True si la pierre appartient à une structure ayant une liberté
	mem[(x, y)] = False
	return False   #sinon, renvoie False
def afficher():
	"""affiche le plateau"""
	print("  1", end = "")
	for aaa in range(3, l+1, 2):
		if aaa < 10:
			print(" ", end = "")
		print("  " + str(aaa), end = "") # les numéros des colonnes
	print()
	for x in range(l):
		if x%2 == 1:
			print("  ", end = "")
		else:
			if x < 9:
				print(" ", end = "")
			print(str(x+1), end = "") # les numéros des lignes
		for y in range(l):
			if y != 0:
				print(" ", end="")			
			if b[x][y] == 0:
				print("+", end="")
			elif b[x][y] == 1:
				print("o", end="")
			else:
				print("‧", end="") # les caractères à proprement parler
		print("")
afficher() # le jeu en lui-même
while pa < 2: # tant que les deux joueurs n'ont pas passé de suite:
	k = True
	while k: # une première fois, puis tant que le coup est illégal:				L = -1				C = randint(0, l-1)
		L = int(input("Ligne "))-1
		C = int(input("Colonne "))-1
		if 0 <= L <= l-1 and 0 <= C <= l-1: # si le joueur ne passe pas:
			c = copy.deepcopy(b) # une copie de b sur laquelle tester les conséquences
			pa = 0 # on réinitialise pa
			done = False # pour vérifier que les co sont bien celles d'une case vide
			if c[L][C] == 2:
				c[L][C] = t
				done = True
			b, c, d = copy.deepcopy(c), copy.deepcopy(b), copy.deepcopy(c) # on inverse les valeurs de b et c pour que b contienne ce qu'on veut tester. Ce principe sera réutilisé plusieurs fois. C'est un peu sale mais moins que de passer 19 * 19 éléments en argument à une fonction récursive. On crée aussi d, une deuxième copie avec le coup fait, pour réaliser les captures. Sans ça, on changerait b, ce qui ferait que les structures perdraient juste une pièce avant de s'arrêter, vu que ça leur ferait une liberté
			for x in range(l):
				for y in range(l):
					if b[x][y] == (0 if t == 1 else 1) and not lib(x, y, [], {}):
						d[x][y] = 2 # tester si les pierres sont capturables et si oui effectuer les captures dans d
			b = copy.deepcopy(d) # on donne à b la valeur de d avec toutes les opérations
			k = (not done or d in ko or not lib(L, C, [], {})) # si ce coup est légal (aka ni un ko ni un suicide et c'était bien une case vide)
			if k: # si coup illégal  on remet b à la valeur de c càd depuis un certain temps le tableau non modifié
				b = copy.deepcopy(c)
		elif L == -1 and C == -1:
			pa+=1
			break
	ko.append(b)
	t = (0 if t == 1 else 1)
	afficher() # on change le tour, on change ko, et on affiche
P = [0, 0.5] # P pour Points. a partir d'ici, le comptage des points à la fin. a cette ligne, initialisation des points, avec le komi pour les blancs
for aaa in range(2): #(on factorise le comptage des points)
	sb = [[]for _ in range(l)] # sb pour ScoreBoard. on remarque que le territoire contrôlé est très similaire à la capture de structures. on réutilise donc lib et l'échange de tableaux, même si avec quelques modifications au préalable
	for i in range(l):
		for j in range(l):
			if b[i][j] == 2:
				sb[i].append((0 if aaa == 1 else 1))
			elif b[i][j] == (0 if aaa == 1 else 1):
				sb[i].append(2)
			else:
				sb[i].append(aaa) # transforme sb en une copie de b mais en inversant les ennemis et les cases vides. En effet, nous voulons ici vérifier si on peut "capturer" les cases vides, pas les ennemis. Cela permet aussi d'implémenter le fait que si il y a un ennemi dans un territaoire, on ne le contrôle pas, car ici, ceux qui étaient les cases vides prendront ceux qui étaient les cases ennemies pour des libertés, et ne seront donc pas "capturés"-> comptés.
	b, sb = copy.deepcopy(sb), copy.deepcopy(b) # on inverse les valeurs
	for i in range(l):
		for j in range(l):
			if b[i][j] == (1 if aaa == 0 else 0) and not lib(i, j, [], {}): # on compte les cases contrôlées
				P[aaa]+=1
	b = copy.deepcopy(sb) # on redonne à b la valeur non inversée
for i in range(l):
	for j in range(l):
		if b[i][j] != 2:
			P[b[i][j]]+=1 # on compte le nombre de pierres sur le plateau de chaque couleur
afficher() # puis, pour finir, on affiche les scores et le gagnant
print(P[0], "-", P[1])
if P[0] > P[1]:
	print("Les Noirs gagnent")
elif P[1] > P[0]:
	print("Les Blancs gagnent")
