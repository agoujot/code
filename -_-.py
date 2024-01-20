#finding, with the description of a graph, whether it is a semi-eulerian graph (and the number of solutions). and, yes, it it not clean.
import copy
def trouver(cpris, c):
	"""a partir de la liste des chemins pris et du chemin qu'on vient de prendre essaie pour tous les autres chemins"""
	global nbsolutions
	global fait
	if not fait:
		C, cpris_ = c[0], copy.deepcopy(cpris)
		if C not in cpris_:
			cpris_.append(C)
			if len(cpris_) == (nbnoeuds):
				if tout:
					nbsolutions += 1
				else:
					fait = True
			else:
				for csuivant in graphe[c]:
					trouver(cpris_, csuivant)
def construire(figure):
	"""construit à partir d'une description pour chaque arrête de ses deux bouts un dictionnaire décrivant à partir de chaque chemin les autres chemins."""
	dic_arretes = {}
	for j in range(len(figure)):
		arrete = figure[j].split("/")
		dic_arretes[arrete[0]] = [arrete[1], arrete[2]] # construit un dictionnaire donnant pour chaque arrete ses bouts
	dic_chemins = {}
	for lettrea in dic_arretes.keys():
		for nombre in dic_arretes[lettrea]: # cad pour chaque chemin
			csuivants = []
			for lettreb in dic_arretes.keys():# pour chaque arrete
				boutsb = [x for x in dic_arretes[lettreb]]
				if nombre in boutsb and lettrea != lettreb: # si ils partagent un bout mais ne sont pas identiques
					boutsb.remove(nombre)
					csuivants.append(lettreb+boutsb[0]) # on rajoute l'autre bout de l'autre arrete
			dic_chemins[lettrea+nombre] = csuivants
	return(dic_chemins)
def afficher():
	"""appelle trouver et fait le formatage de la reponse."""
	global nbsolutions
	nbsolutions = 0
	global fait
	fait = False
	if tout:
		print("Le problème étant comment passer par chaque arrête une et une seule fois dans le graphe, il y a ", end = "")
		for depart in graphe.keys():
			trouver([], depart)
		print(str(nbsolutions)+" solutions.")
	else:
		for depart in graphe.keys():
			trouver([], depart)
		if fait:
			print("Ce graphe est eulerien.")
		else:
			print("Ce graphe n'est pas eulerien.")
def modifier(dic_chemins):
	"""DEWISOTT."""
	if input("Entrez R pour rajouter des arrêtes et S pour en supprimer. ") == "R":
		segments = list(input("Entrez les nouvelles arrêtes avec le même format. ").split("."))
		premier = True # si on en est au premier bout d'un chemin
		for ch in sorted(dic_chemins.keys()): # pour etre sur de le faire dans l'ordre
			if premier:
				der = ch[1:]
				premier = False # on note le bout et on passe au suivant
			else:
				segments.append(ch[0]+"/"+der+"/"+ch[1:]) # on ajoute le nom, le bout note plus tot et le second bout
				premier = True
	else:
		a_enlever = list(map(str, input("Entrez les noms des arrêtes a supprimer separés par des points. ").split(".")))
		segments = []
		premier = True
		for ch in dic_chemins.keys():
			if premier:
				der = ch[1:]
				premier = False
			elif ch[0] not in a_enlever:
				segments.append(ch[0]+"/"+der+"/"+ch[1:])
				premier = True
			else:
				premier = True
	dic_chemins = construire(segments)
	return(dic_chemins)
figure = list(map(str, input("Entrez votre graphe en mettant [arrête]/[noeud1]/[noeud2], \n(avec [arrête] reliant [noeud1] et [noeud2]) et en séparant les arretes par des points.\n ").split(".")))
graphe = construire(figure)
fin = ""
tout = True
while fin == "":
	inp = input("Entrez A pour afficher la solution avec le graphe actuel, M pour le modifier et V pour le vider (enlever tout).")
	if inp == "M":
		graphe = modifier(graphe)
	elif inp == "V":
		graphe = {}
	else:
		nbnoeuds = len(graphe.keys())//2
		tout = input("Entrer pour savoir si le graphe est eulerien et autre chose pour le nombre de solutions.") != ""
		afficher()
	fin = input("Appuyer sur entrer pour continuer et entrez quelque chose pour arrêter.")
"""TESTS:
A/1/2.B/2/3.C/3/4.D/4/5.E/5/6.F/6/7.G/7/8.H/8/1.I/2/8.J/2/4.K/4/6.L/6/8 CARRE DANS CARRE = 960
carre dans carre dans carre
A/1/2.B/2/3.C/3/4.D/4/1.E/5/1.F/1/6.G/2/6.H/2/7.I/3/7.J/3/8.K/4/8.L/4/5.M/5/1.N/6/1.O/6/2.P/7/2.Q/7/3.R/8/3.S/8/4.T/5/4.U/1/2.V/2/3.W/3/4.X/1/4 DEUX CARRES DANS CARRES SUPERPOSES = ?
A/1/2.B/2/3.C/3/4.D/4/1.E/1/2.F/2/3.G/3/4.H/4/1 DEUX CARRES SUPERPOSES = 640
A/1/2.B/2/3.C/3/4.D/4/1.E/1/2.F/2/3.G/3/4.H/4/1.I/1/2.J/2/3.K/3/4.L/4/1 TROIS CARRES SUPERPOSES = 476928
A/1/2.B/2/3.C/3/4.D/4/1.E/1/2.F/2/3.G/3/4.H/4/1.I/1/2.J/2/3.K/3/4.L/4/1.M/1/2.N/2/3.O/3/4.P/4/1 QUATRE CARRES SUPERPOSES = ?
A/1/2.B/2/3.C/3/4.D/4/5.E/1/5.F/2/4.G/3/5.H/2/5 classique = 88
A/1/2.B/2/3.C/3/4.D/4/5.E/1/5.F/2/4.G/3/5.H/2/5.I/2/6.J/3/6 DEUX TOITS = 416
A/1/2.B/2/3.C/3/4.D/4/5.E/1/5.F/2/4.G/3/5.H/2/5.I/2/6.J/3/6.K/5/7.L/4/7 TROIS TOITS = 1920
A/1/2.B/2/3.C/3/4.D/4/5.E/1/5.F/2/4.G/3/5.H/2/5.I/2/6.J/6/3.K/5/7.L/4/7.M/3/8.N/8/4 QUATRE TOITS = 0
A/1/2.B/2/3.C/3/4.D/4/5.E/1/5.F/2/4.G/3/5.H/2/5.I/1/3 FUNNY = 264
A/1/2.B/2/3.C/3/4.D/4/5.E/1/5.F/2/4.G/3/5.H/2/5.I/1/3.J/1/4 UMM = 2640
A/1/2.B/2/3.C/3/4.D/4/1.E/5/1.F/1/6.G/2/6.H/2/7.I/3/7.J/3/8.K/4/8.L/4/5.M/5/9.N/6/9.O/6/10.P/7/10.Q/7/11.R/8/11.S/8/12.T/5/12.U/9/10.V/10/11.W/11/12.X/9/12 MONSTRE = 9316608 (en 30 min)
A/1/2.B/2/3.C/3/4.D/4/1.E/1/5.F/1/5.G/2/5.H/2/5.I/3/5.J/3/5.K/4/5.L/4/5.M/5/9.N/5/9.O/5/10.P/5/10.Q/5/11.R/5/11.S/5/12.T/5/12.U/9/10.V/10/11.W/11/12.X/9/12 MONSTRE SABLIER = ?
A/1/2.B/2/3.C/3/4.D/4/1.E/3/1.F/1/6.G/2/6.H/2/7.I/3/7.J/3/8.K/4/8.L/4/3.M/3/7.N/6/7.O/6/10.P/7/10.Q/7/11.R/8/11.S/8/12.T/3/12.U/7/10.V/10/11.W/11/12.X/7/12 MONSTRE HEDRON = ?
A/1/2.B/2/3.C/3/4.D/4/1 UN SIXIEME DE MONSTRE = 8
A/1/2.B/2/3.C/3/4.D/1/4.F/1/6.E/1/5.N/6/9.M/5/9 DEUX SIXIEMES DE MONSTRE = 32
A/1/2.B/2/3.C/3/4.D/1/4.F/1/6.L/4/5.E/1/5.K/4/8.M/5/9.N/6/9.S/8/12.T/5/12 TROIS SIXIEMES DE MONSTRE = 384
A/1/2.B/2/3.C/3/4.D/1/4.F/1/6.L/4/5.E/1/5.K/4/8.M/5/9.N/6/9.S/8/12.T/5/12.G/2/6.H/2/7.P/7/10.O/6/10 QUATRES SIXIEMES DE MONSTRE = 3840
A/1/2.B/2/3.C/3/4.D/1/4.F/1/6.L/4/5.E/1/5.K/4/8.M/5/9.N/6/9.S/8/12.T/5/12.G/2/6.H/2/7.P/7/10.O/6/10.J/3/8.I/3/7.R/8/11.Q/7/11 CINQ SIXIEMES DE MONSTRE = 117280
"""
