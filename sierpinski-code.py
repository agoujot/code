from turtle import *
from math import *
reset() 
# pour désactiver l'animation, décommenter les lignes 5 et 34. les commenter pour réactiver
#tracer(False) # désactive l'affichage au fur et à mesure, la vitesses maximum de turtle étant assez lente. ceci lui fait générer l'image en entier avant de la montrer l.34, de manière quasi instantanée
maxsize = 512 # la taille du plus grand rectangle
minsize = 4 # la taille des plus petits rectangles. avec l'animation, c'est assez lent(3min30)
h = (sqrt(3)/2) # une grandeur nécessaire pour la hauteur de triangle équilatéraux
speed(0) # pour maximiser la vitesse
hideturtle()
bgcolor("black")
color("aqua")
def pinski(x, y, cote): 
	""" dessine un triangle inversé dans le triangle de coté cote et dont le coin en bas à droite a pour coordonnées x et y, puis se rapelle sur les 3 plut petits triangles ainsi formés"""
	demicote = cote//2 # le coté du triangle inversé
	up()
	goto(x-demicote, y) # le point en bas du triangle inversé
	down()
	for i in range(3):
		left(120)
		forward(demicote) # de l.19 à ici on trace le triangle inversé
	if demicote > minsize:  
		pinski(x, y, demicote) # le triangle en bas à droite du triangle inversé
		pinski(x-demicote, y, demicote) # le triangle en bas à gauche 
		pinski(x-demicote//2, y+h*demicote, demicote) # le triangle en haut (demicote//2 car le trianle en haut est au milieu, et h*demicote étant la heuteur du triangle inversé)
up()
goto(maxsize//2, -(h*maxsize)//2) # le coin en bas à droite
down()
for i in range(3):
	left(120)
	forward(maxsize) # on trace le seul triangle à l'endroit, qui contient tous les autres
right(60)
pinski(maxsize//2, -(h*maxsize)//2, maxsize)
#update()
done()