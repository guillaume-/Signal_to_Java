Compiler :
make

Nettoyer :
make clean

Exécuter :
./main < fichier_signal

Résultat : dossier src contenant du code Java correspondant au fichier_signal

Fonctionnement :
cf "Comptes Rendus/Présentation 31.07.2014.pdf"

Hypothèses :
- code applatit 
> x = 1+1+1 -> {x1 = 1+1 | x2 = x1+1}
- une assignation utilisant un delay ne peut initialiser la variable qu'avec une seule valeur
> si on gère plus qu'un code applatit, il resterait des traitements pour gérer la forme
> (x $1 init 0) + (x $1 init 1)

Derniers ajouts:
	gérer import Usable.* si usage de Call dans une tâche
	rendre le codage automatique des fonctions add, mul et sub lorsque celles-ci sont utilisées
	gérer les blocs when où on retourne false sinon
	ajout du reset() pour les delay à la fin d'un cycle
	vérification syntaxique :
		^a -> nom CORRECT pour désigner l'horloge de a
		^a -> déclaration de variable INCORRECTE
		^a := -> définit maintenant une contrainte d'horloge
		a := x(a) -> INCORRECT
		{ a := b
		a := c } ensemble CORRECT (si on suppose que b et c ont une horloge jamais synchro)
	MAJ du template :
		le fichier In définit à chaque cycle la présence de chaque Signal
		calcul du résultat d'une tâche en fonction de sa présence
	format "constante default expression" désormais équivalent à "constante"

À gérer:
	Les contraintes d'horloge sont sauvegardées dans g_kons, mais inutilisées
	{ a := b
	a := c } est accepté, mais l'une des 2 instructions est actuellement ignorée côté Java

