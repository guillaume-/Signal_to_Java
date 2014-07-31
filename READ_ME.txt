Résultat : dossier src	contenant du code Java correspondant au fichier_signal

Compiler :
make

Nettoyer :
make clean

Exécuter :
./main < fichier_signal

Fonctionnement INRIA :
cf "Comptes Rendus/Présentation 31.07.2014.pdf"

Derniers ajouts:
	gérer import Usable.* si usage de Call dans une tâche
	rendre le codage automatique des fonctions add, mul et sub lorsque celles-ci sont utilisées
	gérer les blocs when où on retourne false sinon
	ajout du reset() pour les delay à la fin d'un cycle
	vérification syntaxique :
		^a -> nom CORRECT
		a := x(a) -> INCORRECT
		{ a := b
		a := c } ensemble CORRECT (si on suppose que b et c ont une horloge jamais synchro)
	MAJ du template :
		le fichier In définit à chaque cycle la présence de chaque Signal
		calcul du résultat d'une tâche en fonction de sa présence
	format "constante default expression" désormais équivalent à "constante"

À gérer:
	^a est accepté, mais n'est pas détecté comme horloge de a
	{ a := b
	a := c } est accepté, mais l'une des 2 instructions est actuellement ignorée
