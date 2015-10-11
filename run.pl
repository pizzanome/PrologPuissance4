%%%%%%%%%%%%%%%%%%%%%%%%%%%% Run %%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Includes %%%%%%%%%%%%
:- [jeu].
:- [ihm].

%%%%%%%%%%%%%%%%%%%%%%%%%%% Constantes %%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%% Gestion du cycle de vie %%
run 	:-
		demandeTypeDeJeu(TypeJoueur1),
		demandeTypeDeJeu(TypeJoueur2),
     	init,
     	random_select(TypeJoueurR,[TypeJoueur1,TypeJoueur2],[TypeJoueurJ|_]),
	   	assert(joueurCourant(rouge,TypeJoueurR)),
	   	assert(autreJoueur(jaune,TypeJoueurJ)),
	   	jeu(PartieNulle),
	   	afficherFin(PartieNulle).
	   
jeu(PartieNulle) :- 	
		tour(PartieNulle).

tour(PartieNulle) 	:- 
		afficher,
		joueurCourant(CouleurJCourant,TypeJoueur),
		aQuiDemanderCoup(CouleurJCourant,TypeJoueur,'',Coup),
		bouclePlacer(Coup,TypeJoueur,CouleurJCourant,Y),
		testFin(Coup,Y,CouleurJCourant, PartieNulle).

bouclePlacer(Coup,_,CouleurJCourant,Y) :-
	placerJeton(Coup,Y,CouleurJCourant),!.
bouclePlacer(_,TypeJoueur,CouleurJCourant,Y) :-
	aQuiDemanderCoup(CouleurJCourant,TypeJoueur,'Votre coup n\'est pas valide. Veuillez reessayer.\n',Coup),
	bouclePlacer(Coup,TypeJoueur,CouleurJCourant,Y).

changerJoueur :-
	joueurCourant(rouge,TypeJoueurR), 
	autreJoueur(jaune,TypeJoueurJ),
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)),
	assert(joueurCourant(jaune,TypeJoueurJ)),
	assert(autreJoueur(rouge,TypeJoueurR)),!.
changerJoueur :-
	joueurCourant(jaune,TypeJoueurJ),
	autreJoueur(rouge,TypeJoueurR),
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)),
	assert(joueurCourant(rouge,TypeJoueurR)),
	assert(autreJoueur(jaune,TypeJoueurJ)),!.

testFin(Coup,Y,CouleurJCourant,PartieNulle) :- gagne(Coup,Y,CouleurJCourant), PartieNulle=false, afficher.
testFin(_,_,_,PartieNulle) :- not(coupPossible), PartieNulle=true, afficher.
testFin(_,_,_,PartieNulle) :- changerJoueur, tour(PartieNulle).

% permet d'appeler l'ihm ou les IAs pour r�cup�rer le coup suivant
% 1==humain
aQuiDemanderCoup(CouleurJCourant,1,Message,Coup) :- demandeCoup(CouleurJCourant,Message,Coup),!.
% 2==IA aleatoire
aQuiDemanderCoup(CouleurJCourant,2,Message,Coup) :- demandeCoup(CouleurJCourant,Message,Coup).
% etc ...

getTypeJoueurString(1,TypeJoueurString) :- TypeJoueurString='Humain',!.
getTypeJoueurString(2,TypeJoueurString) :- TypeJoueurString='IA Aleatoire'.

% partie non nulle
afficherFin(false) :-
	joueurCourant(CouleurGagnante,TypeJoueurGagnant),
	autreJoueur(CouleurPerdante,TypeJoueurPerdant),
	getTypeJoueurString(TypeJoueurGagnant,TypeJoueurGagnantString),
	getTypeJoueurString(TypeJoueurPerdant,TypeJoueurPerdantString),
   	afficherGagnant(CouleurGagnante,CouleurPerdante,TypeJoueurGagnantString,TypeJoueurPerdantString).
% partie nulle
afficherFin(true) :-
	afficherPartieNulle.
