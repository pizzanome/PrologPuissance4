%%%%%%%%%%%% tests.pl %%%%%%%%%%%%
% Quelques tests unitaires.

:- use_module(jeu).
:- use_module(util).
:- use_module(miniMax).
:- use_module(eval).

%%%%% Tests victoire (gagne) %%%%%

t_gagne_colonne :-
	assert(case(1,1,rouge)),
	assert(case(1,2,rouge)),
	assert(case(1,3,rouge)),
	assert(case(1,4,rouge)),
	gagne(1,4,rouge).

t_gagne_ligne :-
	assert(case(1,1,rouge)),
	assert(case(2,1,rouge)),
	assert(case(3,1,rouge)),
	assert(case(4,1,rouge)),
	gagne(4,1,rouge).

t_gagne_diagonale1 :-
	assert(case(1,1,rouge)),
	assert(case(2,2,rouge)),
	assert(case(3,3,rouge)),
	assert(case(4,4,rouge)),
	gagne(4,4,rouge).

t_gagne_diagonale2 :-
	assert(case(4,4,rouge)),
	assert(case(3,3,rouge)),
	assert(case(2,2,rouge)),
	assert(case(1,1,rouge)),
	gagne(1,1,rouge).

%%%%% Tests Minimax %%%%%

t_minimax_prof1 :-
	assert(evaluation(test1)),
	parcoursArbre(rouge,1,R,Value),retract(evaluation(X)),R==4,Value==10.

t_minimax_prof2 :-
	assert(evaluation(test1)),
	parcoursArbre(rouge,2,R,Value),retract(evaluation(X)),R==1,Value==(-5).



%%%%% Tests changer de joueur (changerJoueur) %%%%%

t_changer_joueur1 :-
	assert(joueurCourant(rouge, 1)),
	assert(autreJoueur(jaune, 2)),
	changerJoueur,
	joueurCourant(jaune, 2),
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)).

t_changer_joueur2 :-
	assert(joueurCourant(jaune, 3)),
	assert(autreJoueur(rouge, 1)),
	changerJoueur,
	joueurCourant(rouge, 1),
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)).

%%%%% Tests coup valide (coupValide) %%%%%

t_coup_valide1 :-
	coupValide(1),
	coupValide(2),
	coupValide(3),
	coupValide(4),
	coupValide(5),
	coupValide(6),
	coupValide(7).

t_coup_invalide1 :-
	not(coupValide(0)).

t_coup_invalide2 :-
	not(coupValide(8)).

t_coup_invalide3 :-
	not(coupValide(-1)).

t_coup_valide2 :-
	assert(case(1,1,rouge)),
	assert(case(1,2,rouge)),
	assert(case(1,3,rouge)),
	assert(case(1,4,rouge)),
	assert(case(1,5,rouge)),
	coupValide(1).

t_coup_invalide4 :-
	assert(case(1,1,rouge)),
	assert(case(1,2,rouge)),
	assert(case(1,3,rouge)),
	assert(case(1,4,rouge)),
	assert(case(1,5,rouge)),
	assert(case(1,5,rouge)),
	assert(case(1,6,rouge)),
	not(coupValide(1)).

%%%%% Tests insérer jeton (insererJeton) %%%%%

t_inserer_jeton1 :-
	insererJeton(1, X, rouge),
	X == 1,
	case(1,1, rouge),
	retractall(case(_,_,_)).

t_inserer_jeton2 :-
	insererJeton(1, _, rouge),
	insererJeton(1, _, rouge),
	insererJeton(2, _, jaune),
	case(1,1, rouge),
	case(1,2, rouge),
	case(2,1, jaune),
	retractall(case(_,_,_)).


%%%%% Tests calcul %%%%%
t_calculGauche :-
	t_calculGauche1,t_calculGauche2,t_calculGauche3.

t_calculGauche1 :- 
	calculGauche(1,0,jaune,2,3).

t_calculGauche2 :- 
	calculGauche(3,0,jaune,3,3).

t_calculGauche3 :-
	assert(case(1,0,rouge)),
	calculGauche(2,0,jaune,1,3).

t_calculDroite :-
	t_calculDroite1,t_calculDroite2,t_calculDroite3.

t_calculDroite1 :- 
	calculDroite(5,0,jaune,2,3).

t_calculDroite2 :- 
	calculDroite(1,0,jaune,3,3).

t_calculDroite3 :-
	assert(case(4,0,rouge)),
	calculDroite(2,0,jaune,2,3).

t_calculLigne :-
	t_calculLigne1,t_calculLigne2,t_calculLigne3,t_calculLigne4.

t_calculLigne1 :-
	calculLigne(0,0,jaune,1).

t_calculLigne2 :-
	calculLigne(6,0,jaune,1).

t_calculLigne3 :-
	calculLigne(1,0,jaune,2).

t_calculLigne4 :-
	assert(case(1,0,rouge)),
	assert(case(2,0,jaune)),
	calculLigne(3,0,jaune,2).

t_calculHaut :-
	t_calculHaut1,t_calculHaut2.

t_calculHaut1 :-
	calculHaut(0,1,3,3).

t_calculHaut2 :-
	calculHaut(0,4,2,3).

t_calculBas :-
	t_calculBas1,t_calculBas2,t_calculBas3.

t_calculBas1 :-
	calculBas(0,1,jaune,2,3).

t_calculBas2 :-
	calculBas(0,4,jaune,3,3).

t_calculBas3 :-
	assert(case(0,1,rouge)),
	calculBas(0,2,jaune,1,3).

t_calculColonne :-
	t_calculColonne1,t_calculColonne2,t_calculColonne3.

t_calculColonne1 :-
	assert(case(0,0,rouge)),
	calculColonne(0,3,jaune,2).

t_calculColonne2 :-
	calculColonne(0,0,jaune,1).

t_calculColonne3 :-
	assert(case(0,2,rouge)),
	calculColonne(0,3,jaune,0).

t_calculDiagGD :-
	t_calculDiagGD1,t_calculDiagGD2,t_calculDiagGD3,t_calculDiagGD4.

t_calculDiagGD1 :-
	assert(case(4,1,rouge)),
	calculDiagGaucheDroite(2,3,jaune,1).

t_calculDiagGD2 :-
	calculDiagGaucheDroite(1,2,jaune,1).

t_calculDiagGD3 :-
	calculDiagGaucheDroite(0,1,jaune,0).

t_calculDiagGD4 :-
	initJeu,
	assert(case(5,0,rouge)),
	calculDiagGaucheDroite(2,3,jaune,2).

t_calculDiagDG :-
	t_calculDiagDG1,t_calculDiagDG2,t_calculDiagDG3,t_calculDiagDG4.

t_calculDiagDG1 :-
	assert(case(1,1,rouge)),
	calculDiagDroiteGauche(3,3,jaune,1).

t_calculDiagDG2 :-
	calculDiagDroiteGauche(4,1,jaune,1).

t_calculDiagDG3 :-
	calculDiagDroiteGauche(4,0,jaune,0).

t_calculDiagDG4 :-
	initJeu,
	assert(case(0,0,rouge)),
	calculDiagDroiteGauche(2,2,jaune,2).