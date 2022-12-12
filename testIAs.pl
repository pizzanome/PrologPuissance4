%%%%%%%%%%%% testIAs.pl %%%%%%%%%%%%
% Permet de tester comment performent différentes IAs lorsqu elles jouent l'une contre l'autre.

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(jeu).
:- use_module(ia).
:- use_module(eval).
:- use_module(miniMax).
:- use_module(library(statistics)).
:- ['webserver.pl'].

:- dynamic joueurCourant/2.
:- dynamic autreJoueur/2.
:- dynamic tempsJoueur/2.
:- dynamic nbCoups/2.

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% runTest/3

runTestWithComments(NbIterations,IA1,IA2) :-
	NbIterationsParIA is NbIterations//2,

	%Mise à zéro du temps écoulé pour les joueurs ainsi que leurs nombres de coups.
	retractall(tempsJoueur(_,_)),
	retractall(nbCoups(_,_)),
	assert(tempsJoueur(IA1,0)),
	assert(tempsJoueur(IA2,0)),
	assert(nbCoups(IA1,0)),
	assert(nbCoups(IA2,0)),

	runTestIAXEnPremier(NbIterationsParIA,IA1,IA2,0,NbFoisIA1GagneEnCommencant,0,NbFoisIA1PerdEnCommencant,0,NbEgaliteIA1),
	runTestIAXEnPremier(NbIterationsParIA,IA2,IA1,0,NbFoisIA2GagneEnCommencant,0,NbFoisIA2PerdEnCommencant,0,NbEgaliteIA2),
	typeJoueur(IA1,TypeIA1),
	typeJoueur(IA2,TypeIA2),
	open('result2.txt',append,Stream),

	write(Stream,'Exécution du test avec les statistiques suivantes : '),
	%description de l'IA1 et de l'IA2
	write(Stream,'\nMatch de IA1 : '),
	typeJoueur(IA1,TypeIA1),
	write(Stream,TypeIA1),
	write(Stream,' contre IA2 : '),
	typeJoueur(IA2,TypeIA2),
	write(Stream,TypeIA2),
	write(Stream,'\n'),

	write(Stream, TypeIA2), write(Stream, ' en commençant : a gagné '), write(Stream, NbFoisIA2GagneEnCommencant),write(Stream, ' fois et a perdu '),write(Stream, NbFoisIA2PerdEnCommencant),write(Stream, ' fois.\n'),
	write(Stream, TypeIA1), write(Stream, ' en commençant : a gagné '), write(Stream, NbFoisIA1GagneEnCommencant),write(Stream, ' fois et a perdu '),write(Stream, NbFoisIA1PerdEnCommencant),write(Stream, ' fois.\n'),
	write(Stream, 'Il y a eu '), write(Stream, NbEgaliteIA2),write(Stream, ' égalites pour IA2'),nl,
	write(Stream, '\nIl y a eu '), write(Stream, NbEgaliteIA1),write(Stream, ' egalites pour IA1'),nl,

	write(TypeIA2), write(' en commençant : a gagné '), write(NbFoisIA2GagneEnCommencant),write(' fois et a perdu '),write(NbFoisIA2PerdEnCommencant),write(' fois.'),
	nl,
	write('Il y a eu '), write(NbEgaliteIA2),write(' egalites pour IA2'),nl,
	write(TypeIA1), write(' en commençant : a gagné '), write(NbFoisIA1GagneEnCommencant),write(' fois et a perdu '),write(NbFoisIA1PerdEnCommencant),write(' fois.'),nl,
	write('Il y a eu '), write(NbEgaliteIA1),write(' egalites pour IA1'),
	close(Stream),
	!.

runTestSimple(NbIterations,IA1,IA2) :-
		NbIterationsParIA is NbIterations//2,
	
		%Mise à zéro du temps écoulé pour les joueurs ainsi que leurs nombres de coups.
		retractall(tempsJoueur(_,_)),
		retractall(nbCoups(_,_)),
		assert(tempsJoueur(IA1,0)),
		assert(tempsJoueur(IA2,0)),
		assert(nbCoups(IA1,0)),
		assert(nbCoups(IA2,0)),
	
		runTestIAXEnPremier(NbIterationsParIA,IA1,IA2,0,NbFoisIA1GagneEnCommencant,0,NbFoisIA1PerdEnCommencant,0,NbEgaliteIA1),
		runTestIAXEnPremier(NbIterationsParIA,IA2,IA1,0,NbFoisIA2GagneEnCommencant,0,NbFoisIA2PerdEnCommencant,0,NbEgaliteIA2),
		!.

runTestStat() :-
	%lance pour chaque IA1 et IA2 le test avec 20 parties
	runTestWithTime(20,2,3),
	runTestWithTime(20,2,4),
	runTestWithTime(20,2,5),
	runTestWithTime(20,2,6),
	runTestWithTime(20,2,7),
	runTestWithTime(20,2,8),
	runTestWithTime(20,3,4),
	runTestWithTime(20,3,5),
	runTestWithTime(20,3,6),
	runTestWithTime(20,3,7),
	runTestWithTime(20,3,8).

%Run test with statistics(:Goal) and shown the output of the statistics : thread_cputime, cputime, trail, process_epoch, memory into the result.txt
runTestWithTime(NbIterations,IA1,IA2) :-
	%ouverture fichier et initialisation message
	open('timeT.txt',append,Stream),

	statistics,
	statistics(walltime, [Start,_]),
	runTestSimple(NbIterations,IA1,IA2),
	statistics(walltime, [End,_]),
	Time is End - Start,
	
	%calcul du temps moyen par coups
	tempsJoueur(IA1, TempsIA1),
	tempsJoueur(IA2, TempsIA2),
	nbCoups(IA1, NbCoupsIA1),
	nbCoups(IA2, NbCoupsIA2),
	MeanTimeIA1 is TempsIA1/NbCoupsIA1,
	MeanTimeIA2 is TempsIA2/NbCoupsIA2,

	typeJoueur(IA1,TypeIA1),
	typeJoueur(IA2,TypeIA2),

	write(Stream, TypeIA1), write(Stream, " : "), write(Stream, MeanTimeIA1), write(Stream, " ms\n"),
	write(Stream, NbCoupsIA1), write(Stream, " coups comptabilisés\n"),
	write(Stream, TypeIA2), write(Stream, " : "), write(Stream, MeanTimeIA2), write(Stream, " ms\n"),
	write(Stream, NbCoupsIA2), write(Stream, " coups comptabilisés\n\n"),
	close(Stream).

%Run test with statistics(:Goal) and shown the output of the statistics : thread_cputime, cputime, trail, process_epoch, memory into the result.txt
runTestWithStatisticsShort(NbIterations,IA1,IA2) :-
	%ouverture fichier et initialisation message
	open('result2.txt',append,Stream),
	
	statistics,
	statistics(walltime, [Start,_]),
	runTestWithComments(NbIterations,IA1,IA2),
	statistics(walltime, [End,_]),
	Time is End - Start,

	%calcul du temps moyen par coups
	tempsJoueur(IA1, TempsIA1),
	tempsJoueur(IA2, TempsIA2),
	nbCoups(IA1, NbCoupsIA1),
	nbCoups(IA2, NbCoupsIA2),
	MeanTimeIA1 is TempsIA1/NbCoupsIA1,
	MeanTimeIA2 is TempsIA2/NbCoupsIA2,

	write(Stream, "\nTemps moyen par coups pour IA1 : "), write(Stream, MeanTimeIA1), write(Stream, " ms"),
	write(Stream, "\nTemps moyen par coups pour IA2 : "), write(Stream, MeanTimeIA2), write(Stream, " ms"),

	statistics,
	statistics(cputime,CpuTime),
	statistics(threads, Threads),
	%affichage des statistiques
	write(Stream, '\nExecution Time : '), write(Stream, Time), write(Stream, ' ms.'),
	write(Stream, '\nNb de thread actifs : '), write(Stream, Threads),
	write(Stream, '\nCpuTime : '), write(Stream,CpuTime), write(Stream, ' s'),
	write(Stream, '\n\n'),
	close(Stream).

%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

% test de sortie de runTestIAXEnPremier
runTestIAXEnPremier(0,_,_,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneIni, NbDrawIni, NbDrawIni) :- !.
runTestIAXEnPremier(NbIterations,IA1,IA2,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneFin, NbDrawIni, NbDrawFin) :-
	init,
	assert(joueurCourant(rouge,IA1)), % rouge = IA1
	assert(autreJoueur(jaune,IA2)),

	jeu(PartieNulle),

	joueurCourant(CouleurIAGagnante,_),
	incrementerGagnant(PartieNulle,CouleurIAGagnante,NbIA1GagneIni,NbIA1GagneFin1,NbIA2GagneIni,NbIA2GagneFin1, NbDrawIni, NbDrawFin1),
	NbIterations2 is NbIterations-1,
	runTestIAXEnPremier(NbIterations2,IA1,IA2,NbIA1GagneFin1,NbIA1GagneFin,NbIA2GagneFin1,NbIA2GagneFin, NbDrawFin1, NbDrawFin).

incrementerGagnant(true,_,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneIni, NbDrawIni, NbDrawFin) :-
	NbDrawFin is NbDrawIni+1.
incrementerGagnant(false,rouge,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneIni, NbDrawIni, NbDrawIni) :-
	NbIA1GagneFin is NbIA1GagneIni+1.
incrementerGagnant(false,jaune,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneFin, NbDrawIni, NbDrawIni) :-
	NbIA2GagneFin is NbIA2GagneIni+1.

jeu(PartieNulle) :-
	tour(PartieNulle).

tour(PartieNulle) :-
	%Mesurer le temps de jeu de chaque tour
	statistics(walltime, [Start,_]),

	joueurCourant(CouleurJCourant,TypeJoueur),
	obtenirCoup(CouleurJCourant,TypeJoueur,Coup),
	placerJeton(Coup,Y,CouleurJCourant),

	statistics(walltime, [End,_]),
	Time is End - Start,

	%Incrémente le temps total pour l'IA du temps de ce tour et incrémente le nombre de coups de 1
	tempsJoueur(TypeJoueur, TempsAncien),
	nbCoups(TypeJoueur, NbCoupsAncien),
	TempsNouveau is TempsAncien + Time,
	NbCoupsNouveau is NbCoupsAncien + 1,
	retractall(tempsJoueur(TypeJoueur,_)),
	retractall(nbCoups(TypeJoueur,_)),
	assert(tempsJoueur(TypeJoueur,TempsNouveau)),
	assert(nbCoups(TypeJoueur,NbCoupsNouveau)),

	testFin(Coup,Y,CouleurJCourant, PartieNulle).


testFin(Coup,Y,CouleurJCourant,PartieNulle) :-
	gagne(Coup,Y,CouleurJCourant),
	PartieNulle=false.
testFin(_,_,_,PartieNulle) :-
	not(coupPossible),
	PartieNulle=true.
testFin(_,_,_,PartieNulle) :-
	changerJoueur,
	tour(PartieNulle).

init :-
	initJeu,
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)).
