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

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% runTest/3
% NbIterations: le nombre de parties à jouer,
% IA1 et IA2 : les identifiants des 2 IA à confronter
% IA1 joue contre IA2 "NbIterations" fois le predicat affiche combien de fois qui a battu qui
runTest(NbIterations,IA1,IA2) :-
	NbIterationsParIA is NbIterations//2,
	runTestIAXEnPremier(NbIterationsParIA,IA1,IA2,0,NbFoisIA1GagneEnCommencant,0,NbFoisIA1PerdEnCommencant,0,NbEgaliteIA1),
	runTestIAXEnPremier(NbIterationsParIA,IA2,IA1,0,NbFoisIA2GagneEnCommencant,0,NbFoisIA2PerdEnCommencant,0,NbEgaliteIA2),
	typeJoueur(IA1,TypeIA1),
	typeJoueur(IA2,TypeIA2),

	% write(Stream, TypeIA2), write(Stream, ' en commençant : a gagné '), write(Stream, NbFoisIA2GagneEnCommencant),write(Stream, ' fois et a perdu '),write(Stream, NbFoisIA2PerdEnCommencant),write(Stream, ' fois.\n'),
	% write(Stream, TypeIA1), write(Stream, ' en commençant : a gagné '), write(Stream, NbFoisIA1GagneEnCommencant),write(Stream, ' fois et a perdu '),write(Stream, NbFoisIA1PerdEnCommencant),write(Stream, ' fois.\n'),
	
	write(TypeIA2), write(' en commençant : a gagné '), write(NbFoisIA2GagneEnCommencant),write(' fois et a perdu '),write(NbFoisIA2PerdEnCommencant),write(' fois.'),
	nl,
	write('Il y a eu '), write(NbEgaliteIA2),write(' egalites pour IA2'),nl,
	write(TypeIA1), write(' en commençant : a gagné '), write(NbFoisIA1GagneEnCommencant),write(' fois et a perdu '),write(NbFoisIA1PerdEnCommencant),write(' fois.'),nl,
	write('Il y a eu '), write(NbEgaliteIA1),write(' egalites pour IA1'),
	!.

runTest2(NbIterations,IA1,IA2) :-
	NbIterationsParIA is NbIterations//2,
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
	
	write(TypeIA2), write(' en commençant : a gagné '), write(NbFoisIA2GagneEnCommencant),write(' fois et a perdu '),write(NbFoisIA2PerdEnCommencant),write(' fois.'),
	nl,
	write('Il y a eu '), write(NbEgaliteIA2),write(' egalites pour IA2'),nl,
	write(TypeIA1), write(' en commençant : a gagné '), write(NbFoisIA1GagneEnCommencant),write(' fois et a perdu '),write(NbFoisIA1PerdEnCommencant),write(' fois.'),nl,
	write('Il y a eu '), write(NbEgaliteIA1),write(' egalites pour IA1'),
	close(Stream),
	!.

%Run test with time(:Goal) to see the time it takes to run the test
% runTestWithTime(NbIterations,IA1,IA2) :-
% 	time(runTest(NbIterations,IA1,IA2)).

%Run test with two similar IAs to find the mean time it takes to play a game
runTestWithTimeForMean(NbIterations,IA1,IA2) :-
	time(runTestForMean(NbIterations,IA1,IA2)).
	
% Run test with two similar IAs to find the mean time it takes to play a game
% Pour l'instant, c'est juste le temps moyen d'une IA, il faut faire le temps au cours d'une partie pour chaque IA
runTestForMean(NbIterations,IA1,IA2) :-
	statistics,
	statistics(walltime, [Start,_]),
	runTest(NbIterations,IA1,IA2),
	statistics(walltime, [End,_]),
	Time is End - Start,
	MeanTime is Time/NbIterations,
	write('\nMean time for one game : '), write(MeanTime), write(' ms'), nl.

	
%Run test with statistics(:Goal) and shown the output of the statistics : thread_cputime, cputime, trail, process_epoch, memory into the result.txt
runTestWithStatisticsShort(NbIterations,IA1,IA2) :-
	%ouverture fichier et initialisation message
	open('result2.txt',append,Stream),
	% write(Stream,'Exécution du test avec les statistiques suivantes : '),
	% %description de l'IA1 et de l'IA2
	% write(Stream,'\nMatch de IA1 : '),
	% typeJoueur(IA1,TypeIA1),
	% write(Stream,TypeIA1),
	% write(Stream,' contre IA2 : '),
	% typeJoueur(IA2,TypeIA2),
	% write(Stream,TypeIA2),
	
	statistics,
	statistics(walltime, [Start,_]),
	runTest2(NbIterations,IA1,IA2),
	statistics(walltime, [End,_]),
	Time is End - Start,
	statistics,
	statistics(cputime,CpuTime),
	statistics(threads, Threads),
	%affichage des statistiques
	write(Stream, 'Execution Time : '), write(Stream, Time), write(Stream, ' ms.'),
	write(Stream, '\nNb de thread actifs : '), write(Stream, Threads),
	write(Stream, '\nCpuTime : '), write(Stream,CpuTime), write(Stream, ' s'),
	write(Stream, '\n\n'),
	close(Stream).

%Test avec runTestWithStatisticsShort de toutes les IA contre toutes les autres, avec 10 parties par match
% testAllIA :-
% 	runTestWithStatisticsShort(10,2,3),
% 	runTestWithStatisticsShort(10,2,4),
% 	runTestWithStatisticsShort(10,2,5),
% 	runTestWithStatisticsShort(10,2,6),
% 	runTestWithStatisticsShort(10,2,7),
% 	runTestWithStatisticsShort(10,2,8),
% 	runTestWithStatisticsShort(10,3,4),
% 	runTestWithStatisticsShort(10,3,5),
% 	runTestWithStatisticsShort(10,3,6),
% 	runTestWithStatisticsShort(10,3,7),
% 	runTestWithStatisticsShort(10,3,8),
% 	runTestWithStatisticsShort(10,4,5),
% 	runTestWithStatisticsShort(10,4,6),
% 	runTestWithStatisticsShort(10,4,7),
% 	runTestWithStatisticsShort(10,4,8),
% 	runTestWithStatisticsShort(10,5,6),
% 	runTestWithStatisticsShort(10,5,7),
% 	runTestWithStatisticsShort(10,5,8),
% 	runTestWithStatisticsShort(10,6,7),
% 	runTestWithStatisticsShort(10,6,8),
% 	runTestWithStatisticsShort(10,7,8).


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
	joueurCourant(CouleurJCourant,TypeJoueur),
	obtenirCoup(CouleurJCourant,TypeJoueur,Coup),
	placerJeton(Coup,Y,CouleurJCourant),
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
