:- dynamic nodoRaggiungibileID/1.
:- dynamic numNodiVisitati/1.

% ###########################################
% Algoritmo Iterative Deepening senza l'uso di euristiche
% ###########################################
start:- 
  posizioneIniziale(S),  
  assert(nodoRaggiungibileID(S)),
  assert(numNodiVisitati(1)),

  restituisci_limite_id(L),

  ricerca_id(S, [S], Sol,L),  
  write("\nSoluzione: "),
  write(Sol).
 

% ###################################################
% Questo predicato permette di restituire un limite
% che cresce di una unità alla volta, a meno che non si sia raggiunto il limite
% ###################################################
restituisci_limite_id(L):- L is 1.
restituisci_limite_id(L):- 
    checkFineID, !,
    restituisci_limite_id(SubL), 
    L is SubL + 1.

% ###################################################################
% Questo predicato controlla se sono stati esplorati nuovi nodi.
% Se ciò non è avvenuto, è inutile procedere con in quanto
% l'uscita non è raggiungibile. Nel caso viene aggiornato il numero di nodi visitati
% ###################################################################
checkFineID :-
  numNodiVisitati(NodiVisitatiPrecedente),

  findall(S, nodoRaggiungibileID(S), ListaS),
  length(ListaS, NodiVisitatiAttuale),
  NodiVisitatiAttuale > NodiVisitatiPrecedente,

  retract(numNodiVisitati(NodiVisitatiPrecedente)),
  assert(numNodiVisitati(NodiVisitatiAttuale)).
  

% ###################################################
% ricerca_id/4 (NodoFrontiera, NodiVisisati, AzioniDaEffettuare, NumeroUlterioriEspansioni)
% ###################################################
ricerca_id(S, _, [], _):- posizioneFinale(S).

ricerca_id(S, NodiVisitati, [Azione|AzioniDaEffettuare], N):-
  N > 0,
  applicabile(Azione, S),
  trasforma(Azione, S, NewS),
  \+member(NewS, NodiVisitati),
  checkNodoRaggiungibileID(NewS),

  N1 is N-1,
  ricerca_id(NewS, [NewS|NodiVisitati], AzioniDaEffettuare, N1).


% Si memorizza il fatto di aver visitato questo nodo, restituendo sempre true
checkNodoRaggiungibileID(NewS) :-
  \+nodoRaggiungibileID(NewS),
  assert(nodoRaggiungibileID(NewS)), !.

checkNodoRaggiungibileID(_) :- true.