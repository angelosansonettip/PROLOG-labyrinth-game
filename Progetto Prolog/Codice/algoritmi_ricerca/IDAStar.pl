:- dynamic nodoRaggiungibileIDAStar/1.
:- dynamic numNodiVisitati/1.

% ########################################################
% Algoritmo IDA*.
% Il nodo è rappresentato dalla seguente struttura:
%   • S, rappresenta la posizione del nodo
%   • FS, rappresenta l'F-value del nodo (Costo percorso + costo euristica)
% ########################################################
start:- 
        posizioneIniziale(S),
        assert(nodoRaggiungibileIDAStar(S)),
        assert(numNodiVisitati(1)),

        euristica(S, LimiteIniziale),
        assert(limite(LimiteIniziale)),

        restituisci_limite_idastar(Limite),
        ricerca_idastar(S, Sol, [S], 0, Limite),
        write("\nSoluzione: "), 
        write(Sol).
   

% ###################################################################
% Questo predicato controlla se sono stati esplorati nuovi nodi.
% Se ciò non è avvenuto, è inutile procedere con in quanto
% l'uscita non è raggiungibile. Nel caso viene aggiornato il numero di nodi visitati
% ###################################################################
restituisci_limite_idastar(Limite):- limite(Limite).
restituisci_limite_idastar(Limite):-
    checkFineIDAStar, 
    ricalcolaLimiteIdaStar,
    restituisci_limite_idastar(Limite).

% ###################################################################
% Questo predicato controlla se siano stati esplorati nuovi nodi;
% se ciò non è avvenuto, è inutile procedere con l'esplorazione in quanto
% l'uscita non è accessibile. Inoltre si aggiorna il numero di nodi visitati
% ###################################################################
checkFineIDAStar :-
    numNodiVisitati(NodiVisitatiPrecedente),
  
    findall(S, nodoRaggiungibileIDAStar(S), ListaS),
    length(ListaS, NodiVisitatiAttuale),
    NodiVisitatiAttuale > NodiVisitatiPrecedente,
  
    retract(numNodiVisitati(NodiVisitatiPrecedente)),
    assert(numNodiVisitati(NodiVisitatiAttuale)).


% ###################################################################
% Questo predicato provvede ad aggiornare il limite oltre il quale
% la ricerca deve fermarsi. Si badi bene che il limite potrebbe 
% rimanere sempre lo stesso, qualora l'algoritmo si sia avvicinato senza intoppi
% verso l'uscita.
% ###################################################################
ricalcolaLimiteIdaStar :-
    limite(VecchioLimite),

    findall(FS, nodoSuperante(FS), ListaFS),
    sort(ListaFS, SortedListaFS),
    nth0(0, SortedListaFS, NuovoLimite),

    retractall(nodoSuperante(_)),
    retract(limite(VecchioLimite)),

    assert(limite(NuovoLimite)).

% ##################################################################################
% ricerca_idastar/5 (StatoPartenza, AzioniPossibili, NodiVisitati, CostoPercorsoS, Limite)
% ##################################################################################
ricerca_idastar(S, [], _, _, _):- posizioneFinale(S).

ricerca_idastar(S, [Azione|AltreAzioni], NodiVisitati, CostoPercorsoS, Limite):-
    applicabile(Azione, S),
    trasforma(Azione, S, NewS), 
    \+member(NewS, NodiVisitati),
    checkNodoRaggiungibileIDAStar(NewS),

    NewCostoPercorsoS is CostoPercorsoS + 1,
    euristica(NewS, NewEuristicaS),
    FNewS is NewCostoPercorsoS + NewEuristicaS,
    
    (
     (FNewS =< Limite), ricerca_idastar(NewS, AltreAzioni, [NewS|NodiVisitati], NewCostoPercorsoS, Limite); 
     (FNewS > Limite), assert(nodoSuperante(FNewS)), fail
    ).


% Si memorizza il fatto di aver visitato questo nodo, restituendo sempre true
checkNodoRaggiungibileIDAStar(NewS) :-
    \+nodoRaggiungibileIDAStar(NewS),
    assert(nodoRaggiungibileIDAStar(NewS)), !.

checkNodoRaggiungibileIDAStar(_) :- true.