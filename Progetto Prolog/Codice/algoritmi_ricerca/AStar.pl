% ###################################################################
% Algoritmo A*.
% Ciascun nodo è rappresentato dalle seguenti proprietà:
%   • S, posizione del nodo
%   • ListaAzioni, lista di azioni per raggiungere la posizione S
%   • CostoPercorso, costo per raggiungere S dalla posizione iniziale
%   • CostoEuristica, costo stimato dall'euristica per la posizione S
% ###################################################################
start:-  posizioneIniziale(S),
         euristica(S, CostoEuristica),

         ricerca_astar([nodo(S, [], 0, CostoEuristica)], [], Sol),
         write("\nSoluzione: "), 
         write(Sol).
 

% #####################################################################
% ricerca_astar(NodiDaEsplorare, NodiEspansi, ListaAzioni)
% #####################################################################
ricerca_astar([nodo(S, ListaAzioni, _, _)|_], _, ListaAzioni):- posizioneFinale(S).

ricerca_astar([nodo(S, ListaAzioniPerS, CostoPercorso, CostoEuristica)|Frontiera], NodiEspansi, ListaAzioni):-

  findall(Az, applicabile(Az, S), ListaAzioniPermesse),
  generaFigli(nodo(S, ListaAzioniPerS, CostoPercorso, CostoEuristica), ListaAzioniPermesse, NodiEspansi, ListaNodiFigli),

  append(ListaNodiFigli, Frontiera, NuovaFrontiera),
  predsort(criterio_ordinamento_nodi, NuovaFrontiera, NuovaFrontieraOrdinata),

  ricerca_astar(NuovaFrontieraOrdinata, [S|NodiEspansi], ListaAzioni).


% ##################################################################
% generaFigli(nodo, AzioniPermesse, NodiEspansi, ListaNodiFigli)
% ##################################################################
generaFigli(_, [], _, []).
generaFigli(nodo(S, ListaAzioni, CostoPercorso, CostoEuristica),
             [Azione|AltreAzioni], NodiEspansi,
             [nodo(NewS, NewListaAzioni, NewCostoPercorso, NewCostoEuristica)|AltriNodiFigli]):-
  
  trasforma(Azione, S, NewS),
  \+member(NewS, NodiEspansi), % questa istruzione implementa l'operazione di pruning
  
  NewCostoPercorso is CostoPercorso + 1,
  euristica(NewS, NewCostoEuristica),
  append(ListaAzioni, [Azione], NewListaAzioni),

  generaFigli(nodo(S, ListaAzioni, CostoPercorso, CostoEuristica), AltreAzioni, NodiEspansi, AltriNodiFigli),
  !.

% #########################################################################
% Questo predicato serve per far sì che vengano considerate le altre azioni 
% possibili senza far fallire l'algoritmo
% #########################################################################
generaFigli(Node, [_|AltreAzioni], NodiEspansi, ListaNodiFigli) :-
  generaFigli(Node, AltreAzioni, NodiEspansi, ListaNodiFigli),
  !.


% ###################################################
% criterio_ordinamento_nodi/3 serve per effettuare 
% l'ordinamento dei nodi per la lista della frontiera
% ###################################################
criterio_ordinamento_nodi(R, nodo(_, _, CostoP1, CostoH1), nodo(_, _, CostoP2, CostoH2)) :-
  F1 is CostoP1 + CostoH1,
  F2 is CostoP2 + CostoH2,
  F1 >= F2 -> R = > ; R = < .