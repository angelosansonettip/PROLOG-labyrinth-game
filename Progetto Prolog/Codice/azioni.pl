%####################################################
% applicabile/2 controlla se una determinata azione
% (nord, sud, est, ovest) è possibile effettuarla nella 
% posizione 'pos(R,C)'
%####################################################
applicabile(est, pos(R,C)) :-
  \+num_colonne(C),
  C1 is C+1,
  \+occupato(pos(R,C1)).

applicabile(ovest, pos(R,C)) :-
  C>1,
  C1 is C-1,
  \+occupato(pos(R,C1)).

applicabile(nord, pos(R,C)) :-
  R>1,
  R1 is R-1,
  \+occupato(pos(R1,C)).

applicabile(sud, pos(R,C)) :-
  \+num_righe(R),
  R1 is R+1,
  \+occupato(pos(R1,C)).


%#########################################################
% trasforma/3 restituisce il nuovo stato a partire
% dalla posizione 'pos(R,C)' applicando l'azione specificata
%#########################################################
trasforma(est, pos(R,C), pos(R, CNuovo)) :- CNuovo is C+1.

trasforma(ovest, pos(R,C), pos(R, CNuovo)) :- CNuovo is C-1.

trasforma(nord, pos(R,C), pos(RNuovo,C)) :- RNuovo is R-1.

trasforma(sud, pos(R,C), pos(RNuovo,C)) :- RNuovo is R+1.

%#################################################################
% limiteMax/1 restituisce il numero massimo di azioni effettuabili
% per un dato labirinto. Ciò corrisponde al più al numero delle celle non occupate
%#################################################################
%limiteMax(L) :-
%  num_righe(NR), num_colonne(NC),
%  findall(pos(R, C), occupato(pos(R, C)), CelleOccupate),
%  length(CelleOccupate, NumOccupate),  
%  L is NR * NC - NumOccupate.


