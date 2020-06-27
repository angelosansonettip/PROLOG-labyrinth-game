%##########################################################
% Viene utilizzata la distanza di Manhattan come euristica
% per il labirinto. Questa euristica ha la proprietà di 
% essere consistente, il che permette il safe pruning.
% Se ci sono più stati finali, viene calcolata l'euristica 
% considerando lo stato finale più vicino
%##########################################################
euristica(pos(R1, C1), MinD) :-

  findall(pos(R, C), posizioneFinale(pos(R, C)), PosizioniFinali),
  calcola_distanze(pos(R1, C1), PosizioniFinali, Distanze),
  sort(Distanze, DistanzeSorted),
  nth0(0, DistanzeSorted, MinD).
  

calcola_distanze(_, [], []).
calcola_distanze(pos(R1, C1), [pos(R2, C2)|AltrePosizioni], [D|Ds]) :-
  D is abs(R1 - R2) + abs(C1 - C2),
  calcola_distanze(pos(R1, C1), AltrePosizioni, Ds).