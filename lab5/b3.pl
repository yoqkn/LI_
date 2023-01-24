main :- EstadoInicial = [[1,2,5,8], 0],
		EstadoFinal = [[],1],
		between(1, 1000, CosteMax),
		% Buscamos solución de coste 0; si no, de 1, etc.
		camino( CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino ),
		reverse(Camino, Camino1), write(Camino1), write(' con coste '), write(CosteMax), nl, halt.

camino( 0, E,E, C,C ).
% Caso base: cuando el estado actual es el estado final.
camino( CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ) :-
		CosteMax > 0,
		unPaso( CostePaso, EstadoActual, EstadoSiguiente ),
		% En B.1 y B.2, CostePaso es 1.
		\+ member( EstadoSiguiente, CaminoHastaAhora ),
		CosteMax1 is CosteMax-CostePaso,
camino(CosteMax1, EstadoSiguiente, EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal).

mayoriaMisioneros(M,C):- (M >= C | M==0) , MR is 3-M, CR is 3-C, (MR >= CR | MR == 0).

concat([],L,L).
concat([X|L1], L2, [X|L]):- concat(L1,L2,L).
pert_con_rest(X, L, R):- concat(L1, [X|L2], L), concat(L1, L2, R).
%Una persona 0 a 1
unPaso(Coste, [Ps1,0], [Ps2,1]) :-
	member(P, Ps1),
	Coste is P,
	pert_con_rest(P,Ps1,Ps2).

%Dos personas 0 a 1
unPaso(Coste, [Ps1,0], [Ps2,1]) :-
	member(P1, Ps1),
	pert_con_rest(P1, Ps1, R),
	member(P2, R),
	pert_con_rest(P2, R, Ps2),
	Coste is max(P1,P2).
	
%Una persona 1 a 0
unPaso(Coste, [Ps1,1], [Ps2,0]) :-
	member(P, [1,2,5,8]),
	\+member(P, Ps1),
	concat([P], Ps1, Ps2),
	Coste is P.

%Dos personas 1 a 0
unPaso(Coste, [Ps1,1], [Ps2,0]) :-
	member(P1, [1,2,5,8]),
	\+member(P1, Ps1),
	concat([P1], Ps1, R),
	member(P2, [1,2,5,8]),
	\+member(P2, R),
	concat([P2], R, Ps2),
	Coste is max(P1,P2).
