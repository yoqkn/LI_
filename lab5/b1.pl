main :- EstadoInicial = [3,3,0],
		EstadoFinal = [0,0,1],
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

unPaso(1, [M1,C1,0], [M2, C2,1]) :-
	between(0, 2, NumM),
	between(0, 2, NumC),
	Sum is NumM+NumC,
	between(1,2, Sum),
	M2 is M1-NumM,
	C2 is C1-NumC,
	between(0,3,M2),
	between(0,3,C2),	
	mayoriaMisioneros(M2,C2).

unPaso(1, [M1,C1,1], [M2, C2,0]) :-
	between(0, 2, NumM),
	between(0, 2, NumC),
	Sum is NumM+NumC,
	between(1,2, Sum),
	M2 is M1+NumM,
	C2 is C1+NumC,
	between(0,3,M2),
	between(0,3,C2),	
	mayoriaMisioneros(M2,C2).	
