tamano(8).
pasos(3).

main(EstadoInicial, EstadoFinal):-
		pasos(P),
		camino( P, EstadoInicial, EstadoFinal, [EstadoInicial], Camino ),
		reverse(Camino, Camino1), write(Camino1), write(' con coste '), write(P), nl, halt.

camino( 0, E,E, C,C ).
% Caso base: cuando el estado actual es el estado final.
camino( CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ) :-
		CosteMax > 0,
		unPaso( CostePaso, EstadoActual, EstadoSiguiente ),
		% En B.1 y B.2, CostePaso es 1.
		\+ member( EstadoSiguiente, CaminoHastaAhora ),
		CosteMax1 is CosteMax-CostePaso,
camino(CosteMax1, EstadoSiguiente, EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal).

dentroTablero(F,C):- 
	tamano(N),
	between(1, N, F),
	between(1, N, C).
	
unPaso(1, [F1, C1], [F2, C2]) :-
	between(1, 2, NF),
	between(1, 2, NC),
	Sum is NF+NC,
	Sum is 3,
	F2 is F1+NF,
	C2 is C1+NC,
	dentroTablero(F2, C2).

unPaso(1, [F1, C1], [F2, C2]) :-
	between(1, 2, NF),
	between(1, 2, NC),
	Sum is NF+NC,
	Sum is 3,
	F2 is F1-NF,
	C2 is C1-NC,
	dentroTablero(F2, C2).
	
unPaso(1, [F1, C1], [F2, C2]) :-
	between(1, 2, NF),
	between(1, 2, NC),
	Sum is NF+NC,
	Sum is 3,
	F2 is F1+NF,
	C2 is C1-NC,
	dentroTablero(F2, C2).	

unPaso(1, [F1, C1], [F2, C2]) :-
	between(1, 2, NF),
	between(1, 2, NC),
	Sum is NF+NC,
	Sum is 3,
	F2 is F1-NF,
	C2 is C1+ 	NC,
	dentroTablero(F2, C2).
	
