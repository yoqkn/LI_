%% 3 points.
%% Complete the following prolog program that writes the optimal path in a topographic map
%% to go from the initial location (marked i) to the final location (marked f).
%% The cost of moving to a location is indicated with a positive integer 1, 2 or 3,
%% and the cost of moving to the final state (marked f) is 1.
%% The path cannot go through rivers, lakes, cliffs, ... (marked with x).


topoMap([                      %   An optimal path is:
            [x,3,1,1,i,x],     %     x       i x 
            [x,2,x,2,1,1],     %     x   x   1 1 
            [x,2,x,x,x,2],     %     x   x x x 2 
            [x,1,x,3,1,1],     %     x   x   1 1 
            [x,1,f,3,1,2],     %     x   f 3 1   
            [x,x,x,2,x,1]      %     x x x   x   
        ] ).                   %   with cost: 1+1+2+1+1+1+3+1(f) = 11
/*
 topoMap([                                  %   An optimal path is:
             [x,x,x,x,x,x,x,x,x,x,x,x],     %     x x x x x x x x x x x x
             [x,1,x,3,2,1,1,1,2,2,2,x],     %     x   x                 x
             [x,2,x,3,2,2,2,x,3,3,f,x],     %     x   x         x     f x
             [x,1,2,3,x,x,x,x,1,3,3,x],     %     x       x x x x     3 x
             [x,2,2,2,x,1,1,x,2,2,2,x],     %     x       x     x     2 x
             [x,1,1,1,x,3,2,3,1,2,2,x],     %     x 1 1 1 x           2 x
             [x,2,3,2,2,1,3,3,1,1,1,x],     %     x 2   2 2 1     1 1 1 x
             [x,2,x,x,x,1,1,2,1,x,x,x],     %     x 2 x x x 1 1 2 1 x x x
             [x,2,2,2,x,1,1,1,1,2,3,x],     %     x 2     x             x
             [x,1,i,1,x,x,x,x,3,2,x,x],     %     x 1 i   x x x x     x x
             [x,2,1,1,1,2,3,x,3,x,x,x],     %     x             x   x x x
             [x,x,x,x,x,x,x,x,x,x,x,x]      %     x x x x x x x x x x x x
         ] ).                               %   with cost: 31
*/

% topoMaxCost(Max): Max is an overestimation of the maximum cost of any path
topoMaxCost(Max) :-
    topoMap(M), length(M,NR), M = [Row1|_], length(Row1,NC),
    Max is NR*NC*3.           %% 3 is the maximum cost of one step

%Estado Inicial
estadoInicial([X,Y]) :-
    topoMap(M), nth1(X,M,Row), nth1(Y,Row,i), !.   %% nth1(N,L,X) means "the Nth element of list L is X"

%Estado Final
estadoFinal([X,Y]) :-
    topoMap(M), nth1(X,M,Row), nth1(Y,Row,f), !.   %% nth1(N,L,X) means "the Nth element of list L is X"

%estadoActual y Pos su valor marcado
estadoActual([X,Y], Pos) :-
    topoMap(M), nth1(X,M,Row), nth1(Y,Row,Pos), !.

main :-
    estadoInicial(EstadoInicial),              % Asignamos la posicion incial
    estadoFinal(EstadoFinal),                  % Asignamos la posicion objetivo
    topoMaxCost(Max),                          % Asignamos el coste maximo que puede llegar a tener
    between( 1, Max, CosteMax ),               % Buscamos solucion de coste 1; si no, de 2, etc.
    camino( CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino ),
    displaySol( Camino ), nl,
    write('with cost: '), write(CosteMax), nl, halt.


camino( 0, E, E, C, C ).
camino( CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ) :-
    CosteMax > 0,
    unPaso( CostePaso, EstadoActual, EstadoSiguiente ),
    \+ member(EstadoSiguiente, CaminoHastaAhora),
    CosteMax1 is CosteMax-CostePaso,
    camino( CosteMax1, EstadoSiguiente, EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal ).


%% costPos(E, C): the cost of moving to a topoMap location with content E is C.
%% May be used in the predicate unPaso
costPos(f, 1) :- !.  % move to the final state (f): cost 1.
costPos(E, E).       % move to a normal position in the topoMap: cost 1, 2 or 3

%Un paso hacia izquierda
unPaso(Cost, [X1,Y], [X2,Y]):-
    insideTopoMap([X1,Y]),
    insideTopoMap([X2,Y]),
    estadoActual([X2,Y],Pos),
    Pos \= x,
    X2 is X1-1,
    costPos(Pos, Cost).

%Un paso hacia derecha
unPaso(Cost, [X1,Y], [X2,Y]):-
    insideTopoMap([X2,Y]),      % Comprobar el estadoSiguiente sea valido
    estadoActual([X2,Y],Pos),
    Pos \= x,                   % Comprobar el estadoSiguiente no sea x
    X2 is X1+1,
    costPos(Pos, Cost).

%Un paso hacia abajo
unPaso(Cost, [X,Y1], [X,Y2]):-
    insideTopoMap([X,Y2]),      % Comprobar el estadoSiguiente sea valido
    estadoActual([X,Y2],Pos),
    Pos \= x,                   % Comprobar el estadoSiguiente no sea x
    Y2 is Y1-1,
    costPos(Pos, Cost).

%Un paso hacia arriba
unPaso(Cost, [X,Y1], [X,Y2]):-
    insideTopoMap([X,Y2]),      % Comprobar el estadoSiguiente sea valido
    estadoActual([X,Y2],Pos),
    Pos \= x,                   % Comprobar el estadoSiguiente no sea x
    Y2 is Y1+1,
    costPos(Pos, Cost).

%Comprobar la posicion que accedimos es valido
insideTopoMap([X,Y]):-
    topoMap(M),
    length(M, NRows),           % NRows es el numero de filas de topoMap
    nth1(1,M,Row),
    length(Row, NCols),         % NCols es el numero de columnas de topoMap
    between(1, NRows,Y),
    between(1, NCols,X).


displaySol(Camino) :-
    topoMap(M), nth1(X,M,Row), nl, nth1(Y,Row,E), writePos(E,X,Y,Camino), fail.
displaySol(_).

writePos(E, _, _, _     ) :- member(E, [i,f,x]),    write(E), write(' '), !.
writePos(E, X, Y, Camino) :- member([X,Y], Camino), write(E), write(' '), !.
writePos(_, _, _, _     ) :- write('  '), !.

