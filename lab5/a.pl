subcjto([],[]). %subcjto(L,S) es: "S es un subconjunto de L".
subcjto([X|C],[X|S]):-subcjto(C,S).
subcjto([_|C],S):-subcjto(C,S).

concat([],L,L).
concat([X|L1], L2, [X|L]):- concat(L1,L2,L).
pert_con_resto(X,L,R):- concat(L1, [X|L2], L), concat(L1,L2,R).

permutacion([],[]).
permutacion(L, [X|P]):- pert_con_resto(X, L, R), permutacion(R,P).



cifras(L,N):- 
	length(L,Len),
	between(1, Len, Size),
	subcjto(L,S), 
	length(S,Size),
	permutacion(S,P), 
	expresion(P,E),
	N is E, 
    write(E), write('------------'),write(Size),
	nl,fail.
	
expresion([X],X).
expresion(L,E1+E2):- 
	concat(L1,L2,L), L1\=[],L2\=[],
	expresion(L1,E1), expresion(L2,E2).
expresion(L,E1-E2):- 
	concat(L1,L2,L), L1\=[],L2\=[],
	expresion(L1,E1), expresion(L2,E2).
expresion(L,E1*E2):- 
	concat(L1,L2,L), L1\=[],L2\=[],
	expresion(L1,E1), expresion(L2,E2).
expresion(L, E1/E2):- 
	E2 \= 0,
	M is mod(E1,E2), 
	M is 0,
 	concat(L1,L2,L), L1\=[],L2\=[],
	expresion(L1,E1), expresion(L2,E2).
