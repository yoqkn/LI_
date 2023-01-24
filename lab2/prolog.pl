%1. P es el producte dels elements de la llista
prod([],1).
prod([X|L],P):- prod(L,S), P is X*S.

%2.  P es el producte escalar L1 i L2
pescalar([],[],0).
pescalar([X|L1],[Y|L2],P):- pescalar(L1,L2,NP),P is NP+X*Y.

%3. interseccion y unión
interseccion(_,[],[]):-!.
interseccion(L1,[X|L2],[X|I]):- member(X,L1),!,interseccion(L1,L2,I).   %X pert a L1
interseccion(L1,[_|L2],I):- interseccion(L1,L2,I).  %X no pert L1

union([],L,L).
union([X|L1],L2,L):- member(X,L2),!, union(L1, L2, L). %Si X forma parte de L2 eliminamos X de la L1
union([X|L1],L2,[X|L]):- union(L1,L2,L).

%4. append -> calcular el último elemento y la lista inversa
lastElement(L,X):- append(_,[X],L).

inverseList([],[]).
inverseList([X|L],I):- append(I2,[X],I), inverseList(L,I2), !.

%5. F es el N- ́esimo n ́umero de Fibonacci para la N dada
fib(1,1).
fib(2,1).
fib(N,F):- N > 2, N1 is N-1, N2 is N-2, fib(N1,F1), fib(N2, F2), F is F1+F2.

%6. dados(P,N,L) que signifique: “la lista L expresa una manera de sumar P puntos lanzando N dados”
% P es numero de punts, N num dados, L solución
dados(0,0,[]).
dados(P,N,[X|L]):-  member(X, [1,2,3,4,5,6]), N > 0, N1 is N-1, dados(P1,N1,L), P is P1+X.

%7. Escribe un predicado suma demas(L) que, dada una lista de enteros L, se satisface si existe algún elemento en L que es igual a la suma de los demás elementos de L, y falla en caso contrario.
conca([],L,L).
conca([X|L1],L2,[X|C]):- conca(L1,L2,C).
 
resta(X, L, R):- conca(L1, [X|L2], L), conca(L1,L2,R). %L1 llista que conté els elements que pertany a l'esquerra de X, i L2 dreta.

suma(0,[]).
suma(S,[X|L]):- suma(S1, L), S is S1+X.

suma_demas(L):- member(X,L), resta(X,L,R), suma(S,R), S = X.

%8. Escribe un predicado suma ants(L) que, dada una lista de enteros L, se satisface si existe algún elemento en L que es igual a la suma de los elementos anteriores a él en L, y falla en caso contrario.
suma_ants(L):-concat(L1,[X|_], L), writeln(X), suma(S,L1), S=X.

%9. Escribe un predicado card(L) que, dada una lista de enteros L, escriba la lista que, para cada
%elemento de L, dice cuántas veces aparece este elemento en L. Por ejemplo, si hacemos la consulta
%card( [1,2,1,5,1,3,3,7] ) el intérprete escribirá:
%[[1,3],[2,1],[5,1],[3,2],[7,1]].
car([],[]).
car([X|L],[[X,N]|CR]):- car(L,C), resta([X,N1], C, CR), write(X),!,N is N1+1.
car([X|L],[[X,1]|CR]):- car(L,CR).

card(L):-car(L,C),write(C).

%10. Escribe un predicado esta ordenada(L) que signifique: “la lista L de números enteros está
%ordenada de menor a mayor”. Por ejemplo, a la consulta:
%?-esta ordenada([3,45,67,83]).
%el intérprete responde yes, y a la consulta:
%?-esta ordenada([3,67,45]).
%responde no.
esta_ordenada([]).
esta_ordenada([_]):- !.
esta_ordenada([X1,X2|L]):- X1 =< X2, esta_ordenada([X2|L]).


%11. Escribe un predicado ord(L1,L2) que signifique: “L2 es la lista de enteros L1 ordenada de
%menor a mayor”. Por ejemplo: si L1 es [4,5,3,3,2] entonces L2 será [2,3,3,4,5]. Hazlo en
%una lı́nea, usando sólo los predicados permutacion y esta ordenada.

permutacion([],[]).
permutacion(L,[X|P]) :- resta(X,L,R), permutacion(R,P).

ord(L1,L2):- permutacion(L1, L2), esta_ordenada(L2),!.


%12. Escribe un predicado diccionario(A,N) que, dado un alfabeto A de sı́mbolos y un natural N,
%escriba todas las palabras de N sı́mbolos, por orden alfabético (el orden alfabético es según el
%alfabeto A dado). Por ejemplo, diccionario( [ga,chu,le],2) escribirá:
%gaga gachu gale chuga chuchu chule lega lechu lele.

diccionario(A,N):-dic(A,N,L), escribir(L).

dic(_,0,[]):-!.
dic(A,N,[X|L]):- N1 is N-1, member(X,A), dic(A,N1,L).

escribir([]):- write(' '),!.
escribir([X|L]):- write(X), escribir(L),fail.

%diccionario(A,N):- N > 0, N1 is N-1, member(X,A), dic(X,A,N1).
%dic(X,A,1):- member(Y,A), write(X),write(Y).

%13. Escribe un predicado palindromos(L) que, dada una lista de letras L, escriba todas las per-
%mutaciones de sus elementos que sean palı́ndromos (capicúas). Por ejemplo, con la consulta
%palindromos([a,a,c,c]) se escribe [a,c,c,a] y [c,a,a,c].
palindromos(L):- permutacion(L,P),palind(P), write(P),nl,fail.

palind([]).
palind([_]):-!.
palind([X|L]):- conca(R,[X],L), palind(R).

%14. Encuentra mediante un programa Prolog, usando el predicado permutación, qué 8 dı́gitos difer-
%entes tenemos que asignar a las letras S, E, N, D, M, O, R, Y, de manera que se cumpla la suma
%siguiente:

send_more_money1 :-
	L = [S, E, N, D, M, O, R, Y, _, _],
	permutacion(L, [0,1,2,3,4,5,6,7,8,9]),
	sumaLista([D,N,E,S],[E,R,O,M], [Y,E,N,O],0,M), 
	write('S = '), write(S), nl,
	write('E = '), write(E), nl,
	write('N = '), write(N), nl,
	write('D = '), write(D), nl,
	write('M = '), write(M), nl,
	write('O = '), write(O), nl,
	write('R = '), write(R), nl,
	write([M,O,N,E,Y]), nl.


		
sumaLista([],[],[],C,C).
sumaLista([X|L1],[Y|L2],[Z|L3],Cin,Cout):- 
	Z is (X+Y+Cin) mod 10,	%residuo 
	C is (X+Y+Cin) // 10,	
	sumaLista(L1,L2,L3,C,Cout).
	
% 15. Escribe un predicado simplifica que pueda usarse en combinación con el programa de calcular derivadas.

%16. dom(L) 
cadena(_, []):-!.
cadena(C, [X|L]):- X = (A,B), A is C, cadena(B,L).

dom1(L):- member(X,L), X = (A,_), permutacion(L,P), 	union([X],_,P), cadena(A,P), write(P),!.
dom1(_):- write("No hay cadena").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
p([],[]).
p(L,[X|P]) :- select(X,L,R), p(R,P).

ok([]):-!.
ok([_]):-!.
ok([X|P]):- X = (_,B), P = [Y|_], Y = (B,_), ok(P). 
dom(L) :- p(L,P), ok(P), write(P), !,nl.
dom(_) :- write("no hay cadena"), nl.

%(a) ¿Qué significa el predicado p(L,P) para una lista L dada? es la permutación de las fichas
%(b) Escribe el predicado ok(P) que falta.

%17. Complete the following backtracking procedure for SAT in Prolog. Program everything, except
%the predicate readclauses(F), which reads a list of clauses, where each clause is a list of %integers. %For example, p 3 ∨ ¬p 6 ∨ p 2 is represented by [3,-6,2]. Do things as simple as possible.
p:- readclauses(F), sat([],F).
p:- write('UNSAT'),nl.
sat(I,[]):- write('IT IS SATISFIABLE. Model: '), write(I),nl,!.
sat(I,F):-
	decision_lit(F,Lit), % Select unit clause if any; otherwise, an arbitrary one.
	simplif(Lit,F,F1), % Simplifies F. Warning: may fail and cause backtracking
	sat( [I|Lit], F1).

decision_lit(F,Lit):- member(Lit,F).
decision_lit(_,1).
simplif(Lit,F,F1):- Lit \= 1, resta(F,Lit,F1),!.
simplif(1,_,_):- fail,!.
