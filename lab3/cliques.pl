
% 4 points.
% Given a graph declared as in the example below, write all its cliques of size at least minCliqueSize.
% Remember, a clique is a complete subgraph: a subset S of the vertices such that for all U,V in S there is an edge U-V.
% For the example below, a correct output would be the following (or in another order):
% [2,4,5,7,9]
% [2,4,5,7]
% [2,4,5,9]
% [2,4,7,9]
% [2,4,8,9]
% [2,5,7,9]
% [4,5,7,9]

%%==== Example: ========================================================
numVertices(10).
minCliqueSize(4).
vertices(Vs):- numVertices(N), findall(I,between(1,N,I),Vs).
vertex(V):- vertices(Vs), member(V,Vs).
edge(U,V):- edge1(U,V).
edge(U,V):- edge1(V,U).

edge1(9,8).
edge1(8,2).
edge1(7,4).
edge1(5,7).
edge1(4,2).
edge1(5,2).
edge1(2,7).
edge1(7,9).
edge1(2,9).
edge1(4,8).
edge1(4,9).
edge1(9,5).
edge1(4,5).
%%==========================================================

main:- vertices(Vs), subconjunto(Vs, S), minCliqueSize(MIN), numVertices(MAX), length(S, SIZE), between(MIN, MAX, SIZE), isClique(S),  write(S), nl, fail.
main:- halt.

subconjunto([], []).
subconjunto([V|Vs], [V|S]):- subconjunto(Vs, S).
subconjunto([_|Vs], S):- subconjunto(Vs, S).

isClique([]).
isClique([_]).
isClique([U|S]):- findall(V, (edge(U,V), member(V, S)), Lits), length(S, N), length(Lits, N), isClique(S), !.