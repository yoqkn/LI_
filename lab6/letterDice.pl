:- use_module(library(clpfd)).

%% A (6-sided) "letter dice" has on each side a different letter.
%% Find four of them, with the 24 letters abcdefghijklmnoprstuvwxy such
%% that you can make all the following words: bake, onyx, echo, oval,
%% gird, smug, jump, torn, luck, viny, lush, wrap.

%Some helpful predicates:

word( [b,a,k,e] ).
word( [o,n,y,x] ).
word( [e,c,h,o] ).
word( [o,v,a,l] ).
word( [g,i,r,d] ).
word( [s,m,u,g] ).
word( [j,u,m,p] ).
word( [t,o,r,n] ).
word( [l,u,c,k] ).
word( [v,i,n,y] ).
word( [l,u,s,h] ).
word( [w,r,a,p] ).

num(X,N):- nth1( N, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,v,w,x,y], X ). %num N-éssim de la llista -> lletra X

%24 lletres repartides en 4 daus de 6 cares.
main:-
    %1: Vars + Domini:
    length(D1,6),
    length(D2,6),
    length(D3,6),
    length(D4,6),
    append([D1,D2,D3,D4], Vars),    %concatenar totes les cares de 4 daus
    Vars ins 1..24,

    %2: Constraints
    % restriccions NO OBLIGATÒRIES
    % lletres ordenades
    sorted(D1),
    sorted(D2),
    sorted(D3),
    sorted(D4),
    
    D1=[D11|_], D11 #= 1, %1 and 2 (a, b) are not int the sme dice due to [bake]
    D2=[D21|_], D21 #= 2, 
    D3=[D31|_],
    D4=[D41|_], D31 #< D41,
    
    % restriccions OBLIGATÒRIES   
    all_distinct(Vars),
    computeIncompatiblePairs(L),    % L es la lsita de parejas de letras que no han de salir
                                    % en el mismo dado (sense parelles repetides)    
    makeConstraints(L, D1),
    %makeConstraints(L, D2),
    %makeConstraints(L, D3),
    %makeConstraints(L, D4),
    

    
    %3: labeling    
    labeling([ff], Vars),
    %4: Escrivim solucion
    
    writeN(D1), 
    writeN(D2), 
    writeN(D3), 
    writeN(D4), halt.
    
%all_distinct(L):- mem

sorted([]).
sorted([_]).
sorted([A,B|L]):- A #< B, sorted([B|L]).

computeIncompatiblePairs(L):-  findall(N-M, notInSameDice(N,M), L).

notInSameDice(N,M):- N #< M, num(A, N), num(B, M), word(W), member(A, W), member(B, W).

makeConstraints([], [_,_,_,_,_,_]).
makeConstraints([L1-L2|L], [A,B,C,D,E,F]):- (A #\= L1 #\/ B #\= L2), 
                                            (A #\= L1 #\/ C #\= L2),
                                            (A #\= L1 #\/ D #\= L2),
                                            (A #\= L1 #\/ E #\= L2),
                                            (A #\= L1 #\/ F #\= L2), 
                                            (B #\= L1 #\/ C #\= L2),
                                            (B #\= L1 #\/ D #\= L2),
                                            (B #\= L1 #\/ E #\= L2),
                                            (B #\= L1 #\/ F #\= L2),
                                            (C #\= L1 #\/ D #\= L2),
                                            (C #\= L1 #\/ E #\= L2),
                                            (C #\= L1 #\/ F #\= L2),
                                            (D #\= L1 #\/ E #\= L2),
                                            (D #\= L1 #\/ F #\= L2),
                                            (E #\= L1 #\/ F #\= L2),
                                            makeConstraints(L, [A,B,C,D,E,F]), !.


writeN(D):- findall(X,(member(N,D),num(X,N)),L), write(L), nl, !.

