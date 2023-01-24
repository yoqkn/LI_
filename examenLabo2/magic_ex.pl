:- use_module(library(clpfd)).

% Complete the following program p(N) that writes a (kind of) magic
% square: an NxN matrix with ALL the numbers 1..N^2, such that the
% sum of every row and every column is equal.
% More precisely, this sum is (N + N^3) / 2.
% Note: don't worry if your program is (too) slow when N >= 6.

%% Example:  (this solution is not unique):
%%
%%    1   2  13  24  25
%%    3   8  18  17  19
%%   16  14  15   9  11
%%   22  20   7  10   6
%%   23  21  12   5   4

main:- p(5), nl, halt.

p(N):-
    NSquare is N*N,
    length( Vars, NSquare ),
    ...
    squareByRows(N,Vars,SquareByRows),
    transpose( SquareByRows, SquareByCols ),  % transpose already exists: no need to implement it
    Sum is (N + N*N*N) // 2,
    constraintsSum( Sum, SquareByRows),
    ...
    writeSquare(SquareByRows),nl,!.


squareByRows(_,[],[]):-!.
squareByRows(N,Vars,[Row|SquareByRows]):- append(Row,Vars1,Vars), length(Row,N), squareByRows(N,Vars1,SquareByRows),!.

writeSquare(Square):- member(Row,Square), nl, member(N,Row), write4(N), fail.
writeSquare(_).

write4(N):- N<10,   write('   '), write(N),!.
write4(N):- N<100,  write('  ' ), write(N),!.
write4(N):-         write(' '  ), write(N),!.
