%3. El tall elimina l'exploració posterior de l'objectiu però també backtracking dels subobjectius anteriors de l'objectiu.
%Exemple

p(1).
p(2).

q(a).
q(b).
r(3,4,5).
r(X,Y,Z):- p(X),q(Y),!,s(Z).
%el tall ! treu de la pila les alternatives per a p(),q() i r().
r(5,6,7).

s(3).
s(4).

%DISJUNCIÓ

%Suposem que volem definir la propietat "X és pare o mare de Y", diguem-ne progenitor(X,Y(

progenitor(X,Y):-pare(X,Y).
progenitor(X,Y):-mare(X,Y).

%disjunció
progenitor(X,Y):-pare(X,Y);mare(X,Y).

%operadors lògics
and(A,B):- A, B.
or(A,B) :- A; B.

neg(A):- A, !, false. %Si A és cert amb operador de tall no farà exploració d'A cap avall. Si A és cert retorna false.
neg(_). %Si no operador retorna cert.

implies(A,B) :- A, !, B. %només no ha de cumplir si A cert i B fals. Com que tenim operador no eexploren el següent linea de implies.
implies(_,_).

interval(A,B,[]):- B < A, !.

interval(A,B,[A|X]):- C is A+1, interval(C,B,X).

rang(A,B):- interval(A,B,X), write(X).
