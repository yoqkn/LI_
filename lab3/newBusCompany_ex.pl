symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.

%% A bus company operates services between a set of different cities.
%% We want to design the routes of the buses for a given week.
%% Every day a bus does exactly one traject (according to the trajects
%% given in the input) from one city to another one, and departs from
%% there for the next traject the next day. Each traject takes place
%% at most once per day, and at least a given minimal amount of days
%% per week.  No bus can travel more than a total of a given amount of
%% kms on two consecutive days.

%%%%%% Example input:
numBuses(8).
numDays(7).
maxDistance(1200).  %this is the maximal total distance for two consecutive days for a given bus

%
traject(mad,bcn,4,630).  % the traject mad-bcn has to take place at least 4 days per week, and is 630 km long.
traject(mad,val,1,360).
traject(mad,bil,1,400).
traject(mad,zar,3,320).
traject(mad,sev,1,450).
%
traject(bcn,mad,4,630).
traject(bcn,val,1,350).
traject(bcn,bil,2,610).
traject(bcn,zar,2,320).
traject(bcn,sev,1,800).
%
traject(val,mad,3,360).
traject(val,bcn,1,350).
traject(val,bil,1,610).
traject(val,zar,3,310).
traject(val,sev,1,700).
%
traject(bil,mad,4,400).
traject(bil,bcn,1,610).
traject(bil,val,1,610).
traject(bil,zar,1,300).
traject(bil,sev,1,900).
%
traject(zar,mad,1,320).
traject(zar,bcn,3,320).
traject(zar,val,1,310).
traject(zar,bil,1,300).
traject(zar,sev,1,800).


%%%%%% Some helpful definitions to make the code cleaner:

day(D):-                    numDays(N),between(1,N,D).
consecutiveDays(D1,D2):-    day(D1), day(D2), D2 is D1+1.
bus(B):-                    numBuses(N), between(1,N,B).
trip(C1-C2):-               traject(C1,C2,_,_).
dist(C1-C2,Dist):-          traject(C1,C2,_,Dist).
frequency(C1-C2,F):-        traject(C1,C2,F,_).
tooLongDist(C1-C2-C3):-     trip(C1-C2), trip(C2-C3), dist(C1-C2,D1), dist(C2-C3,D2), maxDist(Max), D1+D2 > Max.


%%%%%%  1. SAT Variables:

satVariable( bdcc(B,D,C1,C2) ):- bus(B), day(D), trip(C1-C2).  % bus B on day D does C1-C2
% ... more variables may be needed

%%%%%%  2. Clause generation:

writeClauses:-  
    exactlyOneTripPerBusAndDay,
    ...
    true,!.
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.

exactlyOneTripPerBusAndDay:- ...
exactlyOneTripPerBusAndDay.

...


%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:

displaySol(M):- nl, frequency(C1-C2,F), nl, write('Trip '), write(C1-C2), write(' (min '), write(F), write('/week):'),
    member(bdcc(B,D,C1,C2),M), 
    write(' Bus'), write(B), write('-Day'), write(D), fail.
displaySol(M):- nl, bus(B), nl, write('Bus '), write(B), write(': '),
    day(D), member(bdcc(B,D,C1,C2),M), dist(C1-C2,Dist), 
    write(C1-C2), write('('), write(Dist), write(' km)  '),fail.
displaySol(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything below is given as a standard library, reusable for solving 
%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits ):- symbolicOutput(1), write( Var ), write(' <--> or('), write(Lits), write(')'), nl, !. 
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.

%% expressOr(a,[x,y]) genera 3 clausulas (como en la Transformación de Tseitin):
%% a == x v y
%% x -> a       -x v a
%% y -> a       -y v a
%% a -> x v y   -a v x v y

% Express that Var is equivalent to the conjunction of Lits:
expressAnd( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> and('), write(Lits), write(')'), nl, !. 
expressAnd( Var, Lits):- member(Lit,Lits), negate(Var,NVar), writeClause([ NVar, Lit ]), fail.
expressAnd( Var, Lits):- findall(NLit, (member(Lit,Lits), negate(Lit,NLit)), NLits), writeClause([ Var | NLits]), !.


%%%%%% Cardinality constraints on arbitrary sets of literals Lits:

exactly(K,Lits):- symbolicOutput(1), write( exactly(K,Lits) ), nl, !.
exactly(K,Lits):- atLeast(K,Lits), atMost(K,Lits),!.

atMost(K,Lits):- symbolicOutput(1), write( atMost(K,Lits) ), nl, !.
atMost(K,Lits):-   % l1+...+ln <= k:  in all subsets of size k+1, at least one is false:
      negateAll(Lits,NLits),
      K1 is K+1,    subsetOfSize(K1,NLits,Clause), writeClause(Clause),fail.
atMost(_,_).

atLeast(K,Lits):- symbolicOutput(1), write( atLeast(K,Lits) ), nl, !.
atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
      length(Lits,N),
      K1 is N-K+1,  subsetOfSize(K1, Lits,Clause), writeClause(Clause),fail.
atLeast(_,_).

negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.

negate( -Var,  Var):-!.
negate(  Var, -Var):-!.

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).


%%%%%% main:

main:-  symbolicOutput(1), !, writeClauses, halt.   % print the clauses in symbolic form and halt
main:-  initClauseGeneration,
        tell(clauses), writeClauses, told,          % generate the (numeric) SAT clauses and call the solver
        tell(header),  writeHeader,  told,
        numVars(N), numClauses(C),
        write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
        shell('cat header clauses > infile.cnf',_),
        write('Calling solver....'), nl,
        shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
        treatResult(Result),!.

treatResult(20):- write('Unsatisfiable'), nl, halt.
treatResult(10):- write('Solution found: '), nl, see(model), symbolicModel(M), seen, displaySol(M), nl,nl,halt.
treatResult( _):- write('cnf input error. Wrote anything strange in your cnf?'), nl,nl, halt.
    

initClauseGeneration:-  %initialize all info about variables and clauses:
        retractall(numClauses(   _)),
        retractall(numVars(      _)),
        retractall(varNumber(_,_,_)),
        assert(numClauses( 0 )),
        assert(numVars(    0 )),     !.

writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w(-Var):- symbolicOutput(1), satVariable(Var), write(-Var), write(' '),!. 
w( Var):- symbolicOutput(1), satVariable(Var), write( Var), write(' '),!. 
w(-Var):- satVariable(Var),  var2num(Var,N),   write(-), write(N), write(' '),!.
w( Var):- satVariable(Var),  var2num(Var,N),             write(N), write(' '),!.
w( Lit):- told, write('ERROR: generating clause with undeclared variable in literal '), write(Lit), nl,nl, halt.


% given the symbolic variable V, find its variable number N in the SAT solver:
:-dynamic(varNumber / 3).
var2num(V,N):- hash_term(V,Key), existsOrCreate(V,Key,N),!.
existsOrCreate(V,Key,N):- varNumber(Key,V,N),!.                            % V already existed with num N
existsOrCreate(V,Key,N):- newVarNumber(N), assert(varNumber(Key,V,N)), !.  % otherwise, introduce new N for V

writeHeader:- numVars(N),numClauses(C), write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-     retract( numClauses(N0) ), N is N0+1, assert( numClauses(N) ),!.
newVarNumber(N):- retract( numVars(   N0) ), N is N0+1, assert(    numVars(N) ),!.

% Getting the symbolic model M from the output file:
symbolicModel(M):- get_code(Char), readWord(Char,W), symbolicModel(M1), addIfPositiveInt(W,M1,M),!.
symbolicModel([]).
addIfPositiveInt(W,L,[Var|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, varNumber(_,Var,N),!.
addIfPositiveInt(_,L,L).
readWord( 99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ c
readWord(115,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ s
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================
