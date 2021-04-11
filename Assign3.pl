:- [assign2].
:- [assign1].
% :- dynamic elementAt/3.
% 1. To find factorial N using accumulator.
findFactUsingAccu(0 , A , A ).
findFactUsingAccu( N , A , Res ) :- N > 0 , A1 is N*A , N1 is N-1 , findFactUsingAccu(N1 , A1 , Res ).
findFactUsingAccu( N , Res ) :- findFactUsingAccu( N , 1 , Res).


% 2. To find length of a list using accumulator.
findlenUsingAccu( [] , Len , Len ).
findlenUsingAccu( [_|T] , A , Len) :- A1 is A+1 , findlenUsingAccu( T , A1 , Len ).
findlenUsingAccu( L , Len ) :- findlenUsingAccu( L , 0 , Len ).

% 3. To remove duplicate elements from a list using accumulator.

member( X , [X |_ ]) :- !.
member( X , [_|T ]) :- member(X , T ).

remDupUsingAccu([] , A , A ).
remDupUsingAccu([H|T]  , A , Res ) :- member( H , A ), remDupUsingAccu( T , A , Res ).
remDupUsingAccu([H|T] , A , Res ) :- remDupUsingAccu( T , [H|A] , Res).
remDupUsingAccu( L , Res) :- remDupUsingAccu( L , [] , Res ).

% 4. To remove duplicate elements from a list without using accumulator.

remDup2( [] , [] ).
remDup2([H|T] , Res ) :- member(H,T) , remDup2(T , Res ).
remDup2([H|T] , [H|T1] ) :- (\+member(H,T)), remDup2( T , T1 ).
% 5. To reverse a list using accumulator.
revUsingAccu( [] , A , A ).
revUsingAccu( [X|L] , A , Res ) :- revUsingAccu(L,[X|A],Res).
revUsingAccu( L , Res ) :- revUsingAccu( L , [] , Res ).

% 6. Next Higher Permutation.
% A positive integer is represented by a list of decimal digits. Its next
% higher permutation is defined to be the next greater integer composed of
% exactly the same digits. For example, the next higher permutation of
% 123542 is 124235.
% Write a) a declarative Prolog program
next_higher_permutation(L,L1) :- higher_permutation(L,L1), not(far_higher_permutation(L1,L)).
    
higher_permutation(L,L1) :- permutation(L,L1), higher(L1,L).
    
higher([H1|_],[H|_]) :- H1 > H ,!.
higher([H|T1],[H|T]) :- higher(T1,T).
    
far_higher_permutation(L1,L) :-  higher_permutation(L,L2), higher(L1,L2).
    
permutation([],[]).
permutation(L,[H|T]) :- select(H,L,R),  permutation(R,T).
    
select(X,[X|R],R).
select(X,[H|R],[H|R1]) :-  select(X,R,R1).

% b) an efficient procedural Prolog program
% that receive a list of decimal digits and return its next higher permutation
% in a list.
trav([H1|[H2|_]],Ind,N,Y):-
    Ind<N,
    H1>H2,!,
    Y is N-Ind.
trav([H1|[H2|L]],Ind,N,Y):-
    Ind<N,
    H1=<H2,
    K is Ind+1,
    trav([H2|L],K,N,Y).
gety(L,Y):-
    reverse(L,L1),
    length(L1,N),
    trav(L1,1,N,Y).
getu([H|_],Ind,N,Y,U):-
    H>Y,!,
    U is N-Ind+1.
getu([H|L],Ind,N,Y,U):-
    H=<Y,
    K is Ind+1,
    getu(L,K,N,Y,U).

swapo(L,I,J,Ival,Jval,X):-
    K is I-1,
    split(L,K,L1,_),
%    write(L1),
    P is J,
    split(L,P,_,L4),
%    write(L4),
    A is I+1,
    B is J-1,
    slice(L,A,B,L5),
%    write(L5),
    append(L1,[Jval],Ans1),
%    write(Ans1),
    append(Ans1,L5,Ans2),
%    write(Ans2),
    append(Ans2,[Ival],Ans3),
%    write(Ans3),
    append(Ans3,L4,X).
%    write(X)

next_higher_perm_proc(L,X):-
    gety(L,Yind),
    reverse(L,L1),
    length(L1,N),
    elementAt(L,Yind,Y),
    getu(L1,1,N,Y,Uind),
    elementAt(L,Uind,U),
%    write(Yind),
%    write(Y),
%    write(Uind),
%    write(U),
    swapo(L,Yind,Uind,Y,U,L2),
    split(L2,Yind,L3,L4),
    reverse(L4,L5),
    append(L3,L5,X).












% 7. Eight Queens’ Problem.
% Eight Queens are to be placed in an 8x8 chess board such that no queen
% attack each other.
% Write a Prolog program to obtain solution(s) of Eight Queen Problem.
solution_eight([]).
solution_eight([X/Y|R]):-
    take_one([1,2,3,4,5,6,7,8],Y),
    solution_eight(R),
    no_attack(X/Y,R).

take_one([X|_],X).
take_one([_|L],Y):-
    take_one(L,Y).

no_attack(_,[]).
no_attack(X/Y,[X1/Y1|R]):-
    Y=\=Y1,
    (X-X1)=\=(Y-Y1),
    (X-X1)=\=(Y1-Y),
    no_attack(X/Y,R).


% 8. Tower of Hanoi Problem.


% The tower of Hanoi is a game played with three poles and a set of N discs.
% The discs are graded in diameter, and fit onto the poles by means of a
% hole cut through the center of each disc. Initially all the discs are on the
% left-hand pole. The object of the game is to move all the discs onto the
% center pole. The right-hand pole can be used as a “spare” pole, temporary
% resting place for discs. Each time a disc is moved from one pole to
% another, two constraints must be observed: only the top disc on a pole can
% be moved, and no disc may be placed on top of a smaller one.
% Write Prolog program to enumerate the moves to transfer N discs from
% the left-hand pole to the center pole.


hanoi(N) :- move(N, left, centre, right).
move(0, _, _, _) :- !.
move(N, A, B, C) :- M is N-1, move(M, A, C, B), inform(A, B , M ), move(M, C, B, A). % 3
inform( X , Y , Val ) :- write("move ") ,write("disk ") , Val1 is Val+1 ,  write(Val1), write(" ") , write(X) , write(" -> ") , write(Y), nl.  

% 9. There is an old song that goes as follows:
% 99 bottles of coke on the wall
% 99 bottles of coke
% You take one down and pass it around
% 98 bottles of coke on the wall
% and so on, until the last verse
% 1 bottle of coke on the wall
% 1 bottle of coke
% You take one down and pass it around
% No bottle of coke on the wall.
% Write a Prolog program cola that receives a positive integer and prints
% the Lyrics of the song. The program should print all the verses, and when
% it gets to the last verse, it must print 1 bottle, not 1 bottles and no bottle
% rather than 0 bottles.

colaSong(0):-write("No bottle of coke on the wall."),nl,!.
colaSong(1):-
    write("1 bottle of coke on the wall"),nl,
    write("1 bottle of coke"),nl,
    write("You take one down and pass it around"),nl,!,
    colaSong(0).

colaSong(N):-
    write(N),write(" bottles of coke on the wall"),nl,
    write(N),write(" bottles of coke"),nl,
    write("You take one down and pass it around"),nl,
    N1 is N-1,
    colaSong(N1).



% 10. In a lost-world language, a poem can have any number of verses, each of
% which takes the following form:
% A B B C
% D E E C
% F F G
% H I I C
% where the same letter represents rhymed words. For example,
% anun kura tama su
% unuri bimo co kuru
% sonen ariten sicom
% kana te shime xanadu.
% Design a database to store a number of lost-world words and write a Prolog
% program to produce a poem for a given number of verses.


selectElement(0, _, []).
selectElement(Count,L,[X|R]) :-
  random_member(X,L),
  select(X, L, R1),
  C1 is Count - 1,
  selectElement(C1, R1, R).

printList([]).
printList([X|L]):-
    write(X),write(" "),
    printList(L).

a(N):-
    Words=[survive, alive, arrive, drive, strive, live, dive],
    selectElement(N,Words,L),
    printList(L).

b(N):-
    Words=[cab , dab , drab , fab , flab , grab , jab , nab , lab , slab , tab , crab],
    selectElement(N,Words,L),
    printList(L).

c(N):-
    Words=[night, fight, bright, height, flight, light, might, wright, tight, knight, kite, quite, slight],
    selectElement(N,Words,L),
    printList(L).

d(N):-
    Words=[take, blake, cake, lake, snake, wake, shake],
    selectElement(N,Words,L),
    printList(L).

e(N):-
    Words=[care, fare, hair, bear, rare, scare, wear, where, stare, glare],
    selectElement(N,Words,L),
    printList(L).

f(N):-
    Words=[ball , call, gall, hall , mall , shawl , tall, wall ,thrall ,gall],
    selectElement(N,Words,L),
    printList(L).

g(N):-
    Words=[school, rule, tool, cool, pool, fool, fuel],
    selectElement(N,Words,L),
    printList(L).

h(N):-
    Words=[side, provide, tried, inside, guide, decide, beside],
    selectElement(N,Words,L),
    printList(L).

i(N):-
    Words=[ship, chip, clip , dip , drip , flip , grip , hip , kip , lip , nip , pip , rip , sip , slip , skip , snip , tip , trip , zip ],
    selectElement(N,Words,L),
    printList(L).



poem(0):-!.
poem(N):-
    a(1),b(2),c(1),nl,
    d(1),e(2),c(1),nl,
    f(2),g(1),nl,
    h(1),i(2),c(1),nl,
    nl,
    N1 is N-1,
    poem(N1).


% 11.Three musicians of a multinational band take turns playing solo in a piece
% of music: each plays only once. The pianist plays first. John plays
% saxophone plays before the Australian. Mark comes from the United States
% and plays before the violinist. One soloist comes from Japan and one is
% Sam.
% Write a PROLOG program to find out who comes from which country,
% plays what instrument, and in which order


% 12. One way of representing the positive whole numbers is a Prolog terms
% involving the integer 0 and the successor functor s with one argument.
% Thus, we represent 0 by itself, 1 by s (0), 2 by s (s (0)) and so on. Write
% definitions of standard arithmetic operations addition, multiplication and
% subtraction, given the above representation of numbers. For example, the
% predicate plus may be defined to exhibit the following behavior.
% ? - plus (s (s (0)), s (s (s (0))), X).
% {X= s (s (s (s (s (0)))))}
% that is, 2+3 = 5. Also define the predicate “less than”, “greater than”,
% “less than equal to” and “greater than equal to”. Further define arithmetic
% operations, like integer division, remainder of integer division, and
% square root.

