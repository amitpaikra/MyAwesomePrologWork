
% 1) To determine whether the first two elements of a list are same.

member([X|[X|_]]).

% 2) To determine whether a list is not a two-element list.
% return true if two-element else false
notTwoElementList([]).
notTwoElementList([_]).
notTwoElementList( [ _| [_| [_|_]]]).

% 3) To determine whether two lists are of same length.

len( [] , 0 ).
len( [_|T] , N ) :- len( T , N1 ) , N = N1 + 1 .

isLengthSame( L1 , L2 ) :- len( L1 , NL1 )  , len( L2  , NL2 ) , NL1 =:= NL2 .

% 4) To determine length of a list using your own number system, that does not contain more than two symbols.

lengthOurSys([],0).
lengthOurSys([_X|R1], s(N) ) :- lengthOurSys(R1,N).

% 5) To determine whether two lists are of same length using the length predicate developed in 4 (previous problem).

isLenSame(X,Y):- lengthOurSys(X,A), lengthOurSys(Y,A).

% 6) To find the last element of a list.

lastElement( [X] , X ).
lastElement( [ _ | R ] , X ) :- lastElement( R  ,  X ).    

findLastElement( L ) :- lastElement( L , X ), write(X) , !.

findLastElement([]) :- write("Empty List !!! "), !.


% 7) To find whether an element is a member of a list.

isElementMember( X , [X|_] ).
isElementMember( X , [_|R]) :- isElementMember( X , R ).


                                

% 8) To find whether two elements are next to each other in a list.

isElementNext( X , Y , [X | [Y | _ ]]).
isElementNext( X , Y , [ _ | [_| R ]] ) :- isElementNext( X, Y , R).

% 9) To append two lists in a third list.

append( [] , L , L ).
append( [X|L1] , L2 , [X|L3] ) :- append( L1 , L2 , L3 ).

% 10) To find the last element of a list using append predicate developed in 9.

findLastElement( L  , X ) :- append( _ , [X] , L ). 

% 11) To find whether an element is a member of a list using append predicate developed in 9.

isMember(L,X) :- append(_L1,[X|_L2],L).

% 12 ) To find whether two elements are next to each other in a list using append predicate developed in 9.

nextEachAppend( X , Y , L ) :- append( _ , [X|[Y|_]] , L ).

% 13) To reverse a list in another list.

rev([],L1 , L1 ).
rev( [ X | Tail ] , L1 , L2) :- rev( Tail , [ X|L1 ] , L2).
reverseList( L1 , L2 ) :- rev( L1 , [] , L2 ).

% 14)  To determine whether a list is a palindrome.
% return true if palindrome else false


compare([] , [] ).
compare( [X|L1] , [ X|L2 ]) :- compare( L1 , L2 ).


palindrome(L1) :- rev( L1 , [] , L2 ) , compare( L1 , L2 ).

% 15) To find the last but one lement of a list.

lastButOne( X , [X,_]).
lastButOne( X ,[_|T]) :- lastButOne( X , T ).

% 16) To find the Kth element of a list.

elementAt([X|_] , 1, X).
elementAt([_|T] , K , Val ) :-  write(T) ,write(" ") , write(K) ,write(" ") ,  write(Val) , nl ,  K1 is K-1 , elementAt( T , K1 , Val ) .
% 17) To find the sum of all elements of a list.

sumList( [] , 0 ).
sumList([X|T] , Sum ) :- sumList( T , Sum1 ), Sum is X+Sum1.

% 18)  To find the length of a list.
lenlist([] , 0).
lenlist( [_|R] , M ) :- lenlist( R , N ) ,  M is N+1.

% 19)  To find the average of all elements of a list using sum and length defined in Problem 17 and 18.

avg( L , X ) :- sumList( L , R1 ) , lenlist( L , R2 ) , X is R1/R2.

% 20) To find the maximum number from a list.
max( X , Y , X ):- X > Y.
max( X , Y , Y ) :- X < Y.

maxList( [X] , X ).


maxList( [X|R] , N) :- maxList(R , M ), max( X , M , N ).

% 21) To find gcd of two integers.

gcd( 0 , X , X ).
gcd( X , Y , Val ) :- K is Y mod X , gcd( K , X , Val ).

% 22) To determine whether a given integer number is prime.

divisible(X,Y):- 0 is X mod Y,!.

divisible(X,Y):- X > Y+1, divisible(X, Y+1).

isPrime(2) :- true,!.
isPrime(X) :- X < 2,!,false.
isPrime(X) :- not(divisible(X, 2)).

% 23) To determine whether two positive integer numbers are coprime.

coPrime(X,Y):- gcd(X,Y,Z), Z is 1.

% 24)  To determine the prime factors of a given positive integer.

factor(X,Y,Y):- X>Y, isPrime(Y), 0 is X mod Y,!.
factor(X,Y,Z):- X>Y, K is Y+1, factor(X,K,Z).

primeFactors(X,[X]):- isPrime(X),!.
primeFactors(X,[Y|L]):- factor(X,2,Y), K is X/Y, primeFactors(K,L).

% 25) Goldbach's conjecture.

checkCond(K,X,[K,M]):- isPrime(K), M is X-K, isPrime(M),!.
checkCond(K,X,L):- T is K+1, checkCond(T,X,L).
goldbach(X,L):- checkCond(2,X,L).

% 26) To generate all integers between two integers N1 and N2, both N1 and N2 included and N2>N1.

genNums(N,N,[N]  , N ) :- !.
genNums(N1,N2,[N1|L] , Sum ):- N1<N2, Count is N1+1,  genNums( Count,N2,L , Psum) , Sum is N1 + Psum .

% 27) To count numbers greater than 100.0 in a list.

countGreater100([X],1):-  X>100,!.
countGreater100([X],0):- X=<100,!.
countGreater100([X|L],N):- countGreater100(L,M), X>100, N is M+1,!.
countGreater100([X|L],N):- countGreater100(L,M), X=<100, N is M,!.

% 28) To split a list of numbers in two lists such that one contains negative numbers and other contains positive numbers.

splitList([X],[X],[]):- X>=0,!.
splitList([X],[],[X]):- X<0,!.
splitList([X|L],[X|P],N):- splitList(L,P,N), X>=0,!.
splitList([X|L],P,[X|N]):- splitList(L,P,N), X<0,!.

% 29) To find N!

fact(0,1).
fact(N,X):- N1 is N-1, fact(N1,M), X is N*M.

% 30) To generate first N Fibonacci numbers.

fib(1,[0]):-!.
fib(2,[1,0]):-!.
fib(X,L):- K is X-1, fib(K,M), M = [A|[B|_]], C is A+B, L = [C|M].