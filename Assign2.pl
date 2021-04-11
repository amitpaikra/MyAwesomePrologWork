% 1. To add an element to a list provided it is not present in the list.
add( X , L , [X | L ]):- !.
isPresent( X , [X|_]) :- !.
isPresent( X , [_|T]) :- isPresent( X , T ).
addToList( X , L , Res ) :- 
    (\+isPresent( X , L )) ,!, add( X , L , Res) 
    ;
    write("Not added, Element already Present!!!"), !.

% 2. To delete first occurrence of an element from a list.


delete1Occur( X ,[X|T] , T ):- !.
delete1Occur( X , [H|T] , [H|T1]):- delete1Occur( X , T  , T1 ).

% 3. To delete all occurrences of an element from a list. // wrong


deleteAllOccur( _ , [] , [] ) :- !.
deleteAllOccur(X , [X|T] , L ) :- ! , deleteAllOccur( X , T , L ).
deleteAllOccur(X , [H|T] , [H|T1]) :- ! , deleteAllOccur( X , T , T1 ).

% 4. To replace the first occurrence of an element X in L with Y giving the result in L1.

replace1_Occur( X , Y , [X|T] , [Y|T] ):- !.
replace1_Occur( X , Y , [H|T] , [H|T1]):- replace1_Occur( X ,Y ,  T  , T1 ).

% 5. has_duplicate(L), that determines whether list L has duplicate elements.

member( X , [X |_ ]) :- !.
member( X , [_|T ]) :- member(X , T ).

has_duplicate( [H | T ]) :- member( H , T ), !.
has_duplicate( [_| T]) :- has_duplicate( T ) , !.

% 6. To duplicate the elements of a list.
% Example: ?- duplicate([a,b,c,c,d],X). {X = [a,a,b,b,c,c,c,c,d,d]}

duplicate( [] , [] ) :- !.
duplicate( [H | T ] , [ H | [H | T1 ]] ) :- duplicate( T , T1 ) , !. 

                                                                                                                                                                                                     
% 7, To duplicate the elements of a list a given number of times.
% Example: ?- duplicate2([a,b,c],3,X). {X = [a,a,a,b,b,b,c,c,c]} What are the results of the goal: ?- duplicate2(X,3,Y).

dup2([],_,[],_) :- ! .
dup2([_|T1] , N , T2 , 0 ) :- dup2(T1 , N , T2 , N ).
dup2([X|T1] , N , [X|T2] , K ) :- K > 0 , K1 is K-1 , dup2([X|T1] , N , T2 , K1).
duplicate2( L1 , N , L2 ) :- dup2( L1 , N , L2 , N ), !.


% 8. To determine whether a list is a sub list of another list. A list is a sub list of another list if it’s elements are present in another list consecutively and in the same order.
% 
%  1 2 3  , a b c 1 2 3 d e f
match( [] , _ ) :- !.
match( [ X | T1 ] , [ X | T2 ]) :- match( T1 , T2 ).
isSubList( [X | T1] , [ X | T2 ] ) :-match( T1 , T2) .
isSubList( L, [ _ | T2 ] ) :- isSubList( L , T2 ) , !.



% 9. To determine whether a set is a subset of another set.

isSubset([] , _ ) :- !.
isSubset( [X | T ] , L2 ) :- member( X , L2 ) , isSubset( T , L2 ) , ! .

% 10. To determine intersection of two sets.

makeIntersection( [] , _ , [] ):- !.
makeIntersection( [X|T] , S ,  [X|T2]) :- isSubset([X] , S ) , makeIntersection(T , S , T2 ) , !.
makeIntersection( [_|T] , S , Res ) :- makeIntersection(T , S , Res ) , !.

% 12.To determine union of two sets.

makeUnion( [] , L , L ) :- !.
makeUnion( [ X | T ] , L , [ X | T2 ] ) :- (\+isSubset( [X] , L  ))  , makeUnion( T , L , T2 ) , ! .
makeUnion( [ _ | T ] , L , Res ) :- makeUnion(T , L , Res ).

% 13.To determine difference of two sets.
makeDiff( [] , _ , [] ) :- !.
makeDiff( [X | T] , L , [ X | T2 ] ) :- (\+isSubset( [X] , L  )) , makeDiff( T , L , T2 ) ,! .
makeDiff( [ _| T ] , L , Res ) :- makeDiff(T , L , Res ) , !.

% 14.To determine symmetric difference of two sets.

%  
% (A-B) U (B-A)

makeSymDiff( A, B , Res ) :- makeDiff( A , B , Res1 ) , makeDiff( B , A , Res2 ) , makeUnion( Res1 , Res2 , Res ) , !.


% 15. To replace n th element by another element X in L, leaving the resultant list in L1.
% 0 based
replaceNthElement( [_|T] , 0 , Val , [Val|T] ):-!.
replaceNthElement( [H | T ] , X , Val ,  [H | T1]  ) :- X > 0 , X1 is X-1 , replaceNthElement( T , X1 , Val , T1 ) , !.
 

% 16. to remove every N'th element from a list.
% Example: ?- remove([a,b,c,d,e,f,g,h,i,k],3,X). {X = [a,b,d,e,g,h,k]}
removeNth( [] , _ , _ , [] ) :- !.
removeNth( [H|T1] , N , K , [H|T2] ) :- N > 1 , N1 is N-1 , removeNth( T1 , N1 , K , T2 ).
removeNth( [_|T1] , 1, K , Res ) :- removeNth( T1 , K , K , Res ).

removeNthElement( L1  , N , Res ) :- removeNth( L1  , N , N ,  Res ) , !.

% For the problems 17 – 18 assume L1, L2 and L denote lists of terms.
% 17. Interleave alternate elements of L1 and L2 into L. For example, if L1= [a, b, c] and L2= [1, 2], then L= [a, 1, b, 2, c].

alterElement( [] , L , L ) :- !.
alterElement( L , [] , L ) :- !. 
alterElement( [H | T ] , [H1 | T1 ] , [H | [H1 | T2 ]] ) :- alterElement( T , T1 , T2  ).

% 18.Transpose L1, L2 into L. That is, if L1= [a, b, c] and L2= [1, 2, 3], then L= [(a, 1), (b, 2), (c, 3)].

transpose( []  , [] , [] ).
transpose( [H|T] , [H1|T2] , [( H , H1 ) | T3 ]) :- transpose( T , T2 , T3 ) .

% 19. To split a list into two parts; the length of the first part is given.
% Do not use any predefined predicates. Example: ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2). {L1 = [a,b,c], L2 = [d,e,f,g,h,i,k]}

split( L , 0 , _ , L ) :- !. 
split([H|T] , K , [H|T2] , L2 ) :- K =\= 0 , K1 is K-1  , split( T , K1 , T2 , L2  ).
% 20. To extract a slice from a list.
% Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1. Example: ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L). {X = [c,d,e,f,g]}

slice( [H|_]  , 1 , 1 , [H] ) :- !.
slice( [H|T] , 1 , End , [H|T1] ) :- End > 1 , End1 is End -1 , slice( T , 1 , End1 , T1  ) .
slice( [_| T ] , Start , End , Res ) :- Start > 1 , Start1 is Start-1 , End1 is End-1 ,  slice( T , Start1 , End1 , Res ) , !.
 
% 21. To insert an element at a given position into a list.
% Example: ?- insert_at(alfa,[a,b,c,d],2,L). {L = [a,alfa,b,c,d]}

insert_at( Val , L ,  1 , [Val | L ]) :- !.
insert_at( Val , [ H | T ] , K , [H | T1 ]) :- K > 1 , K1 is K-1 ,  insert_at( Val , T , K1 , T1 ).

% For the problems 22 - 30 assume L and L1 is a list of terms.
% 22. To remove_every_other (L, L1). List L1 is just list L with every other element removed (the two lists should have the same first element).

% remove_every_other( [X]  ,  [X] ).
remove_every_other( [ H | [ _ | T ] ] , [ H | T1 ] ) :- remove_every_other( T , T1 ).
remove_every_other( [H | _ ] , [H]).
% 23. cutlast (L, L1) that defines L1 to be obtained from L with last element removed.

cutlast([_],[]).

cutlast([H|L],[H|L1]):- cutlast(L,L1).


% 24. trim (N, L, L1) that defines L1 to be obtained from L with first N elements removed.

trim(0,L,L).
trim(N,[_|L],L1):- N1 is N-1,   trim(N1,L,L1).


% 25.trimlast (N, L, L1) that defines L1 to be obtained from L with last N elements removed.

trimlast( N , [_|T1] , Res ) :-   N > 0  , N1 is N-1 ,  trimlast( N1 , T1 , Res ).
trimlast( 0 , L , L ) .

% 26.exchange_first_last(L, L1), defines that L1 to be obtained from L with first and last elements exchanged.
% Example:
% ?-exchange_first_last([a, b, c, d, e], X).
% {X= [e, b, c, d, a]}

findLastElement( [] , [] ) :- !.
findLastElement( [X] , X ) :- !.
findLastElement( [_|T] , X ) :- findLastElement( T , X ). 

replaceLast( [_] , Val , [Val] ) :- !.
replaceLast( [H|L] , Val , [H|L1] ) :- replaceLast( L , Val , L1 ) .

exchange_first_last( [H | T ] ,[H1 | T1 ] ) :- findLastElement( [H|T]  , H1 ) , replaceLast( T , H , T1  ). 


% 27 circular_left_shift(L, L1). That is, if L= [a, b, c, d, e, f] then L1= [b, c, d, e, f, a]..

addElement( [] , Val , [Val]).
addElement( [H|T] , Val , [H|T1] ) :- addElement( T , Val , T1 ).

circular_left_shift( [H|T] , Res ) :- addElement( T , H , Res ). 

% 28. circular_right_shift(L, L1). That is, if L= [a, b, c, d, e, f] then L1= [f, a, b, c, d, e]
% [Try using circular_left_shift in 27 to implement circular_right_shift.]
lenCount( [] , 0 ) :- !.
lenCount( [_|T] , N ) :- lenCount( T , N1 ) , N is N1 + 1 .

rShift( L , 1 , L ) :- !.
rShift( L , N , L1 ) :-circular_left_shift( L , L2 ) , N1 is N-1 ,  rShift( L2 , N1 , L1 ).
circular_right_shift(L, L1) :- lenCount( L , Len ) , rShift( L , Len , L1 ).


% 29.To delete the middle element from an odd-numbered list L into a list L1.
% 0 based
deleteNth( 0 , [_|L] , L ).
deleteNth( N , [H|L] , [H|L1]) :- N1 is N-1 , deleteNth( N1 , L , L1 ).
delete_odd_mid( L , L1 ) :- lenCount( L , Len ) , N is Len//2 , write(N) , deleteNth( N , L , L1).
 

% 30.To delete two middle elements from an even-numbered list L into a list L1.

delete2(1,[_,_|L],L) :- !.
delete2(N,[H|L],[H|L1]):-  N1 is N-1, delete2(N1,L,L1).
delete_even_mid(L,L1):- lenCount(L,Len), N is Len//2, delete2(N,L,L1).

