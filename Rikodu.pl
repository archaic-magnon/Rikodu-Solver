 %% 37, 61 or 91.
 %% size, prefilled, link, rsult
 %% rikudo(37, [(2, 4, 59),(-1, 3, 56),(1, 3, 60),(-6, 2, 1),(4, 2, 42),(-1, 1, 20),(2, 0, 26),(6, 0, 38),(-5, -1, 6),(-2, -2, 17),(4, -2, 34),(-5, -3, 9), (0, 0, -10)], [(3, 1, 4, 0),(5, -1, 3, -1),(1, -1, 2, -2),(1, -3, 0, -4),(3, -3, 4, -4)], R)


 %% finally add 0,0,-10
 solve(Result):- Size1=61,
  Size2=37,
   Size3=19,
    Sizes4=7,

 Prefilled1 = [(2, 4, 59),(-1, 3, 56),(1, 3, 60),(-6, 2, 1),(4, 2, 42),(-1, 1, 20),(2, 0, 26),(6, 0, 38),(-5, -1, 6),(-2, -2, 17),(4, -2, 34),(-5, -3, 9)],
 Prefilled2 = [(-3,3,6),(-4,2,36),(0,2,1),(3,1,15),(-4,0,9),(-2,0,12),(2,0,26),(4,0,18),(-1,-3,30),(3,-3,22)],
 Prefilled3 = [(-2,0,8),(1,1,15),(1,-1,18),(2,-2,3),(-2,-2,6),(-2,2,12),(4,0,1)],
 Prefilled4 = [(1,1,1),(2,0,6)],
 Link=[(3, 1, 4, 0),(5, -1, 3, -1),(1, -1, 2, -2),(1, -3, 0, -4),(3, -3, 4, -4)],
 rikudo(Size1, Prefilled1, Link, Result).





 rikudo(Size, [], Link, Result):-
 findSize(Size, Dimen),
 %% write("Dimen= "),writeln(Dimen),
 minInPrefilled(Min,Prefilled),
 %% write("Min in Prefilled= "),writeln(Min),
 L = [],

 %% write("L= "),writeln(L),
 maxCordinate(Dimen, MaxL),
  rikudoHelper(Size, MaxL, Link, Result, Dimen, Min, L), !.


 rikudo(Size, Prefilled, Link, Result):-
 findSize(Size, Dimen),
 %% write("Dimen= "),writeln(Dimen),
 minInPrefilled(Min,Prefilled),
 %% write("Min in Prefilled= "),writeln(Min),
 L = [],

 %% write("L= "),writeln(L),

 rikudoHelper(Size, Prefilled, Link, Result, Dimen, Min, L) .



 maxCordinate(Dimen, L):-
 (Dimen ==1, L = [(1,1,1), (-1,1,2), (2,0,6)] ) ;
 (Dimen ==2, L = [(1,1,1), (-1,1,2), (2,0,6), (3,1,7), (2,2,8), (4,0,18)] );
 (Dimen ==3, L = [(1,1,1), (-1,1,2), (2,0,6), (3,1,7), (2,2,8), (4,0,18), (5,1,19), (6,0,36)] );
 (Dimen ==4, L = [(1,1,1), (-1,1,2), (2,0,6), (3,1,7), (2,2,8), (4,0,18), (5,1,19), (6,0,36), (7,1,37), (8,0,60)]);
 (Dimen ==5, L = [(1,1,1), (-1,1,2), (2,0,6), (3,1,7), (2,2,8), (4,0,18), (5,1,19), (6,0,36), (7,1,37), (8,0,60), (9,1,61), (10,0,90)]).



 rikudoHelper(Size, Prefilled, Link, Result, Dimen, (X,Y,V), L):-



 (
 	lengthOfList(L, Len), Len >= Size - 2) -> 
 (
 	addToFront((X,Y,V), L, La),
 	addToFront((0,0,-10), La, Lb),
 	appendList(Lb,[],Result)				)
 ;
 (

 	V1 is V+1,
 	%% writeln(V),

 	presentInListByValue(Prefilled,(_,_,V1)), 
 	not(presentInListByValue(L,(_,_,V1)) )-> 
 	(
 		findCordinate( (_,_,V1), (Px, Py, Pv), Prefilled), 
 		generateNbr(X,Y, Dimen, (Px, Py))) -> 
 	(
 		addToFront((X,Y,V), L, L1),
 		rikudoHelper(Size, Prefilled, Link, Result, Dimen, (Px,Py,Pv), L1)
 	)
 	; 
 	false
 	;



 	(

 		generateNbr(X,Y,Dimen,(NbrX,NbrY)),
 		generateValue(V, Size, NewV),







 		( 
 			(presentInList(Prefilled, (NbrX,NbrY,NewV)), not(presentInList(L, (NbrX,NbrY,NewV))))  -> 
 			(addToFront((X,Y,V), L, L1),
 				%% update result and L
 				rikudoHelper(Size, Prefilled, Link, Result, Dimen, (NbrX,NbrY,NewV), L1));(
 				presentInListByCordinate( (NbrX,NbrY,_), Prefilled) -> false;	(
 					presentInListByValue(Prefilled, (_,_,NewV)) -> false;	(
 						presentInList(L, (NbrX,NbrY,NewV)) -> false;(
 							presentInListByCordinate( (NbrX,NbrY,_), L) -> false;(
 								presentInListByValue(L, (_,_,NewV)) -> false;
 								(addToFront((X,Y,V), L, L1),
 									rikudoHelper(Size, Prefilled, Link, Result, Dimen, (NbrX,NbrY,NewV), L1) )
 								)
 							)
 						)
 					)
 				)
 			)
 		)
 	).




 findAndDelete([H|T], H, T):- !.
 findAndDelete([H|T], A, X):- 
 findAndDelete(T, A, Z), X = [H|Z].


 generateValue(X, Size, Y):- 
 (X == 1, Y is 2);
 (X == Size, Size>1, Y is Size-1);
 (X > 1, X < Size, Y is X+1);
 (X > 1, X < Size, Y is X-1).


 generateNbr(X, Y, Dimen, L):- 
 (X1 is X+2, Y1 is Y, isValidCell(X1, Y1, Dimen), L = (X1, Y1));
 (X2 is X-2, Y2 is Y, isValidCell(X2, Y2, Dimen), L = (X2, Y2));
 (X3 is X-1, Y3 is Y+1, isValidCell(X3, Y3, Dimen), L = (X3, Y3));
 (X4 is X+1, Y4 is Y+1, isValidCell(X4, Y4, Dimen), L = (X4, Y4));
 (X5 is X-1, Y5 is Y-1, isValidCell(X5, Y5, Dimen), L = (X5, Y5));
 (X6 is X+1, Y6 is Y-1, isValidCell(X6, Y6, Dimen), L = (X6, Y6)).





 isLinked(X1, Y1, X2, Y2, Link):-
 presentInList((X1, Y1, X2, Y2), Link); 
 presentInList((X2, Y2, X1, Y1), Link).




 minInPrefilled((1,1,1), []).
 minInPrefilled(Min, [Min]).                
 minInPrefilled(M,[(A1,B1,C1),(_,_,C2)|T]) :- C1 =< C2, minInPrefilled(M,[(A1,B1,C1)|T]).               
 minInPrefilled(M,[(_,_,C1),(A2,B2,C2)|T]) :- C1 > C2, minInPrefilled(M,[(A2,B2,C2)|T]).  


 presentInList([H|_], H) :- !.
 presentInList([_|T], P):- presentInList(T, P).



 presentInListByCordinate((X,Y,_),[(X,Y,_)|_]):- !.
 presentInListByCordinate((X,Y,_),[_|T]):- presentInListByCordinate((X,Y,_),T).



 presentInListByValue([(_,_,V)|_], (_,_,V)):- !.
 presentInListByValue([_|T], (_,_,V)):- presentInListByValue(T, (_,_,V)).




 findCordinate((_,_,V), (X,Y,V), [(X,Y,V)|_]):- !.
 findCordinate((_,_,V), (A,B,C), [_|T]):- findCordinate( (_,_,V), (A,B,C), T).


 %% checkAndAppend(L, X, Y, Dimen, M):- 
 %% 		M1 is L,  
 %% 		X1 is X+2
 %% 		isValidCell(X1, Y, Dimen), addToFront()



 addToFront(A, L, [A|L]). 



 %% isValid(Size, Prefilled, Link, Result):- findSize(Size, Dimen).

 findSize(Size, Dimen):- Size == 7, Dimen is 1.
 findSize(Size, Dimen):- Size == 19, Dimen is 2.
 findSize(Size, Dimen):- Size == 37, Dimen is 3.
 findSize(Size, Dimen):- Size == 61, Dimen is 4.
 findSize(Size, Dimen):- Size == 91, Dimen is 5.

 isValidCell(0, 0, Dimen):-!,fail.
 isValidCell(X, Y, Dimen):- X =< 2*Dimen, X >= -2*Dimen, Y =< Dimen, Y >= -Dimen, 
 abs(Y, Ya), X =< (2*Dimen - Ya), X >= -(2*Dimen - Ya).

 abs(M, N):- M >= 0, N is M.
 abs(M, N):- M < 0, N is -M.


 lengthOfList([],0).
 lengthOfList([H|T], N):- lengthOfList(T, N1), N is N1+1.


 appendList([], L, L).
 appendList([H|T], L, [H|Ts]) :- appendList(T, L, Ts).


