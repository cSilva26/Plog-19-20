:- use_module(library(clpfd)).
:- use_module(library(lists)).

battleships(Width, Height, Objects):-
	length(Xs, 10), length(Ys, 10),
	domain(Xs,1,Width), domain(Ys,1,Height),
	append([Xs,Ys], Vars),
	transpose([Xs,Ys],Pairs),
	getShapes(Shapes, ShapesIds),
	length(ShapesIds, NShapes),
	length(ShapesVars, 10),
	domain(ShapesVars,1, NShapes),
	list_to_fdset(ShapesIds, Set),
	getObjects(Pairs, ShapesVars, 1, Objects),
	Box1 is Width+1, Box2 is Height+1,
	Options=[bounding_box([1,1],[Box1,Box2])],
	geost(Objects, Shapes,Options),
	N is Height * Width,
	labeling([], Vars),
	print_board(Objects, N).


writeBoard([],[],[]).
writeBoard([Xx|Xr], [Yy|Yr], [Object|RObjects]):-
	write('('),
	write(Xx),
	write(','),
	write(Yy),
	write(')->'),
	write(Object),nl,
	writeBoard(Xr, Yr, RObjects).

print_board_internal(Objects, N) :-
    N > 0,
    Y is (N div 5),
    X is (4 - (N mod 5)),
    print_cell(Objects, [X,Y]),
    Next is (N - 1),
    print_board_internal(Objects, Next).

print_cell(Objects, Coord):-
	Objects(_,_,Coord),
		write("|B|").

getShapes(Shapes, ShapesIds):-
	findall(Id, barco(Id,_), ShapesIds),
	findall(L, barco(_,L), Ls),
	append(Ls, Shapes).

getObjects([],[],_,[]).
getObjects([Pair|RPairs], [ShapeVar|RShapeVar], Id, [Object|RObjects]):-
	Object=object(Id, ShapeVar, Pair),
	NId is Id+1,
	getObjects(RPairs, RShapeVar, NId, RObjects).


barco(1, [sbox(1, [0,0],[1,1]), sbox(1, [1,1],[1,1])]).
barco(2, [sbox(2, [0,0],[1,1]), sbox(2, [1,1],[1,1])]).
barco(3, [sbox(3, [0,0],[1,1]), sbox(3, [1,1],[1,1])]).
barco(4, [sbox(4, [0,0],[1,1]), sbox(4, [1,1],[1,1])]).
barco(5, [sbox(5, [0,0],[2,1]), sbox(5, [1,1],[2,1])]).
barco(6, [sbox(6, [0,0],[2,1]), sbox(6, [1,1],[2,1])]).
barco(7, [sbox(7, [0,0],[2,1]), sbox(7, [1,1],[2,1])]).
barco(8, [sbox(8, [0,0],[3,1]), sbox(8, [1,1],[3,1])]).
barco(9, [sbox(9, [0,0],[3,1]), sbox(9, [1,1],[4,1])]).
barco(10, [sbox(10, [0,0],[4,1]), sbox(10, [1,1],[4,1])]).