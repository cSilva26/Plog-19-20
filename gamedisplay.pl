sample0 :-
    nl,initialize(X),dbDrawBoard(X).
sample1 :-
    X = [[1,3,1,3,1], 
         [0,0,0,0,0],  
         [0,0,0,0,0], 
         [0,0,0,0,0], 
         [2,4,2,4,2]],
	nl,dbDrawBoard(X).
    
sample2 :-
    X = [[1,0,1,0,0], 
         [0,0,3,0,0],  
         [0,0,4,1,3], 
         [2,4,0,0,0], 
         [0,0,2,0,2]],
	nl,dbDrawBoard(X).

sample3 :-
    X = [[0,0,0,0,0], 
         [0,0,0,0,0],  
         [0,0,0,0,0], 
         [0,0,0,0,0], 
         [0,0,0,0,0]],
    nl,dbDrawBoard(X).

sample4 :-
    X = [[0,0,3,0,0], 
         [0,0,0,0,0],  
         [0,1,0,0,0], 
         [0,0,0,1,0], 
         [0,0,0,0,0]],
    nl,dbDrawBoard(X).

initialize(Board) :-
	Board = [[1,3,1,3,1], 
         	[0,0,0,0,0],  
         	[0,0,0,0,0], 
         	[0,0,0,0,0], 
         	[2,4,2,4,2]].

dbDrawBoard(Board) :-
    write('  A   B   C   D   E'),nl,
	dbDrawLine(Board, 1).

dbDrawLine([X|Xs], Row) :-
    dbDrawHLine,nl,
    dbDrawCell(X),
    write(' '),
    write(Row),
    NextRow is Row + 1,
    nl,
    dbDrawLine(Xs, NextRow).

dbDrawLine([],_) :-
    dbDrawHLine.

dbDrawHLine :-
    write('+---+---+---+---+---+').

dbDrawCell([X|Xs]) :-
    write('|'), 
    X==0 -> write('   '), dbDrawCell(Xs);
    X==1 -> write(' A '), dbDrawCell(Xs);
    X==2 -> write(' V '), dbDrawCell(Xs);
    X==3 -> write(' Ap '), dbDrawCell(Xs);
    X==4 -> write(' Vp '), dbDrawCell(Xs).

dbDrawCell([]) :-
    write('|').