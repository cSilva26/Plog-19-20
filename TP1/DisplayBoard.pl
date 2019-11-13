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
    write('+---+---+---+---+---+---+---+---+---+---+').

dbDrawCell([X|Xs]) :-
    write('|'), 
    X==0 -> write('   '), dbDrawCell(Xs);
    X==1 -> write(' P '), dbDrawCell(Xs);
    X==2 -> write(' B '), dbDrawCell(Xs).

dbDrawCell([]) :-
    write('|').