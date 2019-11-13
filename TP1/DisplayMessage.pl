dmShowHeader :-
    nl, nl,
    write('***************************************'),nl,
    write('****************  ORDO  ***************'),nl,
    write('***************************************'),nl,
    nl, nl.

dmShowMainMenu :- 
    write('1. Player vs Player'),nl,
    write('2. Player vs Computer'),nl,
    write('3. Computer vs Computer'),nl,
    write('4. View game rules'),nl,
    write('0. Quit game'),nl, nl, imMenu.

dmShowRules:-
    nl, nl,
    write('To view the full game rules navigate to: '), nl,
    write('https://spielstein.com/games/ordo/rules/official'), nl, nl.

dmShowPlayInfo(Board, PlayerName, Piece) :-
    nl, nl, dbDrawBoard(Board), nl, nl,
    write(PlayerName), 
    write(' ('), 
    write(Piece),
    write('), it\'s your turn!'), nl,
    dmShowMoveCommands, nl.

dmShowMoveCommands :-
    write('1. Move a single piece.'), nl,
    write('2. Move a horizontal ordo.'), nl,
    write('3. Move a vertical ordo.'), nl, 
    write('0. Quit').

dmShowMoveSinglePiece :-
    write('1. See a list of all possible plays.'), nl,
    write('2. Enter the coordinates.'), nl,
    write('0. Quit').

dmInvalidMove :-
    write('Invalid move, please try again!'), nl, nl.

dmDisplayPlaysList(List) :-
    uCount(List, 0, Num) ->
    write('Plays List:'),nl ->
    dmDisplayPlaysListMove(List, 0, Num).    

dmDisplayPlaysListMove(List, Offset, Max) :-
    Offset == Max, true;
    (
        uGetIndexElem(List, Offset, Move),
        uGetIndexElem(Move, 2, FromX),
        uGetIndexElem(Move, 3, FromY),
        uGetIndexElem(Move, 4, ToX),
        uGetIndexElem(Move, 5, ToY),
        
        uTranslateColumn(FromXT, FromX),
        uTranslateRow(FromYT, FromY),
        uTranslateColumn(ToXT, ToX),
        uTranslateRow(ToYT, ToY),
        NextOffset is Offset + 1,
        write(Offset), 
        write('. From('),
        write(FromXT), write(','), write(FromYT),
        write('), To('),
        write(ToXT), write(','), write(ToYT),
        write(')'), nl,
        dmDisplayPlaysListMove(List, NextOffset, Max)
    ).
