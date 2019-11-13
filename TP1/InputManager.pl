imMenu :-
    read(Opt),
    (   
        Opt==1 -> play;
        Opt==2 -> playPvC;
        Opt==3 -> playCvC;
        Opt==4 -> dmShowRules, dmShowMainMenu;
        Opt==0 -> uQuit;
        dmInvalidMove, fail
    ). 

imSelectPlay(Board, Player, Player1, Player2) :-
    nl, nl, read(Opt), nl, nl,
    (
        Opt==1 -> imPlaySinglePiece(Board, Player, Player1, Player2);
        Opt==2 -> imPlayHorzOrdo;
        Opt==3 -> imPlayVertOrdo;
        Opt==0 -> uQuit;
        dmInvalidMove, fail
    ).

imPlaySinglePiece(Board, Player, Player1, Player2) :-
    dmShowMoveSinglePiece,
    nl, nl, read(Opt), nl, nl,
    (
        Opt==1 ->   plPlaysList(Board, Player, List),
                    dmDisplayPlaysList(List),
                    read(MoveIndex),
                    moveFromList(Board, Player, List, MoveIndex, FinishBoard),
                    play(FinishBoard, Player, Player1, Player2);
        Opt==2 ->   write('Enter from column (Letter): '),
                    read(FromXT),
                    write('Enter from row (Number): '),
                    read(FromYT),
                    write('Enter to column (Leter): '),
                    read(ToXT),
                    write('Enter to row (Number): '),
                    read(ToYT),
                    moveFromCoords(Board, Player, FromXT, FromYT, ToXT, ToYT, FinishBoard),
                    play(FinishBoard, Player, Player1, Player2);
        Opt==0 -> uQuit;
        dmInvalidMove, fail
    ).

imSelectPlayPvC(Board, Player, Player1, Player2) :-
    nl, nl, read(Opt), nl, nl,
    (
        Opt==1 -> imPlaySinglePiecePvC(Board, Player, Player1, Player2);
        Opt==2 -> imPlayHorzOrdo;
        Opt==3 -> imPlayVertOrdo;
        Opt==0 -> uQuit;
        dmInvalidMove, fail
    ).

imPlaySinglePiecePvC(Board, Player, Player1, Player2) :-
    dmShowMoveSinglePiece,
    nl, nl, read(Opt), nl, nl,
    (
        Opt==1 ->   plPlaysList(Board, Player, List),
                    dmDisplayPlaysList(List),
                    read(MoveIndex),
                    moveFromList(Board, Player, List, MoveIndex, FinishBoard),
                    playPvC(FinishBoard, Player, Player1, Player2);
        Opt==2 ->   write('Enter from column (Letter): '),
                    read(FromXT),
                    write('Enter from row (Number): '),
                    read(FromYT),
                    write('Enter to column (Leter): '),
                    read(ToXT),
                    write('Enter to row (Number): '),
                    read(ToYT),
                    moveFromCoords(Board, Player, FromXT, FromYT, ToXT, ToYT, FinishBoard),
                    playPvC(FinishBoard, Player, Player1, Player2);
        Opt==0 -> uQuit;
        dmInvalidMove, fail
    ).

imPlayHorzOrdo :-
    write('Not yet implemented!'), nl, fail.

imPlayVertOrdo :-
    write('Not yet implemented!'), nl, fail.