:-use_module(library(random)).
:-['Boards', 'DisplayBoard', 'DisplayMessage', 'InputManager', 'PlaysList', 'Rules', 'Utils'].

test :-
    initialize(Board),
    plPlaysList(Board, 1, List),
    dmDisplayPlaysList(List).


start :-
    dmShowHeader,
    dmShowMainMenu.

/* Game flow predicates */
play :-
    initialize(Board),

    write('Enter Player 1 name (lowercase): '),
    read(Player1), nl,
    write('Enter Player 2 name (lowercase): '),
    read(Player2), nl,
    
    PrevPlayer is 2,
    play(Board, PrevPlayer, Player1, Player2).

play(Board, PrevPlayer, Player1, Player2):-
    \+ achieveGoal(Board, PrevPlayer, Player1, Player2),
    uSwitchPlayer(PrevPlayer, Player),
    (
        Player==1 -> dmShowPlayInfo(Board, Player1, 'P');
        Player==2 -> dmShowPlayInfo(Board, Player2, 'B')
    ),
    (
        imSelectPlay(Board, Player, Player1, Player2),
        play(Board, Player, Player1, Player2)
    ).

playPvC :-
    initialize(Board),

    write('Enter Player name (lowercase): '),
    read(Player1), nl,
    Player2 = 'computer',
    PrevPlayer is 2,
playPvC(Board, PrevPlayer, Player1, Player2).

playPvC(Board, PrevPlayer, Player1, Player2) :-
    \+ achieveGoal(Board, PrevPlayer, Player1, Player2),
    uSwitchPlayer(PrevPlayer, Player),
    nl,nl,nl,
    (
        Player==1 ->    dmShowPlayInfo(Board, Player1, 'P'),
                        imSelectPlayPvC(Board, Player, Player1, Player2),
                        playPvC(Board, Player, Player1, Player2);
        Player==2 ->    write('computer (B) turn!'), nl,
                        plPlaysList(Board, Player, List),
                        uCount(List, 0, Count),
                        random(0, Count, Index),
                        moveFromList(Board, Player, List, Index, FinishBoard),
                        nl, nl, dbDrawBoard(FinishBoard), nl, nl,
                        write('Enter anything to continue (0 to quit): '),
                        read(Opt),
                        (
                            Opt == 0 -> uQuit;
                            playPvC(FinishBoard, Player, Player1, Player2)
                        )
    ).

playCvC :-
    initialize(Board),
    Player1 = 'computer1',
    Player2 = 'computer2',
    PrevPlayer is 2,
    nl, nl, dbDrawBoard(Board), nl, nl,
    playCvC(Board, PrevPlayer, Player1, Player2).

playCvC(Board, PrevPlayer, Player1, Player2) :-
    \+ achieveGoal(Board, PrevPlayer, Player1, Player2),
    uSwitchPlayer(PrevPlayer, Player),
    
    (
        Player==1 -> write('computer1 (P) turn!'), nl;
        Player==2 -> write('computer2 (B) turn!'), nl
    ),
    plPlaysList(Board, Player, List),
    uCount(List, 0, Count),
    random(0, Count, Index),
    moveFromList(Board, Player, List, Index, FinishBoard),
    nl, nl, dbDrawBoard(FinishBoard), nl, nl,
    write('Enter anything to continue (0 to quit): '),
    read(Opt),
    (
        Opt == 0 -> uQuit;
        playCvC(FinishBoard, Player, Player1, Player2)
    ).

moveFromList(Board, Player, List, Index, FinishBoard) :-
    uGetIndexElem(List, Index, Move),
    uGetIndexElem(Move, 2, FromX),
    uGetIndexElem(Move, 3, FromY),
    uGetIndexElem(Move, 4, ToX),
    uGetIndexElem(Move, 5, ToY),
    uChangeElem(Board, FromX, FromY, 0, TmpBoard),
    uChangeElem(TmpBoard, ToX, ToY, Player, FinishBoard),
    
    uTranslateColumn(FromXT, FromX),
    uTranslateRow(FromYT, FromY),
    uTranslateColumn(ToXT, ToX),
    uTranslateRow(ToYT, ToY),
    write('Moving from ('),
    write(FromXT), write(','), write(FromYT),
    write(') to ('),
    write(ToXT), write(','), write(ToYT),
    write(')'), nl.

moveFromCoords(Board, Player, FromXT, FromYT, ToXT, ToYT, FinishBoard) :-
    uTranslateColumn(FromXT, FromX),
    uTranslateRow(FromYT, FromY),
    uTranslateColumn(ToXT, ToX),
    uTranslateRow(ToYT, ToY),
    uChangeElem(Board, FromX, FromY, 0, TmpBoard),
    uChangeElem(TmpBoard, ToX, ToY, Player, FinishBoard).

achieveGoal(Board, Player, Player1, Player2) :-
    (
        Player == 1 ->  uGetIndexElem(Board, 7, Row),
                        uExist(Row, Player),
                        write('Congratulations player '),
                        write(Player1), 
                        write('! You WON!'),
                        uQuit;
        Player == 2 ->  uGetIndexElem(Board, 0, Row),
                        uExist(Row, Player),
                        write('Congratulations player '),
                        write(Player2), 
                        write('! You WON!'),
                        uQuit;
        fail
    ).
/* END Game flow predicates */
