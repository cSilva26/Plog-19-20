/*
 * Player 1 can only go sides and down (unless in reconecting moves)
 * Player 2 can only go sides and up (unless in reconecting moves)
 * Board, Player, PieceCoords (X, Y);
 * Returns List with the possible moves
 */
 % Gets all possible plays for the player(Player)
plPlaysList(Board, Player, List) :-
    plSinglePiecePlaysList(Board, Player, 0, 0, [], List).

% Gets all possible plays for every single piece from the player(Player)
plSinglePiecePlaysList(_, _, _, 8, CurrentList, CurrentList).
plSinglePiecePlaysList(Board, Player, 9, Y, CurrentList, FinalList) :-
    NextY is Y + 1,
    plSinglePiece(Board, Player, 9, Y, CurrentList, NextList),
    plSinglePiecePlaysList(Board, Player, 0, NextY, NextList, FinalList).
plSinglePiecePlaysList(Board, Player, X, Y, CurrentList, FinalList) :-
    NextX is X + 1,
    plSinglePiece(Board, Player, X, Y, CurrentList, NextList),
    plSinglePiecePlaysList(Board, Player, NextX, Y, NextList, FinalList).

% Gets all possible plays for a single piece
plSinglePiece(Board, Identifier, X, Y, PreviousList, List) :-
    % Checks if current piece is from the player
    \+ uGetIndexElem(Board, X, Y, Identifier), List = PreviousList;
    % Places an invalid element in the X Y pos so it won't be considered
    uChangeElem(Board, X, Y, -1, TmpBoard),
    
    % Surrounding coordinates
    LeftX is X-1,
    RightX is X+1,
    UpY is Y-1,
    DownY is Y+1,

    % Plays to the left    
    plSinglePieceLeft(TmpBoard, Identifier, X, Y, LeftX, Y, PreviousList, Result),
    
    % Plays to the right
    plSinglePieceRight(TmpBoard, Identifier, X, Y, RightX, Y, Result, Result2),
    
    % Plays up
    plSinglePieceUp(TmpBoard, Identifier, X, Y, X, UpY, Result2, Result3),
    
    % Plays down
    plSinglePieceUp(TmpBoard, Identifier, X, Y, X, DownY, Result3, Result4),

    % Plays up left
    plSinglePieceUpLeft(TmpBoard, Identifier, X, Y, LeftX, UpY, Result4, Result5),

    % Plays up right
    plSinglePieceUpRight(TmpBoard, Identifier, X, Y, RightX, UpY, Result5, Result6),

    % Plays down left
    plSinglePieceDownLeft(TmpBoard, Identifier, X, Y, LeftX, DownY, Result6, Result7),

    % Plays down right
    plSinglePieceDownRight(TmpBoard, Identifier, X, Y, RightX, DownY, Result7, List).
    

/* Getting valid moves when going Left */
plSinglePieceLeft(Board, Identifier, XOrigin, YOrigin, X, Y, List, Result) :-
    NextX is X-1,
    uGetIndexElem(Board, X, Y, Cell),
    (
        % Empty Cell
        Cell == 0           ->  plAddMoveToList(Board, Identifier, 'S', 'M', XOrigin, YOrigin, X, Y, List, NewList),
                                plSinglePieceLeft(Board, Identifier, XOrigin, YOrigin, NextX, Y, NewList, Result);

        % Current player cell
        Cell == Identifier  ->  Result = List;

        % Adversary cell
        plAddMoveToList(Board, Identifier, 'S', 'C', XOrigin, YOrigin, X, Y, List, NewList),
        Result = NewList
    );
    Result = List.


/* Getting valid moves when going Right */
plSinglePieceRight(Board, Identifier, XOrigin, YOrigin, X, Y, List, Result) :-
    NextX is X+1,
    uGetIndexElem(Board, X, Y, Cell),
    (
        % Empty Cell
        Cell == 0           ->  plAddMoveToList(Board, Identifier, 'S', 'M', XOrigin, YOrigin, X, Y, List, NewList),
                                plSinglePieceRight(Board, Identifier, XOrigin, YOrigin, NextX, Y, NewList, Result);

        % Current player cell
        Cell == Identifier  ->  Result = List;

        % Adversary cell
        plAddMoveToList(Board, Identifier, 'S', 'C', XOrigin, YOrigin, X, Y, List, NewList),
        Result = NewList
    );
    Result = List.


/* Getting valid moves when going Up */
plSinglePieceUp(Board, Identifier, XOrigin, YOrigin, X, Y, List, Result) :-
    NextY is Y-1,
    
    uGetIndexElem(Board, X, Y, Cell),
    (
        % Empty Cell
        Cell == 0           ->  plAddMoveToList(Board, Identifier, 'S', 'M', XOrigin, YOrigin, X, Y, List, NewList),
                                plSinglePieceUp(Board, Identifier, XOrigin, YOrigin, X, NextY, NewList, Result);

        % Current player cell
        Cell == Identifier  ->  Result = List;

        % Adversary cell
        plAddMoveToList(Board, Identifier, 'S', 'C', XOrigin, YOrigin, X, Y, List, NewList),
        Result = NewList
    );
    Result = List.


/* Getting valid moves when going Down */
plSinglePieceDown(Board, Identifier, XOrigin, YOrigin, X, Y, List, Result) :-
    NextY is Y+1,
    
    uGetIndexElem(Board, X, Y, Cell),
    (
        % Empty Cell
        Cell == 0           ->  plAddMoveToList(Board, Identifier, 'S', 'M', XOrigin, YOrigin, X, Y, List, NewList),
                                plSinglePieceDown(Board, Identifier, XOrigin, YOrigin, X, NextY, NewList, Result);

        % Current player cell
        Cell == Identifier  ->  Result = List;

        % Adversary cell
        plAddMoveToList(Board, Identifier, 'S', 'C', XOrigin, YOrigin, X, Y, List, NewList),
        Result = NewList
    );
    Result = List.


/* Getting valid moves when going UpLeft */
plSinglePieceUpLeft(Board, Identifier, XOrigin, YOrigin, X, Y, List, Result) :-
    NextX is X-1,
    NextY is Y-1,
    
    uGetIndexElem(Board, X, Y, Cell),
    (
        % Empty Cell
        Cell == 0           ->  plAddMoveToList(Board, Identifier, 'S', 'M', XOrigin, YOrigin, X, Y, List, NewList),
                                plSinglePieceUpLeft(Board, Identifier, XOrigin, YOrigin, NextX, NextY, NewList, Result);

        % Current player cell
        Cell == Identifier  ->  Result = List;

        % Adversary cell
        plAddMoveToList(Board, Identifier, 'S', 'C', XOrigin, YOrigin, X, Y, List, NewList),
        Result = NewList
    );
    Result = List.


/* Getting valid moves when going UpRight */
plSinglePieceUpRight(Board, Identifier, XOrigin, YOrigin, X, Y, List, Result) :-
    NextX is X+1,
    NextY is Y-1,
    
    uGetIndexElem(Board, X, Y, Cell),
    (
        % Empty Cell
        Cell == 0           ->  plAddMoveToList(Board, Identifier, 'S', 'M', XOrigin, YOrigin, X, Y, List, NewList),
                                plSinglePieceUpRight(Board, Identifier, XOrigin, YOrigin, NextX, NextY, NewList, Result);

        % Current player cell
        Cell == Identifier  ->  Result = List;

        % Adversary cell
        plAddMoveToList(Board, Identifier, 'S', 'C', XOrigin, YOrigin, X, Y, List, NewList),
        Result = NewList
    );
    Result = List.


/* Getting valid moves when going DownLeft */
plSinglePieceDownLeft(Board, Identifier, XOrigin, YOrigin, X, Y, List, Result) :-
    NextX is X-1,
    NextY is Y+1,
    
    uGetIndexElem(Board, X, Y, Cell),
    (
        % Empty Cell
        Cell == 0           ->  plAddMoveToList(Board, Identifier, 'S', 'M', XOrigin, YOrigin, X, Y, List, NewList),
                                plSinglePieceDownLeft(Board, Identifier, XOrigin, YOrigin, NextX, NextY, NewList, Result);

        % Current player cell
        Cell == Identifier  ->  Result = List;

        % Adversary cell
        plAddMoveToList(Board, Identifier, 'S', 'C', XOrigin, YOrigin, X, Y, List, NewList),
        Result = NewList
    );
    Result = List.


/* Getting valid moves when going DownRight */
plSinglePieceDownRight(Board, Identifier, XOrigin, YOrigin, X, Y, List, Result) :-
    NextX is X+1,
    NextY is Y+1,
    
    uGetIndexElem(Board, X, Y, Cell),
    (
        % Empty Cell
        Cell == 0           ->  plAddMoveToList(Board, Identifier, 'S', 'M', XOrigin, YOrigin, X, Y, List, NewList),
                                plSinglePieceDownRight(Board, Identifier, XOrigin, YOrigin, NextX, NextY, NewList, Result);

        % Current player cell
        Cell == Identifier  ->  Result = List;

        % Adversary cell
        plAddMoveToList(Board, Identifier, 'S', 'C', XOrigin, YOrigin, X, Y, List, NewList),
        Result = NewList
    );
    Result = List.

plHOrdo(Board, Identifier, XOrigin1, XOrigin2, Y, List) :-
    UpY is Y - 1,
    plHOrdoUp(Board, Identifier, XOrigin1, XOrigin2, Y, UpY, [], List).

plHOrdoUp(Board, Identifier, XOrigin1, XOrigin2, YOrigin, Y, List, Result) :-
    NextY is Y-1,!,
    (
        plCheckHOrdoSpace(Board, Xorigin1, XOrigin2, Y),!,
        plAddHOrdoMoveToList(Board, Identifier, 'H', 'M', XOrigin1, XOrigin2, YOrigin, Y, List, NewList),!,
        plHOrdoUp(Board, Identifier, Xorigin1, XOrigin2, YOrigin, NextY, NewList, Result)
    );
    Result = List.

plCheckHOrdoSpace(Board, X1, X2, Y) :-
    X1 > X2, true;
    plCheckOrdoSpace(Board, X1, Y),
    NextX1 is X1 + 1,
    plCheckHOrdoSpace(Board, NextX1, X2, Y).

plCheckOrdoSpace(Board, X, Y) :-
    uGetIndexElem(Board, X, Y, 0).

plCheckHOrdoLink(Board, Identifier, X1, X2, Y) :-
    X1 =< X2,!,
    plCheckLink(Board, Identifier, X1, Y);
    NextX1 is X1 + 1,
    plCheckHOrdoLink(Board, Identifier, NextX1, X2, Y).
    
plAddHOrdoMoveToList(Board, Identifier, MoveId, Type, X1, X2, Y, DestX, List, Result) :-
    (
        plCheckHOrdoLink(Board, Identifier, X1, X2, Y),
        append(List, [[MoveId, Type, X1, X2, Y, DestX]], Result)
    );
    Result = List.

% Adds a new move to the list if valid
plAddMoveToList(Board, Identifier, MoveId, Type, X, Y, DestX, DestY, List, Result) :-
    (
        plCheckLink(Board, Identifier, X, Y),
        append(List, [[MoveId, Type, X, Y, DestX, DestY]], Result)
    );
    Result = List.


% Checks if in the X Y pos there are any piece to keep the link
plCheckLink(Board, Identifier, X, Y) :-
    plValidateIndex(X, Y),
    plCheckLinkTop(Board, Identifier, X, Y);
    plCheckLinkBot(Board, Identifier, X, Y);
    plCheckLinkLeft(Board, Identifier, X, Y);
    plCheckLinkRight(Board, Identifier, X, Y);
    (
        TopY is Y-1,
        plValidateIndex(X, TopY),
        (
            plCheckLinkLeft(Board, Identifier, X, TopY);
            plCheckLinkRight(Board, Identifier, X, TopY)
        )
    );
    (
        BotY is Y+1,
        plValidateIndex(X, BotY),
        (
            plCheckLinkLeft(Board, Identifier, X, BotY);
            plCheckLinkRight(Board, Identifier, X, BotY)
        )
    ).   
    
% Checks if there's a link at the top
plCheckLinkTop(Board, Identifier, X, Y) :-
    Y>0,
    NewY is Y-1,
    uGetIndexElem(Board, X, NewY, Identifier).

% Checks if there's a link at the bot
plCheckLinkBot(Board, Identifier, X, Y) :-
    Y<7,
    NewY is Y+1,
    uGetIndexElem(Board, X, NewY, Identifier).

% Checks if there's a link at the left
plCheckLinkLeft(Board, Identifier, X, Y) :-
    X>0,
    NewX is X-1,
    uGetIndexElem(Board, NewX, Y, Identifier).

% Checks if there's a link at the right
plCheckLinkRight(Board, Identifier, X, Y) :-
    X<9,
    NewX is X+1,
    uGetIndexElem(Board, NewX, Y, Identifier).

% Checks if the index is inside the Board length
plValidateIndex(X, Y) :-
    X >= 0,
    X =< 9,
    Y >= 0,
    Y =< 9.
