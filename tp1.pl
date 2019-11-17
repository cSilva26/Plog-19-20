/*:-use_module(library(between)).*/

initial_board(board(Red, Blue)):-
		Red = [piece(white, 0, 0), piece(black, 1, 0), piece(white, 2, 0), 
				piece(black, 3, 0), piece(white, 4, 0)],
		Blue = [piece(white, 0, 4), piece(black, 1, 4), piece(white, 2, 4), 
				piece(black, 3, 4), piece(white, 4, 4)].

sample2(board(Red, Blue)):-
    Red = [piece(white, 0, 1), piece(black, 1, 1), piece(white, 2, 2), 
        piece(black, 3, 0), piece(white, 4, 0)],
    Blue = [piece(white, 0, 4), piece(black, 2, 3), piece(white, 2, 4), 
        piece(black, 3, 2), piece(white, 4, 2)].

sample3(board(Red,Blue)):-
    Red = [],
    Blue = [].

sample4(board(Red,Blue)):-
    Red = [],
    Blue = [piece(white, 1, 2), piece(black, 2, 4), piece(white, 3, 1)].

piece(white, X, Y)  :- position(X, Y).
piece(black, X, Y) :- position(X, Y).

board_tile(black,0,0).
board_tile(white,0,1).
board_tile(black,0,2).
board_tile(white,0,3).
board_tile(black,0,4).
board_tile(white,1,0).
board_tile(black,1,1).
board_tile(white,1,2).
board_tile(black,1,3).
board_tile(white,1,4).
board_tile(black,2,0).
board_tile(white,2,1).
board_tile(black,2,2).
board_tile(white,2,3).
board_tile(black,2,4).
board_tile(white,3,0).
board_tile(black,3,1).
board_tile(white,3,2).
board_tile(black,3,3).
board_tile(white,3,4).
board_tile(black,4,0).
board_tile(white,4,1).
board_tile(black,4,2).
board_tile(white,4,3).
board_tile(black,4,4).

player(red).
player(blue).
other_player(red, blue).
other_player(blue, red).

look_ahead_on(2).

coordinate(X) :- list_member([0, 1, 2, 3, 4], X).

/* Any valid cell on the board.*/
cell(X, Y) :- coordinate(Y), coordinate(X).

position(X, Y) :-
    cell(X, Y).


evaluate_board(board(Red, Blue), Score) :-
    list_count(Blue, piece(_, _, _), Blues),
    list_count(Red, piece(_, _, _), Reds),
    Score is (Reds-Blues).

/*---------------------------------------------------------------------------------------------*/
piece_color(NewListCurrentPlayerMoves, X1,Y1, Color):-
  list_member(NewListCurrentPlayerMoves,piece(Color,X1,Y1)).

list_member(List, Member):- member(Member, List).
list_absent(List, Element):- \+ (member(Element, List)).
delete_from_list(List, Element, NewList):- select(Element, List, NewList).
append_list(List1, List2, Result):- append(List1, List2, Result).

list_count([], _, 0).
list_count([FirstElement | Tail], Element, Occurrences) :-
    unifiable(FirstElement, Element, _),
    list_count(Tail, Element, Tmp),
    Occurrences is (Tmp + 1), !.
list_count([_ | Tail], Element, Occurrences) :- list_count(Tail, Element, Occurrences).

/*predicates to list the members of the board depending on the side of the player*/
listBoards(blue, board(Red, Blue), Blue, Red).
listBoards(red, board(Red, Blue), Red, Blue).
/*----------------------------------------------------------------------------------------------------*/
/*Print any symbol inside cell. Also prints separator & vertical cells numbering.*/
print_cell_symbol(X, Y, Symbol) :- X  = 4, write(' | '), write(Symbol), write(' | '), write(Y),
                                   write('\n  --------------------------\n').
print_cell_symbol(X, _, Symbol) :- X \= 4, X \= 0, write(' | '), write(Symbol).
print_cell_symbol(X, Y, Symbol) :- X  = 0, write(Y), write(' | '), write(Symbol).

/* Print empty cell*/
print_empty_cell(X, Y) :- Sum is (X + Y), Reminder is (Sum mod 2), Reminder  = 0,
                          print_cell_symbol(X, Y, '--'). % Black cell
print_empty_cell(X, Y) :- Sum is (X + Y), Reminder is (Sum mod 2), Reminder \= 0,
                          print_cell_symbol(X, Y, '  '). % White cell

/* Generic predicates to print cell with appropriate symbol inside.*/
print_cell(board(Red, _), X, Y)       :- (list_member(Red, piece(white,  X, Y)), print_cell_symbol(X, Y, 'Vb'));
                                           (list_member(Red, piece(black, X, Y)), print_cell_symbol(X, Y, 'Vp')).
print_cell(board(_, Blue), X, Y)       :- (list_member(Blue, piece(white,  X, Y)), print_cell_symbol(X, Y, 'Ab')) ;
                                           (list_member(Blue, piece(black, X, Y)), print_cell_symbol(X, Y, 'Ap')).
print_cell(board(_, _), X, Y)           :- \+ (position(X, Y)), print_empty_cell(X, Y).
print_cell(board(Red, Blue), X, Y)   :- list_absent(Red, piece(_, X, Y)), list_absent(Blue, piece(_, X, Y)),
                                           print_empty_cell(X, Y).

/* Recursive predicate to print all cells.*/
print_board_internal(board(Red, Blue), 0) :- print_cell(board(Red, Blue), 4, 0).
print_board_internal(board(Red, Blue), N) :-
    N > 0,
    Y is (N div 5),
    X is (4 - (N mod 5)),
    print_cell(board(Red, Blue), X, Y),
    Next is (N - 1),
    print_board_internal(board(Red, Blue), Next).

/* Print whole board*/
print_board(Board) :-
    write('    0    1    2    3    4   \n'),
    write('  --------------------------\n'),
    print_board_internal(Board, 24),
    write('    0    1    2    3    4   \n'), !.

/*Movements ------------------------------------------------------------------*/
can_move(Side, Board) :- move(Side, Board, _), !.
can_move(Side, _) :- other_player(Side, OtherSide), write('\n'), write(OtherSide), write(' wins!\n'), !, fail.

move(Side, board(Red, Blue), board(NewRed, NewBlue)) :-
    move_figure(Side, board(Red, Blue), board(NewRed, NewBlue)).

/*predicado que acede à lista de pecas remove a peca escolhida do sitio prévio */
/*  e adiciona uma nova com a nova posicao escolhida*/
move_figure(Side, ListPlayer, ListOpponent, NewListPlayer):-
            delete_from_list(ListPlayer, InitialPosition, TmpListPlayer),
            possible_move(Side, InitialPosition, FinalPosition),
            way_is_free(Side, ListPlayer, ListOpponent, FinalPosition, NewListOpponent),
            append_list(TmpListPlayer, [FinalPosition], NewListPlayer).


move_figure(red, board(Red, Blue), board(NewRed, Blue)) :-
    move_figure(red, Red, Blue, NewRed).
move_figure(blue, board(Red, Blue), board(Red, NewBlue)) :-
    move_figure(blue, Blue, Red, NewBlue).

possible_move(red,piece(ColorPiece,X,Y), piece(_,X2,Y2)):-
  board_tile(ColorBoard, X, Y),
  (((Y2 is Y+1); (Y2 is Y-1); (Y2 is Y)),
  ((X2 is X+1); (X2 is X-1); (X2 is X)),
  inside_board(X2,Y2));
  (ColorBoard==ColorPiece) -> (knight_movement_possible(X, Y, X2, Y2)); (bishop_movement_possible(X, Y, X2, Y2)).

possible_move(blue,piece(ColorPiece,X,Y), piece(_,X2,Y2)):-
  board_tile(ColorBoard, X, Y),
  (((Y2 is Y+1); (Y2 is Y-1); (Y2 is Y)),
  ((X2 is X+1); (X2 is X-1); (X2 is X)),
  inside_board(X2,Y2));
  (ColorBoard==ColorPiece) -> (knight_movement_possible(X, Y, X2, Y2)); (bishop_movement_possible(X, Y, X2, Y2)).

knight_movement_possible(Xs,Ys,Xd,Yd):-
        ground(Xs),ground(Ys),
        (knight_mov(DistX,DistY);knight_mov(DistY,DistX)),
        Xd is Xs+DistX,
        Yd is Ys+DistY,
        inside_board(Xd,Yd).

bishop_movement_possible(Xi,Yi,Xf,Yf):-
        Xi \= Xf, Yi \= Yf,
        DiagDiffi is (Xi-Yi), DiagDifff is (Xf-Yf),
        DiagSumi is (Xi+Yi), DiagSumf is (Xf+Yf),
        (DiagDiffi = DiagDifff ; DiagSumi = DiagSumf),
        inside_board(Xf,Yf).


move_is_possible(red,piece(ColorPiece,X,Y), piece(_,X2,Y2)):-
  inside_board(X2,Y2),
  ((board_tile(ColorBoard, X, Y),
  (ColorBoard==ColorPiece) -> (bishop_movement(X, Y, X2, Y2)) ; (knight_movement(X, Y, X2, Y2)));
  (
    (((Y2=:=Y+1); (Y2=:=Y-1); (Y2=:=Y)),((X2=:=X+1); (X2=:=X-1) ; (X2=:=X))),
  inside_board(X2,Y2)
  )).

knight_movement(Xs, Ys, Xd, Yd):-
        ground(Xs),ground(Ys),
        (knight_mov(DistX,DistY);knight_mov(DistY,DistX)),
        (Xd =:= Xs+DistX;
        Yd =:= Ys+DistY),
        inside_board(Xd,Yd).

bishop_movement(Xi, Yi, Xf, Yf):-
        Xi \= Xf, Yi \= Yf,
        DiagDiffi is (Xi-Yi), DiagDifff is (Xf-Yf),
        DiagSumi is (Xi+Yi), DiagSumf is (Xf+Yf),
        (DiagDiffi = DiagDifff ; DiagSumi = DiagSumf),
        inside_board(Xf,Yf).

inside_board(X,Y):-
      between(0,4,X),
      between(0,4,Y).

knight_mov(-2,-1).
knight_mov(-2,1).
knight_mov(2,-1).
knight_mov(2,1).

/* Predicate to check if the way is free */
way_is_free(Side, ListCurrentPlayerMoves, ListNoMoves, piece(_, X2, Y2), NewListOpponent) :-
    inside_board(X2,Y2),
    (list_member(ListNoMoves, piece(_, X2, Y2)),
      delete_from_list(ListNoMoves, piece(_,X2,Y2), NewListOpponent)
      );
    (list_absent(ListCurrentPlayerMoves, piece(_, X2, Y2)),
      NewListOpponent = ListNoMoves).
/*---------------MINMAX Algorithm---------------------------------------------*/
swap_node_type(min, max).
swap_node_type(max, min).

/* Helper predicate to get board with best score. For max node 'best' is maximal score. For min node - minimal.*/
best_score_and_board(max, Board1, Score1, Board2, Score2, BestBoard, BestScore) :-
    (Score1 >= Score2, !, BestBoard = Board1, BestScore = Score1) ;
    (Score1  < Score2, BestBoard = Board2, BestScore = Score2).
best_score_and_board(min, Board1, Score1, Board2, Score2, BestBoard, BestScore) :-
    (Score1 =< Score2, !, BestBoard = Board1, BestScore = Score1) ;
    (Score1  > Score2, BestBoard = Board2, BestScore = Score2).

/* Checks if specified value prune other nodes.*/
alpha_beta_prune(max, _, Beta, Value)  :- Value > Beta.
alpha_beta_prune(min, Alpha, _, Value) :- Value < Alpha.

/*Update Alpha or Beta value based on new 'Value'*/
update_alpha_beta(max, Alpha, Beta, Value, NewAlpha, Beta) :- (Value > Alpha, !, NewAlpha = Value) ; (NewAlpha = Alpha).
update_alpha_beta(min, Alpha, Beta, Value, Alpha, NewBeta) :- (Value < Beta,  !, NewBeta = Value) ; (NewBeta = Beta).

/* Build list of all possible moves for the 'Side' on provided board.*/
possible_moves_list(Side, Board, List) :- findall(NewBoard, move(Side, Board, NewBoard), List).

/* Get best move from the list of possible moves.*/
/* If depth is zero (leaf node), choose board by the best score.*/
find_best_move(_, _, [BestBoard], BestBoard, BestScore, 0, _, _) :-
    evaluate_board(BestBoard, BestScore), !.

find_best_move(Side, NodeType, [Board1 | Tail], BestBoard, BestScore, 0, Alpha, Beta) :-
    evaluate_board(Board1, Score1),
    (
        (alpha_beta_prune(NodeType, Alpha, Beta, Score1), !, BestBoard = Board1, BestScore = Score1) ;
        (
            find_best_move(Side, NodeType, Tail, Board2, Score2, 0, Alpha, Beta),
            best_score_and_board(NodeType, Board1, Score1, Board2, Score2, BestBoard, BestScore)
        )
    ).

/* If depth is not zero - evaluate each node with minmax predicate, and chose the best.*/
find_best_move(Side, NodeType, [BestBoard], BestBoard, BestScore, Depth, Alpha, Beta) :-
    Depth > 0,
    NewDepth is (Depth - 1),
    swap_node_type(NodeType, NextNodeType),
    other_player(Side, OtherSide),
    minmax(OtherSide, NextNodeType, BestBoard, _, BestScore, NewDepth, Alpha, Beta), !.

find_best_move(Side, NodeType, [Board1 | Tail], BestBoard, BestScore, Depth, Alpha, Beta) :-
    Depth > 0,
    NewDepth is (Depth - 1),
    swap_node_type(NodeType, NextNodeType),
    other_player(Side, OtherSide),
    minmax(OtherSide, NextNodeType, Board1, _, Score1, NewDepth, Alpha, Beta),
    (
        (alpha_beta_prune(NodeType, Alpha, Beta, Score1), !, BestBoard = Board1, BestScore = Score1) ;
        (
            update_alpha_beta(NodeType, Alpha, Beta, Score1, NewAlpha, NewBeta),
            find_best_move(Side, NodeType, Tail, Board2, Score2, Depth, NewAlpha, NewBeta),
            best_score_and_board(NodeType, Board1, Score1, Board2, Score2, BestBoard, BestScore)
        )
    ).

/* If there are no more possible moves: this is a leaf node. Only need to evaluate it.*/
minmax(Side, _, Board, Board, Score, _, _, _) :-
    possible_moves_list(Side, Board, []),
    evaluate_board(Board, Score), !.

/* Otherwise get list of all possible moves and choose the best.*/
minmax(Side, NodeType, Board, BestBoard, BestScore, Depth, Alpha, Beta) :-
    possible_moves_list(Side, Board, PossibleMoves),
    find_best_move(Side, NodeType, PossibleMoves, BestBoard, BestScore, Depth, Alpha, Beta).

/* Predicate for 'smart move' for each side.*/
smart_move(red, Board, NewBoard) :-
    look_ahead_on(Depth), minmax(red, max, Board, NewBoard, _, Depth, -100000, 100000).
smart_move(blue, Board, NewBoard) :-
    look_ahead_on(Depth), minmax(blue, min, Board, NewBoard, _, Depth, -100000, 100000).



/*User Input ------------------------------------------------------------------*/

/*predicate to request player to choose a side*/
player_select_side(Side) :-
    write('Select your side (red/blue): '),
    read(UserSide),
    (
        (player(UserSide), Side = UserSide, !);
        (write('Invalid side, please type \'red.\' or \'blue.\'\n'), player_select_side(Side))
    ).

player_input_pos(position(X, Y)) :-
    read(A-B),
    (
        (position(A, B), X = A, Y = B, !);
        (write('Invalid position. Retry: '), player_input_pos(position(X, Y)))
    ).

/*predicate to request player to input a movement*/
player_input_move(Side, board(Red, Blue), NewBoard):- 
    write('Choose piece to move: (X-Y)'),
    player_input_pos(position(X1, Y1)), 
    write('Choose where to move: (X-Y)'),
    player_input_pos(position(X2, Y2)),
    listBoards(Side, board(Red, Blue), ListCurrentPlayerMoves, ListOpponent),
    (
      (listBoards(Side, NewBoard, NewListCurrentPlayerMoves, NewListOpponent),
       /*possible_move(Side, piece(_,X1,Y1), piece(_,X2,Y2)),*/
       piece_color(ListCurrentPlayerMoves, X1,Y1, Color),
       move_is_possible(Side,piece(Color,X1,Y1), piece(_,X2,Y2)),
       way_is_free(Side, ListCurrentPlayerMoves, ListOpponent, piece(_,X2,Y2), NewListOpponent),
       delete_from_list(ListCurrentPlayerMoves, piece(Color,X1,Y1), ListCurrentPlayerMovesTemp),
       append_list(ListCurrentPlayerMovesTemp, [piece(Color,X2, Y2)], NewListCurrentPlayerMoves)
       /*move(Side, board(Red, Blue), NewBoard, piece(Color,X1,Y1), piece(_,X2,Y2))*/
        ); 
      write('That move is not valid. Try again: (X-Y)'), player_input_move(Side, board(Red, Blue), NewBoard)
    ),!.
/*------------------------------------------------------------------------------*/
game(Side, Board1):-
        other_player(Side, OtherSide),
        print_board(Board1),
        can_move(OtherSide, Board1),
        player_input_move(Side, Board1, Board2),
        write('\n'), write(Side), write(' moving:\n'),
        print_board(Board2),
        can_move(OtherSide, Board2),
        smart_move(OtherSide, Board2, Board3),
        write('\n'), write(OtherSide), write(' moving:\n'),
        game(Side, Board3).



play:-
	initial_board(B),
  player_select_side(Side),
	game(Side, B).
