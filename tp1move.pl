:-use_module(library(between)).

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

coordinate(X) :- list_member([0, 1, 2, 3, 4], X).

/* Any valid cell on the board.*/
cell(X, Y) :- coordinate(Y), coordinate(X).

position(X, Y) :-
    cell(X, Y).


/*---------------------------------------------------------------------------------------------*/
list_member(List, Member):- member(Member, List).
list_absent(List, Element):- \+ (member(Element, List)).
delete_from_list(List, Element, NewList):- select(Element, List, NewList).
append_list(List1, List2, Result):- append(List1, List2, Result).

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
move(Side, board(Red, Blue), board(NewRed, NewBlue)) :-
    move_figure(Side, board(Red, Blue), board(NewRed, NewBlue)).

/*predicado que acede à lista de pecas remove a peca escolhida do sitio prévio */
/*  e adiciona uma nova com a nova posicao escolhida*/
move_figure(Side, ListPlayer, ListOpponent, NewListPlayer):-
            delete_from_list(ListPlayer, InitialPosition, TmpListPlayer),
            possible_move(Side, InitialPosition, FinalPosition),
            way_is_free(ListPlayer, ListOpponent, InitialPosition, FinalPosition),
            append_list(TmpListPlayer, [FinalPosition], NewListPlayer).


move_figure(red, board(Red, Blue), board(NewRed, Blue)) :-
    move_figure(red, Red, Blue, NewRed).
move_figure(blue, board(Red, Blue), board(Red, NewBlue)) :-
    move_figure(blue, Blue, Red, NewBlue).

possible_move(red,piece(ColorPiece,X,Y), piece(_,X2,Y2))):-
  board_tile(ColorBoard, X, Y),
  ((Y2 is Y+1); (Y2 is Y-1),
  (X2 is X+1); (X2 is X-1)),
  ((ColorBoard==ColorPiece) -> knight_movement(Xi/Yi,Xf/Yf); bishop_movement(Xi/Yi,Xf/Yf)).

possible_move(blue,piece(ColorPiece,X,Y), piece(_,X2,Y2))):-
  board_tile(ColorBoard, X, Y),
  ((Y2 is Y+1); (Y2 is Y-1),
  (X2 is X+1); (X2 is X-1)),
  inside_board((X2,Y2),
  ((ColorBoard==ColorPiece) -> knight_movement(Xi/Yi,Xf/Yf); bishop_movement(Xi/Yi,Xf/Yf)).
  

knight_movement(Xs/Ys,Xd/Yd):-
        ground(Xs),ground(Ys),
        (knight_mov(DistX,DistY);knight_mov(DistY,DistX)),
        Xd is Xs+DistX,
        Yd is Ys+DistY,
        inside_board(Xd/Yd).

bishop_movement(Xi/Yi,Xf/Yf):-
        Xi \= Xf, Yi \= Yf,
        DiagDiffi is (Xi-Yi), DiagDifff is (Xf-Yf),
        DiagSumi is (Xi+Yi), DiagSumf is (Xf+Yf),
        (DiagDiffi = DiagDifff ; DiagSumi = DiagSumf)
        inside_board(Xf/Yf).

inside_board((X/Y)):-
      between(0,4,X),
      between(0,4,Y).

knight_mov(-2,-1).
knight_mov(-2,1).
knight_mov(2,-1).
knight_mov(2,1).

/* Predicate to check if the way is free */
way_is_free(ListCurrentPlayerMoves, ListNoMoves, piece(_, X2, Y2)) :-
    list_absent(ListCurrentPlayerMoves, piece(_, X2, Y2)),
    list_absent(ListNoMoves, piece(_, X2, Y2)).


/* Definition of move */
move(Side, board(Red, Blue), board(NewRed,NewBlue)):-
            move_figure(Side, board(Red,Blue), board(NewRed,NewBlue)).




/*User Input ------------------------------------------------------------------*/

/*predicate to request player to choose a side*/
can_move(Side, Board) :- move(Side, Board, _), !.
player_select_side(Side) :-
    write('Select your side (red/blue): '),
    read(UserSide),
    (
        (player(UserSide), Side = UserSide, !);
        (write('Invalid side, please type \'red.\' or \'blue.\'\n'), player_select_side(Side))
    ).

/*predicate to request player to input a movement*/
player_input_move(Side, board(Red, Blue), NewBoard):- 
    write('Choose piece to move: (X-Y)'),
    player_input_pos(position(X1, Y1)), 
    write('Choose where to move: (X-Y)')
    player_input_pos(position(X2, Y2)),
    listBoards(Side, board(Red, Blue), ListCurrentPlayerMoves, ListNoMoves),
    (
      (delete_from_list(ListCurrentPlayerMoves, position(X1,Y1), ListCurrentPlayerMOvesTemp),
       append_list(ListCurrentPlayerMovesTemp, position(X2, Y2), NewListCurrentPLayerMoves), 
       listBoards(Side, NewBoard, NewListCurrentPLayerMoves, ListNoMoves),
       move(Side, board(Red, Blue), NewBoard)
        ); 
      write('That move is not valid. Try again: (X-Y)'), player_input_move(Side, board(Red, Blue), NewBoard)
    ),!.
/*------------------------------------------------------------------------------*/
game(Side, Board1):-
        otherSide(Side, otherSide),
        print_board(Board1),
        /*avaliacao_estado(),*/
        player_input_move(Side, Board1, Board2),



draw:-
	initial_board(B),
  /*sample2(B), */
  player_select_side(Side),
	print_board(B).
