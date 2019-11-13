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

/*---------------------------------------------------------------------------------------------*/
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


/*User Input ------------------------------------------------------------------*/
player_select_side(Side) :-
    write('Select your side (red/blue): '),
    read(UserSide),
    (
        (player(UserSide), Side = UserSide, !);
        (write('Invalid side, please type \'red.\' or \'blue.\'\n'), player_select_side(Side))
    ).

draw:-
	initial_board(B),
  /*sample2(B), */
  player_select_side(Side),
	print_board(B).
