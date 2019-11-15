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
can_move(Side, Board) :- move(Side, Board, _), !