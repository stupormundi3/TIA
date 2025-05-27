:- module(pontuxl_game, [
    initial_bridges/1,
    initial_state/1,
    valid_move/3,
    apply_move_and_action/4,
    game_over/2,
    collect_valid_moves/3,
    ai_select_move/3,
    player_order/1,
    next_player/2,
    valid_square/2,
    valid_bridge/1,
    canonical_bridge/8,
    can_rotate/4,
    evaluate_board/3
]).

:- use_module(library(lists)).
:- use_module(library(random)).

% Transposition table for AI
:- dynamic transposition/3.

% Valid square check
valid_square(X, Y) :-
    between(1, 6, X),
    between(1, 6, Y).

% Generate initial bridges
initial_bridges(Bridges) :-
    findall(bridge((X1,Y1)-(X2,Y2)),
            (
                (between(1, 5, X1), between(1, 6, Y1),
                 X2 is X1 + 1, Y2 = Y1);
                (between(1, 6, X1), between(1, 5, Y1),
                 X2 = X1, Y2 is Y1 + 1)
            ),
            Bridges).

% Bridge validation
valid_bridge(bridge((X1,Y1)-(X2,Y2))) :-
    (X1 < X2 ; (X1 = X2, Y1 < Y2)).

% Initial game state
initial_state(state(Goblins, Bridges, Player, Phase, Mode)) :-
    initial_bridges(Bridges),
    % Check for both string "random" and atom random
    ((Mode = "random" ; Mode = random) ->
        player_order(Players),
        random_placement([], Goblins, Players),
        Player = green,
        Phase = play
    ; % Manual mode
        Goblins = [],
        Player = green,
        Phase = placement
    ).

% Player order
player_order([green, blue, yellow, red]).

% Get next player
next_player(Current, Next) :-
    player_order(Players),
    nth0(Index, Players, Current),
    NextIndex is (Index + 1) mod 4,
    nth0(NextIndex, Players, Next).

% Random placement
random_placement(GoblinsIn, GoblinsOut, Players) :-
    findall((X,Y), valid_square(X, Y), AllSquares),
    random_placement_players(Players, GoblinsIn, GoblinsOut, AllSquares).

random_placement_players([], Goblins, Goblins, _).
random_placement_players([Player|Players], GoblinsIn, GoblinsOut, AvailableSquares) :-
    place_player_goblins(Player, 4, GoblinsIn, GoblinsMid, AvailableSquares, NewAvailableSquares),
    random_placement_players(Players, GoblinsMid, GoblinsOut, NewAvailableSquares).

place_player_goblins(_, 0, Goblins, Goblins, Squares, Squares) :- !.
place_player_goblins(Player, N, GoblinsIn, GoblinsOut, AvailableSquares, NewAvailableSquares) :-
    N > 0,
    random_member((X,Y), AvailableSquares),
    \+ member(goblin(_, X, Y), GoblinsIn),
    NewGoblins = [goblin(Player, X, Y)|GoblinsIn],
    select((X,Y), AvailableSquares, RemainingSquares),
    N1 is N - 1,
    place_player_goblins(Player, N1, NewGoblins, GoblinsOut, RemainingSquares, NewAvailableSquares).

% Valid move
valid_move(state(Goblins, Bridges, Player, play, Mode), state(NewGoblins, Bridges, NextPlayer, play, Mode), Move) :-
    Move = move(goblin(Player, X1, Y1), (X2, Y2), _),
    member(goblin(Player, X1, Y1), Goblins),
    (member(bridge((X1,Y1)-(X2,Y2)), Bridges) ;
     member(bridge((X2,Y2)-(X1,Y1)), Bridges)),
    \+ member(goblin(_, X2, Y2), Goblins),
    select(goblin(Player, X1, Y1), Goblins, TempGoblins),
    append([goblin(Player, X2, Y2)], TempGoblins, NewGoblins),
    next_player(Player, NextPlayer).

% Skip move if blocked
valid_move(state(Goblins, Bridges, Player, play, Mode), state(Goblins, Bridges, NextPlayer, play, Mode), Move) :-
    Move = move(none, none, _),
    \+ (member(goblin(Player, X1, Y1), Goblins),
        (member(bridge((X1,Y1)-(X2,Y2)), Bridges) ;
         member(bridge((X2,Y2)-(X1,Y1)), Bridges)),
        \+ member(goblin(_, X2, Y2), Goblins)),
    next_player(Player, NextPlayer).

% Apply move and action
apply_move_and_action(State, Move, Action, FinalState) :-
    valid_move(State, MidState, Move),
    apply_bridge_action(MidState, Action, FinalState).

% Bridge actions
apply_bridge_action(State, none, State) :- !.

apply_bridge_action(State, remove(X1, Y1, X2, Y2), NewState) :-
    State = state(Goblins, Bridges, Player, Phase, Mode),
    canonical_bridge(X1, Y1, X2, Y2, CanX1, CanY1, CanX2, CanY2),
    select(bridge((CanX1,CanY1)-(CanX2,CanY2)), Bridges, RemainingBridges),
    NewState = state(Goblins, RemainingBridges, Player, Phase, Mode).

apply_bridge_action(State, rotate(X1, Y1, X2, Y2, PivotX, PivotY), NewState) :-
    State = state(Goblins, Bridges, Player, Phase, Mode),
    canonical_bridge(X1, Y1, X2, Y2, CanX1, CanY1, CanX2, CanY2),
    can_rotate(bridge((CanX1,CanY1)-(CanX2,CanY2)), PivotX, PivotY, RotatedBridge),
    select(bridge((CanX1,CanY1)-(CanX2,CanY2)), Bridges, TempBridges),
    NewBridges = [RotatedBridge|TempBridges],
    NewState = state(Goblins, NewBridges, Player, Phase, Mode).

% Canonical bridge form
canonical_bridge(X1, Y1, X2, Y2, CanX1, CanY1, CanX2, CanY2) :-
    (X1 < X2 ; (X1 = X2, Y1 < Y2)) ->
        CanX1 = X1, CanY1 = Y1, CanX2 = X2, CanY2 = Y2
    ;
        CanX1 = X2, CanY1 = Y2, CanX2 = X1, CanY2 = Y1.

% Bridge rotation
can_rotate(bridge((X1,Y1)-(X2,Y2)), PivotX, PivotY, NewBridge) :-
    ((PivotX = X1, PivotY = Y1) ; (PivotX = X2, PivotY = Y2)),
    (
        (X2 is X1 + 1, Y1 = Y2) ->
            ((PivotX = X1, PivotY = Y1) ->
                NewX1 = X1, NewY1 = Y1,
                NewX2 = X1, NewY2 is Y1 + 1
            ;
                NewX1 = X2, NewY1 is Y2 - 1,
                NewX2 = X2, NewY2 = Y2
            )
        ;
        (X1 = X2, Y2 is Y1 + 1) ->
            ((PivotX = X1, PivotY = Y1) ->
                NewX1 = X1, NewY1 = Y1,
                NewX2 is X1 + 1, NewY2 = Y1
            ;
                NewX1 is X2 - 1, NewY1 = Y2,
                NewX2 = X2, NewY2 = Y2
            )
    ),
    valid_square(NewX1, NewY1),
    valid_square(NewX2, NewY2),
    canonical_bridge(NewX1, NewY1, NewX2, NewY2, FinalX1, FinalY1, FinalX2, FinalY2),
    NewBridge = bridge((FinalX1,FinalY1)-(FinalX2,FinalY2)).

% Check if player is eliminated
player_eliminated(Player, Goblins, Bridges) :-
    findall(goblin(Player, X, Y), member(goblin(Player, X, Y), Goblins), PlayerGoblins),
    PlayerGoblins \= [],
    forall(member(goblin(Player, X, Y), PlayerGoblins),
           \+ ((member(bridge((X,Y)-(_,_)), Bridges) ;
                member(bridge((_,_)-(X,Y)), Bridges)))).

% Game over
game_over(state(Goblins, Bridges, _, play, _), Winner) :-
    findall(Player,
            (player_order(Players),
             member(Player, Players),
             member(goblin(Player, _, _), Goblins),
             \+ player_eliminated(Player, Goblins, Bridges)),
            ActivePlayers),
    (ActivePlayers = [Winner] ; ActivePlayers = [], Winner = 'No one').

% Simple AI
ai_select_move(State, Move, Action) :-
    State = state(_, _, Player, _, _),
    collect_valid_moves(State, Player, Moves),
    (Moves = [] ->
        Move = move(none, none, _),
        Action = none
    ;
        select_random_move(Moves, Move, Action)
    ).

% Collect valid moves
collect_valid_moves(State, Player, Moves) :-
    findall(Move-none,
            (Move = move(goblin(Player, X1, Y1), (X2, Y2), _),
             valid_move(State, _, Move)),
            SimpleMoves),
    (SimpleMoves = [] ->
        BaseMoves = [move(none, none, _)-none]
    ;
        BaseMoves = SimpleMoves
    ),
    State = state(_, Bridges, _, _, _),
    findall(Move-remove(X1, Y1, X2, Y2),
            (member(Move-_, BaseMoves),
             member(bridge((X1,Y1)-(X2,Y2)), Bridges)),
            RemoveMoves),
    findall(Move-rotate(X1, Y1, X2, Y2, PX, PY),
            (member(Move-_, BaseMoves),
             member(bridge((X1,Y1)-(X2,Y2)), Bridges),
             (PX = X1, PY = Y1 ; PX = X2, PY = Y2),
             can_rotate(bridge((X1,Y1)-(X2,Y2)), PX, PY, _)),
            RotateMoves),
    append([BaseMoves, RemoveMoves, RotateMoves], AllMoves),
    sort(AllMoves, Moves).

% Helper functions
select_random_move(Moves, Move, Action) :-
    length(Moves, Length),
    Length > 0,
    random(0, Length, Index),
    nth0(Index, Moves, Move-Action).

% Simplified evaluation
evaluate_board(state(Goblins, Bridges, _, _, _), Player, Score) :-
    findall(goblin(Player, X, Y), member(goblin(Player, X, Y), Goblins), PlayerGoblins),
    length(PlayerGoblins, PlayerCount),
    (player_eliminated(Player, Goblins, Bridges) ->
        Score = -500
    ;
        Score is PlayerCount * 10
    ).
