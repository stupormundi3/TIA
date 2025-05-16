% PontuXL Game with Enhanced Alpha-Beta AI and Improved Bridge Management
% Features:
% - 6x6 board, 4 players (Green/Yellow human, Blue/Red AI)
% - Movement in all directions (up, down, left, right)
% - Enhanced bridge manipulation: Remove any existing bridge, not just connected ones
% - Improved alpha-beta pruning with move ordering, transposition table, quiescence search
% - Fixed evaluation function for more accurate state assessment
% - Iterative deepening with 5-second timeout per depth using call_with_time_limit/2




:- module(pontuxl_api, [
    start_server/1
]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_log)).
:- use_module(project_v1).

:- dynamic game_state/2.
:- dynamic ai_difficulty/2.

% HTTP Handlers
:- http_handler(root(.), handle_root, []).
:- http_handler(root(new_game), handle_new_game, [methods([post, options])]).
:- http_handler(root(game_state), handle_get_state, [methods([get, options])]).
:- http_handler(root(make_move), handle_make_move, [methods([post, options])]).
:- http_handler(root(valid_moves), handle_valid_moves, [methods([get, options])]).
:- http_handler(root(game_over), handle_game_over, [methods([get, options])]).
:- http_handler(root(ai_settings), handle_ai_settings, [methods([post, options])]).
:- http_handler(root(available_games), handle_available_games, [methods([get, options])]).
:- http_handler(root(save_game), handle_save_game, [methods([post, options])]).
:- http_handler(root(load_game), handle_load_game, [methods([post, options])]).

% Enable CORS with security settings
:- set_setting(http:cors, [
    methods([get, post, delete, options]),
    allow_origin(['http://localhost:3000', 'https://pontuxl-game.example.com']), 
    allow_headers([accept, authorization, content-type, x-requested-with])
]).

% Start the HTTP server with better logging
start_server(Port) :-
    http_server(http_dispatch, [
        port(Port),
        timeout(60),
        keep_alive_timeout(20)
    ]),
    format('PontuXL API server running on port ~w~n', [Port]),
    format('API documentation available at http://localhost:~w/~n', [Port]).

% Root handler - API documentation
handle_root(_Request) :-
    reply_html_page(
        title('PontuXL API Documentation'),
        [
            h1('PontuXL Game API'),
            p('Welcome to the PontuXL Game API. Below are the available endpoints:'),
            ul([
                li(['POST /new_game - Start a new game']),
                li(['GET /game_state?game_id=ID - Get current game state']),
                li(['POST /make_move - Make a move and bridge action']),
                li(['GET /valid_moves?game_id=ID&player=PLAYER - Get valid moves']),
                li(['GET /game_over?game_id=ID - Check if game is over']),
                li(['POST /ai_settings - Configure AI difficulty']),
                li(['GET /available_games - List available game IDs']),
                li(['POST /save_game - Save current game state']),
                li(['POST /load_game - Load a saved game state'])
            ]),
            h2('API Examples'),
            pre('{
    "game_id": "default",
    "move": {
        "type": "goblin", 
        "player": "green", 
        "from": {"x": 1, "y": 2},
        "to": {"x": 1, "y": 3}
    },
    "action": {
        "type": "remove",
        "bridge": {
            "from": {"x": 4, "y": 3}, 
            "to": {"x": 4, "y": 4}
        }
    }
}')
        ]
    ).

% --- Handler: Start new game ---
handle_new_game(Request) :-
    % Enhanced error handling for JSON parsing
    catch(
        http_read_json_dict(Request, Params),
        Error,
        (   
            print_message(warning, Error),
            reply_json_dict(_{
                status: error, 
                message: 'Invalid JSON request',
                details: Error
            }, [status(400)])
        )
    ),
    
    % Extract parameters with defaults
    (get_dict(mode, Params, Mode) -> true ; Mode = random),
    (get_dict(game_id, Params, GameID) -> true ; 
        % Generate unique game ID if not provided
        get_time(Time),
        format(atom(GameID), 'game_~w', [Time])
    ),
    (get_dict(ai_difficulty, Params, Difficulty) -> 
        assertz(ai_difficulty(GameID, Difficulty))
    ; 
        assertz(ai_difficulty(GameID, medium))
    ),
    
    % Initialize game state
    initial_bridges(Bridges),
    ( Mode = manual -> 
        Goblins = [], 
        Phase = placement
    ; Mode = random ->
        player_order(Players),
        random_placement([], Goblins, Players),
        Phase = play
    ),
    Player = green,
    
    % Update game state
    retractall(game_state(GameID, _)),
    assertz(game_state(GameID, state(Goblins, Bridges, Player, Phase, Mode))),
    
    % Respond with game information
    state_to_json(state(Goblins, Bridges, Player, Phase, Mode), StateJSON),
    reply_json_dict(_{
        status: success, 
        game_id: GameID,
        state: StateJSON,
        message: 'Game created successfully'
    }).

% --- Handler: Get game state ---
handle_get_state(Request) :-
    % Parameter handling with better error reporting
    catch(
        http_parameters(Request, [
            game_id(GameID, [optional(false), atom])
        ]),
        Error,
        (
            print_message(warning, Error),
            reply_json_dict(_{
                status: error, 
                message: 'Missing or invalid game_id parameter'
            }, [status(400)])
        )
    ),
    
    % Get game state with proper error handling
    ( game_state(GameID, State) ->
        state_to_json(State, JSON),
        reply_json_dict(JSON)
    ; 
        reply_json_dict(_{
            status: error, 
            message: 'Game not found',
            game_id: GameID
        }, [status(404)])
    ).

% --- Handler: Make a move ---
handle_make_move(Request) :-
    % Parse JSON with error handling
    catch(
        http_read_json_dict(Request, Dict),
        Error,
        (
            print_message(warning, Error),
            reply_json_dict(_{
                status: error, 
                message: 'Invalid JSON request',
                details: Error
            }, [status(400)])
        )
    ),
    
    % Validate required fields
    (get_dict(game_id, Dict, GameID) -> true ; 
        reply_json_dict(_{
            status: error, 
            message: 'Missing game_id field'
        }, [status(400)]),
        !,
        fail
    ),
    
    % Handle move
    ( game_state(GameID, State) ->
        % Extract move and action
        (get_dict(move, Dict, MoveDict) -> true ; 
            reply_json_dict(_{
                status: error, 
                message: 'Missing move field'
            }, [status(400)]),
            !,
            fail
        ),
        (get_dict(action, Dict, ActionDict) -> true ; 
            reply_json_dict(_{
                status: error, 
                message: 'Missing action field'
            }, [status(400)]),
            !,
            fail
        ),
        
        % Process move and action
        catch(
            (
                json_to_move(MoveDict, Move),
                json_to_action(ActionDict, Action),
                ( apply_move_and_action(State, Move, Action, NewState) ->
                    retract(game_state(GameID, _)),
                    assertz(game_state(GameID, NewState)),
                    
                    % Process AI moves if enabled and it's AI's turn
                    NewState = state(_, _, NextPlayer, Phase, _),
                    process_ai_turns(GameID, NextPlayer),
                    
                    % Get updated state after AI moves
                    game_state(GameID, FinalState),
                    state_to_json(FinalState, StateJSON),
                    
                    reply_json_dict(_{
                        status: success,
                        message: 'Move processed successfully',
                        state: StateJSON
                    })
                ; 
                    reply_json_dict(_{
                        status: error, 
                        message: 'Invalid move or action'
                    }, [status(400)])
                )
            ),
            Error,
            (
                print_message(warning, Error),
                reply_json_dict(_{
                    status: error, 
                    message: 'Error processing move',
                    details: Error
                }, [status(500)])
            )
        )
    ; 
        reply_json_dict(_{
            status: error, 
            message: 'Game not found'
        }, [status(404)])
    ).

% Process AI turns automatically if current player is AI
process_ai_turns(GameID, Player) :-
    % Process turns for AI players (blue and red)
    process_ai_turn(GameID, Player).

process_ai_turn(GameID, Player) :-
    ( member(Player, [blue, red]),
      game_state(GameID, State),
      % Check if game is still active
      \+ game_over(State, _) ->
        % Get AI difficulty
        (ai_difficulty(GameID, Difficulty) -> true ; Difficulty = medium),
        
        % Select move based on difficulty level
        select_ai_move(State, Player, Difficulty, Move, Action),
        
        % Apply move
        apply_move_and_action(State, Move, Action, NewState),
        retract(game_state(GameID, _)),
        assertz(game_state(GameID, NewState)),
        
        % Process next AI turn if applicable
        NewState = state(_, _, NextPlayer, _, _),
        process_ai_turn(GameID, NextPlayer)
    ; 
        % Not an AI player or game is over, do nothing
        true
    ).

% Select AI move based on difficulty
select_ai_move(State, Player, Difficulty, Move, Action) :-
    ( Difficulty = easy ->
        % Easy: Random valid moves
        collect_valid_moves(State, Player, Moves),
        select_random_move(Moves, Move, Action)
    ; Difficulty = medium ->
        % Medium: Limited depth search (3)
        MaxDepth = 3,
        catch(
            find_best_depth_move(State, Player, 1, MaxDepth, move(none, none, none), none, Move, Action),
            _,
            (collect_valid_moves(State, Player, Moves),
             select_random_move(Moves, Move, Action))
        )
    ; % Difficulty = hard (default)
        % Hard: Deeper search (5) with quiescence
        ai_select_move(State, Move, Action)
    ).

% --- Handler: Get valid moves ---
handle_valid_moves(Request) :-
    catch(
        http_parameters(Request, [
            game_id(GameIDParam, []),
            player(PlayerAtom, [])
        ]),
        Error,
        (
            print_message(warning, Error),
            reply_json_dict(_{
                status: error, 
                message: 'Missing or invalid parameters'
            }, [status(400)])
        )
    ),
    
    atom_string(Player, PlayerAtom),
    
    ( game_state(GameIDParam, State) ->
        collect_valid_moves(State, Player, Moves),
        maplist(move_action_to_json, Moves, JSONMoves),
        reply_json_dict(_{
            status: success,
            moves: JSONMoves,
            count: length(JSONMoves, Count),
            player: Player
        })
    ; 
        reply_json_dict(_{
            status: error, 
            message: 'Game not found'
        }, [status(404)])
    ).

% --- Handler: Check if game is over ---
handle_game_over(Request) :-
    catch(
        http_parameters(Request, [
            game_id(GameID, [])
        ]),
        Error,
        (
            print_message(warning, Error),
            reply_json_dict(_{
                status: error, 
                message: 'Missing or invalid game_id parameter'
            }, [status(400)])
        )
    ),
    
    ( game_state(GameID, State) ->
        ( game_over(State, Winner) ->
            reply_json_dict(_{
                status: success,
                game_over: true, 
                winner: Winner,
                message: format('Game over! ~w wins!', [Winner])
            })
        ; 
            reply_json_dict(_{
                status: success,
                game_over: false,
                message: 'Game is still in progress'
            })
        )
    ; 
        reply_json_dict(_{
            status: error, 
            message: 'Game not found'
        }, [status(404)])
    ).

% --- NEW Handler: AI Settings ---
handle_ai_settings(Request) :-
    catch(
        http_read_json_dict(Request, Params),
        Error,
        (
            print_message(warning, Error),
            reply_json_dict(_{
                status: error, 
                message: 'Invalid JSON request',
                details: Error
            }, [status(400)])
        )
    ),
    
    % Extract parameters
    (get_dict(game_id, Params, GameID) -> true ; 
        reply_json_dict(_{
            status: error, 
            message: 'Missing game_id field'
        }, [status(400)]),
        !,
        fail
    ),
    
    % Check if game exists
    (game_state(GameID, _) -> true ;
        reply_json_dict(_{
            status: error, 
            message: 'Game not found'
        }, [status(404)]),
        !,
        fail
    ),
    
    % Extract difficulty
    (get_dict(difficulty, Params, Difficulty) -> true ; 
        reply_json_dict(_{
            status: error, 
            message: 'Missing difficulty field'
        }, [status(400)]),
        !,
        fail
    ),
    
    % Validate difficulty
    (member(Difficulty, [easy, medium, hard]) -> true ;
        reply_json_dict(_{
            status: error, 
            message: 'Invalid difficulty. Must be one of: easy, medium, hard'
        }, [status(400)]),
        !,
        fail
    ),
    
    % Update AI difficulty
    retractall(ai_difficulty(GameID, _)),
    assertz(ai_difficulty(GameID, Difficulty)),
    
    reply_json_dict(_{
        status: success,
        message: 'AI settings updated successfully',
        game_id: GameID,
        difficulty: Difficulty
    }).

% --- NEW Handler: Available Games ---
handle_available_games(_Request) :-
    findall(GameID, game_state(GameID, _), GameIDs),
    reply_json_dict(_{
        status: success,
        games: GameIDs
    }).

% --- NEW Handler: Save Game ---
handle_save_game(Request) :-
    catch(
        http_read_json_dict(Request, Params),
        Error,
        (
            print_message(warning, Error),
            reply_json_dict(_{
                status: error, 
                message: 'Invalid JSON request',
                details: Error
            }, [status(400)])
        )
    ),
    
    % Extract parameters
    (get_dict(game_id, Params, GameID) -> true ; 
        reply_json_dict(_{
            status: error, 
            message: 'Missing game_id field'
        }, [status(400)]),
        !,
        fail
    ),
    
    % Optionally get save name
    (get_dict(save_name, Params, SaveName) -> true ; SaveName = GameID),
    
    % Check if game exists
    ( game_state(GameID, State) ->
        % Generate a timestamp
        get_time(TimeStamp),
        timestamp_string(TimeStamp, TimeString),
        
        % Convert state to JSON
        state_to_json(State, StateJSON),
        
        % Build save data
        SaveData = _{
            game_id: GameID,
            save_name: SaveName,
            timestamp: TimeString,
            state: StateJSON
        },
        
        % In a real implementation, this would save to disk
        % For this example, well simulate success
        reply_json_dict(_{
            status: success,
            message: 'Game saved successfully',
            save_data: SaveData
        })
    ; 
        reply_json_dict(_{
            status: error, 
            message: 'Game not found'
        }, [status(404)])
    ).

% --- NEW Handler: Load Game ---
handle_load_game(Request) :-
    catch(
        http_read_json_dict(Request, Params),
        Error,
        (
            print_message(warning, Error),
            reply_json_dict(_{
                status: error, 
                message: 'Invalid JSON request',
                details: Error
            }, [status(400)])
        )
    ),
    
    % Extract parameters
    (get_dict(save_data, Params, SaveData) -> true ; 
        reply_json_dict(_{
            status: error, 
            message: 'Missing save_data field'
        }, [status(400)]),
        !,
        fail
    ),
    
    % Extract game ID and state
    (get_dict(game_id, SaveData, GameID) -> true ;
        reply_json_dict(_{
            status: error, 
            message: 'Missing game_id in save data'
        }, [status(400)]),
        !,
        fail
    ),
    
    % Extract state
    (get_dict(state, SaveData, StateJSON) -> true ;
        reply_json_dict(_{
            status: error, 
            message: 'Missing state in save data'
        }, [status(400)]),
        !,
        fail
    ),
    
    % Convert JSON state back to Prolog term
    catch(
        json_to_state(StateJSON, State),
        _,
        (
            reply_json_dict(_{
                status: error, 
                message: 'Invalid state format in save data'
            }, [status(400)]),
            !,
            fail
        )
    ),
    
    % Update game state
    retractall(game_state(GameID, _)),
    assertz(game_state(GameID, State)),
    
    reply_json_dict(_{
        status: success,
        message: 'Game loaded successfully',
        game_id: GameID,
        state: StateJSON
    }).

% --- Convert state to JSON ---
state_to_json(state(Goblins, Bridges, Player, Phase, Mode), _{
    goblins: GoblinList,
    bridges: BridgeList,
    current_player: Player,
    phase: Phase,
    mode: Mode,
    board_size: 6  % Added board size for frontend reference
}) :-
    maplist(goblin_to_json, Goblins, GoblinList),
    maplist(bridge_to_json, Bridges, BridgeList).

% --- Convert JSON to state ---
json_to_state(JSON, state(Goblins, Bridges, Player, Phase, Mode)) :-
    % Extract components
    get_dict(goblins, JSON, GoblinJSON),
    maplist(json_to_goblin, GoblinJSON, Goblins),
    
    get_dict(bridges, JSON, BridgeJSON),
    maplist(json_to_bridge, BridgeJSON, Bridges),
    
    get_dict(current_player, JSON, Player),
    get_dict(phase, JSON, Phase),
    get_dict(mode, JSON, Mode).

goblin_to_json(goblin(Player, X, Y), _{
    player: Player, 
    x: X, 
    y: Y,
    type: goblin
}).

json_to_goblin(_{player: Player, x: X, y: Y}, goblin(Player, X, Y)).

bridge_to_json(bridge((X1,Y1)-(X2,Y2)), _{
    from: _{x: X1, y: Y1},
    to: _{x: X2, y: Y2},
    type: bridge
}).

json_to_bridge(_{from: _{x: X1, y: Y1}, to: _{x: X2, y: Y2}}, 
               bridge((X1,Y1)-(X2,Y2))).

% --- Move and Action conversion ---
json_to_move(_{type: none}, move(none, none, _)).
json_to_move(_{type: goblin, player: Player, from: _{x: FX, y: FY}, to: _{x: TX, y: TY}},
             move(goblin(Player, FX, FY), (TX, TY), _)).

json_to_action(_{type: none}, none).
json_to_action(_{type: remove, bridge: _{from: _{x: X1, y: Y1}, to: _{x: X2, y: Y2}}},
               remove(X1, Y1, X2, Y2)).
json_to_action(_{type: rotate, bridge: _{from: _{x: X1, y: Y1}, to: _{x: X2, y: Y2}}, pivot: _{x: PX, y: PY}},
               rotate(X1, Y1, X2, Y2, PX, PY)).

move_action_to_json(Move-Action, _{move: MoveJson, action: ActionJson}) :-
    move_to_json(Move, MoveJson),
    action_to_json(Action, ActionJson).

move_to_json(move(none, none, _), _{type: none}).
move_to_json(move(goblin(Player, FX, FY), (TX, TY), _), _{
    type: goblin,
    player: Player,
    from: _{x: FX, y: FY},
    to: _{x: TX, y: TY}
}).

action_to_json(none, _{type: none}).
action_to_json(remove(X1,Y1,X2,Y2), _{
    type: remove,
    bridge: _{from: _{x: X1, y: Y1}, to: _{x: X2, y: Y2}}
}).
action_to_json(rotate(X1,Y1,X2,Y2,PX,PY), _{
    type: rotate,
    bridge: _{from: _{x: X1, y: Y1}, to: _{x: X2, y: Y2}},
    pivot: _{x: PX, y: PY}
}).

% Convert timestamp to string for save/load functionality
timestamp_string(TimeStamp, TimeString) :-
    format_time(string(TimeString), '%Y-%m-%d %H:%M:%S', TimeStamp).

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(timeout)). % For call_with_time_limit/2
:- dynamic transposition/3. % Transposition table: transposition(Hash, Depth, Score)
:- dynamic debug_quiescence/1. % Debug logging control (set debug_quiescence(true) to enable)

% --- Utility Predicates for Infinity Handling ---
% Check if a value is +inf or -inf
is_inf(X) :- float(X), (X = +inf ; X = -inf).

% Check if a value is a valid number (finite or infinity)
is_valid_number(X) :- number(X).
is_valid_number(X) :- is_inf(X).

% --- Board Setup (For All Directions) ---
valid_square(X, Y) :- between(1, 6, X), between(1, 6, Y).

% Initialize all possible bridges (up, down, left, right)
initial_bridges(Bridges) :-
    findall(bridge((X1,Y1)-(X2,Y2)),
            ( ( % Horizontal bridges: right (X,Y) to (X+1,Y)
                between(1, 5, X1), between(1, 6, Y1),
                X2 is X1 + 1, Y2 = Y1,
                valid_square(X2, Y2)
              ; % Vertical bridges: up (X,Y) to (X,Y+1)
                between(1, 6, X1), between(1, 5, Y1),
                X2 = X1, Y2 is Y1 + 1,
                valid_square(X2, Y2)
              ),
              valid_bridge(bridge((X1,Y1)-(X2,Y2)))
            ),
            Bridges).

valid_bridge(bridge((X1,Y1)-(X2,Y2))) :-
    % Ensure canonical order - smallest coordinate first
    ( X1 < X2 ; (X1 = X2, Y1 < Y2) ).

initial_state(state(Goblins, Bridges, Player, Phase, Mode)) :-
    initial_bridges(Bridges),
    choose_placement_mode(Mode),
    ( Mode = random ->
        player_order(Players),
        random_placement([], Goblins, Players),
        Player = green,
        Phase = play
    ; Mode = manual ->
        Goblins = [],
        Player = green,
        Phase = placement
    ).

player_order([green, blue, yellow, red]).

next_player(Current, Next) :-
    player_order(Players),
    nth0(Index, Players, Current),
    NextIndex is (Index + 1) mod 4,
    nth0(NextIndex, Players, Next).

choose_placement_mode(Mode) :-
    format('Choose placement mode (manual or random): ', []),
    read(Input),
    ( member(Input, [manual, random]) ->
        Mode = Input
    ; format('Invalid mode: ~w. Please enter "manual" or "random".~n', [Input]),
      choose_placement_mode(Mode)
    ).

random_placement(GoblinsIn, GoblinsOut, Players) :-
    findall((X,Y), valid_square(X, Y), AllSquares),
    random_placement_players(Players, GoblinsIn, GoblinsOut, AllSquares).

random_placement_players([], Goblins, Goblins, _).
random_placement_players([Player|Players], GoblinsIn, GoblinsOut, AvailableSquares) :-
    place_player_goblins(Player, 4, GoblinsIn, GoblinsMid, AvailableSquares, NewAvailableSquares),
    random_placement_players(Players, GoblinsMid, GoblinsOut, NewAvailableSquares).

place_player_goblins(_, 0, Goblins, Goblins, Squares, Squares).
place_player_goblins(Player, N, GoblinsIn, GoblinsOut, AvailableSquares, NewAvailableSquares) :-
    N > 0,
    random_member((X,Y), AvailableSquares),
    \+ member(goblin(_, X, Y), GoblinsIn),
    NewGoblins = [goblin(Player, X, Y)|GoblinsIn],
    select((X,Y), AvailableSquares, RemainingSquares),
    N1 is N - 1,
    place_player_goblins(Player, N1, NewGoblins, GoblinsOut, RemainingSquares, NewAvailableSquares).

% --- Game Loop ---
play_pontuxl :-
    reset_transposition_table,
    initial_state(State),
    format('~nStarting PontuXL! Place goblins on a 6x6 grid.~n', []),
    game_loop(State).

% Reset transposition table before new game
reset_transposition_table :-
    retractall(transposition(_, _, _)).

game_loop(State) :-
    display_state(State),
    ( game_over(State, Winner) ->
        format('~nGame over! ~w wins!~n', [Winner])
    ; play_turn(State, NewState),
      game_loop(NewState)
    ).

% --- State Visualization ---
display_state(state(Goblins, Bridges, Player, Phase, Mode)) :-
    format('~n=== Game State ===~nPhase: ~w~nPlacement Mode: ~w~nCurrent Player: ~w~n', [Phase, Mode, Player]),
    display_board(Goblins, Bridges),
    count_connections_by_player(Goblins, Bridges, ConnectionCounts),
    format('Bridge connections by player: ~w~n', [ConnectionCounts]).

% Count bridge connections per player for display
count_connections_by_player(Goblins, Bridges, ConnectionCounts) :-
    findall(Player-Count,
            (member(goblin(Player, _, _), Goblins),
             sort(Goblins, SortedGoblins),
             findall(1,
                     (member(goblin(Player, X, Y), SortedGoblins),
                      (member(bridge((X,Y)-(_, _)), Bridges);
                       member(bridge((_, _)-(X,Y)), Bridges))),
                     Connections),
             length(Connections, Count)),
            ConnectionCounts).

display_board(Goblins, Bridges) :-
    format('   | 1   2   3   4   5   6  ~n', []),
    format('---+---+---+---+---+---+---~n', []),
    foreach(between(1, 6, Y),
            (format('~w  |', [Y]),
             foreach(between(1, 6, X),
                     display_square(X, Y, Goblins, Bridges)),
             nl)),
    format('---+---+---+---+---+---+---~n', []).

display_square(X, Y, Goblins, Bridges) :-
    ( member(goblin(Player, X, Y), Goblins) ->
        ( Player = green -> Char = 'G'
        ; Player = yellow -> Char = 'Y'
        ; Player = blue -> Char = 'B'
        ; Player = red -> Char = 'R' )
    ; Char = '.'
    ),
    % Check for right bridge
    ( X < 6,
      ( member(bridge((X,Y)-(X2,Y)), Bridges), X2 is X+1 ; 
        member(bridge((X2,Y)-(X,Y)), Bridges), X2 is X+1 ) -> Right = '-'
    ; Right = ' '
    ),
    % Check for down bridge
    ( Y < 6,
      ( member(bridge((X,Y)-(X,Y2)), Bridges), Y2 is Y+1 ;
        member(bridge((X,Y2)-(X,Y)), Bridges), Y2 is Y+1 ) -> Down = '|'
    ; Down = ' '
    ),
    format(' ~w~w~w', [Char, Right, Down]).

% --- Turn Handling (With Enhanced Bridge Management) ---
play_turn(state(Goblins, Bridges, Player, placement, manual), NewState) :-
    format('~w, enter X Y to place goblin (e.g., 1 2): ', [Player]),
    read(Input),
    ( valid_placement_input(Input, X, Y, Goblins) ->
        NewGoblins = [goblin(Player, X, Y) | Goblins],
        next_player(Player, NextPlayer),
        ( length(NewGoblins, 16) -> NewPhase = play ; NewPhase = placement ),
        NewState = state(NewGoblins, Bridges, NextPlayer, NewPhase, manual)
    ; format('Invalid position for ~w at ~w. Try again.~n', [Player, Input]),
      play_turn(state(Goblins, Bridges, Player, placement, manual), NewState)
    ).

play_turn(state(Goblins, Bridges, Player, placement, random), NewState) :-
    NewState = state(Goblins, Bridges, Player, play, random).

play_turn(state(Goblins, Bridges, Player, play, Mode), NewState) :-
    ( member(Player, [red, blue]) ->
        % AI players
        ai_select_move(state(Goblins, Bridges, Player, play, Mode), Move, Action),
        execute_move(Player, Move, Action, state(Goblins, Bridges, Player, play, Mode), NewState)
    ; % Human players
      format('~w, enter move: FromX FromY ToX ToY (e.g., 1 1 1 2) or none: ', [Player]),
      read(FromToInput),
      ( valid_move_input(FromToInput, FromX, FromY, ToX, ToY),
        % Verify move validity before prompting for bridge action
        valid_goblin_move(state(Goblins, Bridges, Player, play, Mode), FromX, FromY, ToX, ToY) ->
          action_bridge(state(Goblins, Bridges, Player, play, Mode), FromX, FromY, ToX, ToY, NewState)
      ; FromToInput = none ->
          % If player cannot move, they can still perform bridge action
          action_bridge(state(Goblins, Bridges, Player, play, Mode), none, none, none, none, NewState)
      ; % Invalid move
        format('Invalid move for ~w: ~w. Try again.~n', [Player, FromToInput]),
        play_turn(state(Goblins, Bridges, Player, play, Mode), NewState)
      )
    ).

% Check if goblin movement is valid before prompting for bridge action
valid_goblin_move(state(Goblins, Bridges, Player, _, _), FromX, FromY, ToX, ToY) :-
    member(goblin(Player, FromX, FromY), Goblins),
    ( % Movement through a bridge in canonical form
      member(bridge((FromX,FromY)-(ToX,ToY)), Bridges)
    ; % Movement through a bridge in reversed form
      member(bridge((ToX,ToY)-(FromX,FromY)), Bridges)
    ),
    \+ member(goblin(_, ToX, ToY), Goblins).

execute_move(Player, Move, Action, state(Goblins, Bridges, _, play, Mode), NewState) :-
    ( Move = move(goblin(Player, FromX, FromY), (ToX, ToY), _) ->
        format('~w moves goblin from (~w,~w) to (~w,~w)~n', [Player, FromX, FromY, ToX, ToY])
    ; Move = move(none, none, _) ->
        format('~w cannot move, performs bridge action~n', [Player])
    ),
    apply_bridge_action(state(Goblins, Bridges, Player, play, Mode), Move, Action, NewState).

action_bridge(State, FromX, FromY, ToX, ToY, NewState) :-
    State = state(_, Bridges, Player, _, _),
    display_available_bridges(Bridges),
    format('~w, enter bridge action:~n', [Player]),
    format('- remove(X1 Y1 X2 Y2) to remove any existing bridge~n', []),
    format('- rotate(X1 Y1 X2 Y2 AroundX AroundY) to rotate a bridge~n', []),
    format('- none if no action is possible~n', []),
    read(ActionInput),
    ( valid_action_input(ActionInput, Action),
      ( Action = remove(X1, Y1, X2, Y2) -> 
          canonical_bridge(X1, Y1, X2, Y2, CanX1, CanY1, CanX2, CanY2),
          member(bridge((CanX1,CanY1)-(CanX2,CanY2)), Bridges)
      ; Action = rotate(X1, Y1, X2, Y2, AroundX, AroundY) ->
          canonical_bridge(X1, Y1, X2, Y2, CanX1, CanY1, CanX2, CanY2),
          member(bridge((CanX1,CanY1)-(CanX2,CanY2)), Bridges)
      ; Action = none ->
          true
      ) ->
        Move = move(goblin(Player, FromX, FromY), (ToX, ToY), Action),
        apply_bridge_action(State, Move, Action, NewState)
    ; format('Invalid or non-existent bridge action for ~w: ~w. Try again.~n', [Player, ActionInput]),
      action_bridge(State, FromX, FromY, ToX, ToY, NewState)
    ).

% Display available bridges to help players
display_available_bridges(Bridges) :-
    format('Available bridges:~n', []),
    forall(member(bridge((X1,Y1)-(X2,Y2)), Bridges),
           format('- Bridge: (~w,~w)->(~w,~w)~n', [X1,Y1,X2,Y2])).

% Convert any bridge coordinates to canonical form (smaller coordinate first)
canonical_bridge(X1, Y1, X2, Y2, CanX1, CanY1, CanX2, CanY2) :-
    ( X1 < X2 ; (X1 = X2, Y1 < Y2) ) -> 
        CanX1 = X1, CanY1 = Y1, CanX2 = X2, CanY2 = Y2
    ; 
        CanX1 = X2, CanY1 = Y2, CanX2 = X1, CanY2 = Y1.

apply_bridge_action(State, Move, Action, NewState) :-
    State = state(Goblins, Bridges, Player, _, Mode),
    valid_move(State, TempState, Move),
    ( Action = none ->
        format('~w performs no bridge action~n', [Player])
    ; format('~w performs bridge action: ~w~n', [Player, Action])
    ),
    TempState = state(NewGoblins, _, NextPlayer, _, _),
    ( Action = remove(X1, Y1, X2, Y2) ->
        canonical_bridge(X1, Y1, X2, Y2, CanX1, CanY1, CanX2, CanY2),
        select(bridge((CanX1,CanY1)-(CanX2,CanY2)), Bridges, RemainingBridges),
        NewState = state(NewGoblins, RemainingBridges, NextPlayer, play, Mode)
    ; Action = rotate(X1, Y1, X2, Y2, AroundX, AroundY) ->
        canonical_bridge(X1, Y1, X2, Y2, CanX1, CanY1, CanX2, CanY2),
        can_rotate(bridge((CanX1,CanY1)-(CanX2,CanY2)), AroundX, AroundY, RotatedBridge),
        select(bridge((CanX1,CanY1)-(CanX2,CanY2)), Bridges, TempBridges),
        NewBridges = [RotatedBridge|TempBridges],
        NewState = state(NewGoblins, NewBridges, NextPlayer, play, Mode)
    ; Action = none ->
        NewState = TempState
    ).

% --- Input Validation ---
valid_placement_input(X-Y, X, Y, Goblins) :-
    integer(X), integer(Y),
    valid_square(X, Y),
    \+ member(goblin(_, X, Y), Goblins).
valid_placement_input([X,Y], X, Y, Goblins) :-
    integer(X), integer(Y),
    valid_square(X, Y),
    \+ member(goblin(_, X, Y), Goblins).

valid_move_input([FromX,FromY,ToX,ToY], FromX, FromY, ToX, ToY) :-
    integer(FromX), integer(FromY), integer(ToX), integer(ToY).
valid_move_input(FromX-FromY-ToX-ToY, FromX, FromY, ToX, ToY) :-
    integer(FromX), integer(FromY), integer(ToX), integer(ToY).

valid_action_input(remove([X1,Y1,X2,Y2]), remove(X1,Y1,X2,Y2)) :-
    integer(X1), integer(Y1), integer(X2), integer(Y2).
valid_action_input(remove(X1-Y1-X2-Y2), remove(X1,Y1,X2,Y2)) :-
    integer(X1), integer(Y1), integer(X2), integer(Y2).
valid_action_input(rotate([X1,Y1,X2,Y2,AroundX,AroundY]), rotate(X1,Y1,X2,Y2,AroundX,AroundY)) :-
    integer(X1), integer(Y1), integer(X2), integer(Y2), integer(AroundX), integer(AroundY).
valid_action_input(rotate(X1-Y1-X2-Y2-AroundX-AroundY), rotate(X1,Y1,X2,Y2,AroundX,AroundY)) :-
    integer(X1), integer(Y1), integer(X2), integer(Y2), integer(AroundX), integer(AroundY).
valid_action_input(none, none).

% --- Move Validation (Enhanced for All Directions) ---
% Valid move: player moves goblin through a bridge to an empty square
valid_move(state(Goblins, Bridges, Player, play, Mode), state(NewGoblins, Bridges, NextPlayer, play, Mode), Move) :-
    Move = move(goblin(Player, X1, Y1), (X2, Y2), _),
    member(goblin(Player, X1, Y1), Goblins),
    % Allow movement in either direction through a bridge
    ( member(bridge((X1,Y1)-(X2,Y2)), Bridges) ; 
      member(bridge((X2,Y2)-(X1,Y1)), Bridges) ),
    \+ member(goblin(_, X2, Y2), Goblins),
    select(goblin(Player, X1, Y1), Goblins, TempGoblins),
    append([goblin(Player, X2, Y2)], TempGoblins, NewGoblins),
    next_player(Player, NextPlayer).

% Valid move: player cannot move but can still perform bridge action
valid_move(state(Goblins, Bridges, Player, play, Mode), state(Goblins, Bridges, NextPlayer, play, Mode), Move) :-
    Move = move(none, none, _),
    % Check if player cannot move any goblin
    \+ (member(goblin(Player, X1, Y1), Goblins),
        (member(bridge((X1,Y1)-(X2,Y2)), Bridges) ; member(bridge((X2,Y2)-(X1,Y1)), Bridges)),
        \+ member(goblin(_, X2, Y2), Goblins)),
    next_player(Player, NextPlayer).

% --- Bridge Rotation ---
can_rotate(bridge((X1,Y1)-(X2,Y2)), PivotX, PivotY, bridge((NewX1,NewY1)-(NewX2,NewY2))) :-
    % Pivot must be one of the bridge endpoints
    ( (PivotX = X1, PivotY = Y1) ; (PivotX = X2, PivotY = Y2) ),
    
    % Horizontal bridge rotation
    ( X2 is X1 + 1, Y1 = Y2 ->
        % Pivot on left end: rotate to vertical upward
        ( (PivotX = X1, PivotY = Y1) ->
            NewX1 = X1, NewY1 = Y1,
            NewX2 = X1, NewY2 is Y1 + 1
        % Pivot on right end: rotate to vertical downward
        ; NewX1 = X2, NewY1 is Y2 - 1,
          NewX2 = X2, NewY2 = Y2
        )
    
    % Vertical bridge rotation
    ; X1 = X2, Y2 is Y1 + 1 ->
        % Pivot on bottom end: rotate to horizontal rightward
        ( (PivotX = X1, PivotY = Y1) ->
            NewX1 = X1, NewY1 = Y1,
            NewX2 is X1 + 1, NewY2 = Y1
        % Pivot on top end: rotate to horizontal leftward
        ; NewX1 is X2 - 1, NewY1 = Y2,
          NewX2 = X2, NewY2 = Y2
        )
    ),
    
    % Ensure rotation result is a valid bridge
    valid_square(NewX1, NewY1),
    valid_square(NewX2, NewY2),
    % Ensure canonical form
    canonical_bridge(NewX1, NewY1, NewX2, NewY2, NewX1, NewY1, NewX2, NewY2).

% --- Win Conditions ---
player_eliminated(Player, Goblins, Bridges) :-
    findall(goblin(Player, X, Y), member(goblin(Player, X, Y), Goblins), PlayerGoblins),
    % A player is eliminated when all their goblins are isolated
    length(PlayerGoblins, GoblinCount),
    GoblinCount > 0, % Must have at least one goblin
    forall(member(goblin(Player, X, Y), PlayerGoblins),
           \+ ((member(bridge((X,Y)-(_,_)), Bridges) ; 
                member(bridge((_,_)-(X,Y)), Bridges)))).

game_over(state(Goblins, Bridges, _, play, _), Winner) :-
    findall(Player,
            (member(Player, [green, yellow, blue, red]),
             % Check if player has any goblins and is not eliminated
             member(goblin(Player, _, _), Goblins),
             \+ player_eliminated(Player, Goblins, Bridges)),
            ActivePlayers),
    ( ActivePlayers = [Winner]     % Last player standing wins
    ; ActivePlayers = [], Winner = 'No one' % Everyone eliminated, no winner
    ).

% --- Helper Predicates ---
select(X, [X|Xs], Xs).
select(X, [Y|Xs], [Y|Ys]) :- select(X, Xs, Ys).

select(X, [X|Xs], Y, [Y|Xs]).
select(X, [Z|Xs], Y, [Z|Ys]) :- select(X, Xs, Y, Ys).

between(Low, High, X) :- integer(Low), integer(High), Low =< High, X is Low.
between(Low, High, X) :- integer(Low), integer(High), Low < High, Next is Low + 1, between(Next, High, X).

% --- Enhanced AI with Improved Alpha-Beta Pruning ---
% AI move selection with better handling of time constraints
ai_select_move(State, Move, Action) :-
    State = state(_, _, Player, _, _),
    format('~w is thinking...~n', [Player]),
    
    % Initialize with default move in case all else fails
    collect_valid_moves(State, Player, Moves),
    ( Moves = [] ->
        format('ERROR: No valid moves available for ~w!~n', [Player]),
        Move = move(none, none, none),
        Action = none
    ; % Ensure theres at least one valid move with bridge action
      select_random_move(Moves, FallbackMove, FallbackAction),
      
      % Try iterative deepening with timeouts
      MaxDepth = 4,
      
      % Try depths 1 to MaxDepth with timeout per depth
      ( catch(
            find_best_depth_move(State, Player, 1, MaxDepth, move(none, none, none), none, BestMove, BestAction),
            _,
            (BestMove = FallbackMove, BestAction = FallbackAction)
        ) ->
          ( BestMove = move(none, none, none) ->
              % If no move was found, use the fallback
              Move = FallbackMove,
              Action = FallbackAction,
              format('~w: Using fallback move: ~w, action: ~w~n', [Player, Move, Action])
          ; % Otherwise use the best move found
            Move = BestMove,
            Action = BestAction,
            format('~w selected move: ~w, action: ~w~n', [Player, Move, Action])
          )
      ; % If finding the best move fails completely
        Move = FallbackMove,
        Action = FallbackAction,
        format('~w: Error in search, using random move: ~w, action: ~w~n', [Player, Move, Action])
      )
    ).

% Try each depth level with timeout, keeping track of the best move
find_best_depth_move(_, _, Depth, MaxDepth, BestMove, BestAction, BestMove, BestAction) :-
    Depth > MaxDepth, !.

find_best_depth_move(State, Player, Depth, MaxDepth, CurrentBestMove, CurrentBestAction, BestMove, BestAction) :-
    Depth =< MaxDepth,
    
    % Clear old transposition entries for a new depth
    retractall(transposition(_, OldDepth, _)),
    OldDepth < Depth,
    
    % Try to find the best move at current depth with timeout
    ( catch(
          call_with_time_limit(3, % 3 second timeout per depth
              minimax(State, Depth, -inf, +inf, MoveScore-(TempMove-TempAction), Player, true)),
          time_limit_exceeded,
          fail) ->
        
        % Successfully found a move at this depth
        NextDepth is Depth + 1,
        format('~w found move at depth ~w: ~w, action: ~w, score: ~w~n', 
               [Player, Depth, TempMove, TempAction, MoveScore]),
        find_best_depth_move(State, Player, NextDepth, MaxDepth, TempMove, TempAction, BestMove, BestAction)
    ; 
        % Timeout at this depth, use best move so far
        NextDepth is MaxDepth + 1, % Skip to end
        find_best_depth_move(State, Player, NextDepth, MaxDepth, CurrentBestMove, CurrentBestAction, BestMove, BestAction)
    ).

% Copy state to prevent side effects
copy_state(state(Goblins, Bridges, Player, Phase, Mode), state(NewGoblins, NewBridges, Player, Phase, Mode)) :-
    copy_term(Goblins, NewGoblins),
    copy_term(Bridges, NewBridges).

% Core alpha-beta pruning algorithm with improved safety checks
minimax(State, Depth, Alpha, Beta, BestMove, Player, IsMax) :-
    % Check transposition table first
    hash_state(State, Hash),
    ( transposition(Hash, StoredDepth, StoredScore), StoredDepth >= Depth ->
        % Use stored result
        BestMove = StoredScore-(move(none, none, none)-none)
    ; Depth > 0,
      \+ game_over(State, _),
      State = state(_, _, CurrentPlayer, _, _),
      collect_valid_moves(State, CurrentPlayer, Moves),
      Moves \= [] ->
        % Order moves for better pruning efficiency
        order_moves(State, Player, Moves, OrderedMoves),
        
        % Initialize best move with worst possible value
        ( IsMax -> InitialScore = -inf ; InitialScore = +inf ),
        InitialBestMove = InitialScore-(move(none, none, none)-none),
        
        % Process the ordered moves with pruning
        evaluate_moves(State, OrderedMoves, Alpha, Beta, Depth, Player, IsMax, InitialBestMove, BestMove, FinalAlpha, _),
        
        % Extract actual score from best move
        BestMove = ActualScore-(_-_),
        
        % Store result in transposition table
        ( is_valid_number(ActualScore) ->
            assertz(transposition(Hash, Depth, ActualScore))
        ; true
        )
    ; % Terminal case or depth zero
      ( game_over(State, Winner) ->
          evaluate_terminal(State, Player, Winner, Score),
          BestMove = Score-(move(none, none, none)-none)
      ; % Quiescence search at leaf nodes
        quiescence(State, Alpha, Beta, Score, Depth, Player),
        ( is_valid_number(Score) ->
            State = state(_, _, CurrentPlayer, _, _),
            collect_valid_moves(State, CurrentPlayer, Moves),
            ( Moves = [] ->
                BestMove = Score-(move(none, none, none)-none)
            ; select_random_move(Moves, Move, Action),
              BestMove = Score-(Move-Action)
            )
        ; % Fallback for invalid score
          BestMove = 0-(move(none, none, none)-none)
        )
      )
    ).

% Evaluate terminal positions (win/loss)
evaluate_terminal(_, Player, Winner, Score) :-
    ( Winner = Player -> Score = 1000
    ; Winner = 'No one' -> Score = 0
    ; Score = -1000
    ).
% Complete the evaluate_moves predicate
evaluate_moves(State, [Move-Action|Moves], Alpha, Beta, Depth, Player, IsMax, CurrentBest, BestMove, NewAlpha, NewBeta) :-
    copy_state(State, StateCopy),
    
    % Apply the move to get new state
    valid_move(StateCopy, TempState, Move),
    
    % Apply bridge action to get final state
    apply_bridge_action(TempState, Move, Action, FinalState),
    
    % Reduce depth and flip maximizing player
    Depth1 is Depth - 1,
    
    % Call minimax recursively with flipped IsMax
    ( IsMax -> NextIsMax = false ; NextIsMax = true ),
    minimax(FinalState, Depth1, Alpha, Beta, ChildBestMove, Player, NextIsMax),
    
    % Extract score from childs best move
    ChildBestMove = Score-(_-_),
    CurrentBest = CurrentScore-(_-_),
    
    % Determine if this move is better than current best
    ( (IsMax, Score > CurrentScore) ->
        ThisMoveBest = Score-(Move-Action)
    ; (IsMax, Score =< CurrentScore) ->
        ThisMoveBest = CurrentBest
    ; (\+ IsMax, Score < CurrentScore) ->
        ThisMoveBest = Score-(Move-Action)
    ; % \+ IsMax, Score >= CurrentScore
        ThisMoveBest = CurrentBest
    ),
    
    % Update alpha/beta based on whether this is maximizing or minimizing level
    ( IsMax ->
        % Maximizing player updates alpha
        NewAlpha1 is max(Alpha, Score),
        
        % Check for pruning
        ( NewAlpha1 >= Beta ->
            % Beta cutoff - remaining moves wont affect outcome
            BestMove = ThisMoveBest,
            NewAlpha = NewAlpha1,
            NewBeta = Beta
        ; % Continue with remaining moves
          evaluate_moves(State, Moves, NewAlpha1, Beta, Depth, Player, IsMax, 
                       ThisMoveBest, BestMove, NewAlpha, NewBeta)
        )
    ; % Minimizing player updates beta
        NewBeta1 is min(Beta, Score),
        
        % Check for pruning
        ( Alpha >= NewBeta1 ->
            % Alpha cutoff - remaining moves wont affect outcome
            BestMove = ThisMoveBest,
            NewAlpha = Alpha,
            NewBeta = NewBeta1
        ; % Continue with remaining moves
          evaluate_moves(State, Moves, Alpha, NewBeta1, Depth, Player, IsMax, 
                       ThisMoveBest, BestMove, NewAlpha, NewBeta)
        )
    ).

% Base case: no more moves to evaluate

% Revised evaluate_moves predicate
evaluate_moves(_, [], Alpha, Beta, _, _, true, CurrentBest, CurrentBest, Alpha, Beta).
evaluate_moves(_, [], Alpha, Beta, _, _, false, CurrentBest, CurrentBest, Alpha, Beta).

% Order moves for better pruning efficiency
order_moves(State, Player, Moves, OrderedMoves) :-
    maplist(score_move(State, Player), Moves, ScoredMoves),
    sort(2, @>=, ScoredMoves, SortedScoredMoves),
    maplist(remove_score, SortedScoredMoves, OrderedMoves).

% Assign preliminary scores to moves for ordering

score_move(State, Player, Move-Action, Move-Action-Score) :-
    % Score the goblin move
    ( Move = move(goblin(MovePlayer, _, _), _, _) ->
        ( MovePlayer = Player -> MoveScore = 10 ; MoveScore = 5 )
    ; MoveScore = 0
    ),
    % Score the bridge action
    ( Action = none -> ActionScore = 0
    ; Action = remove(X1, Y1, X2, Y2) ->
        % Higher score if removal isolates an opponent
        ( isolates_opponent(State, Player, X1, Y1, X2, Y2) -> ActionScore = 20
        ; ActionScore = 5
        )
    ; Action = rotate(_, _, _, _, _, _) ->
        % Lower score for rotate to balance with remove
        ActionScore = 2
    ),
    Score is MoveScore + ActionScore.

% Remove scoring information
remove_score(Move-Action-_, Move-Action).


isolates_opponent(State, Player, X1, Y1, X2, Y2) :-
    State = state(Goblins, Bridges, _, _, _),
    % Ensure the bridge exists
    ( member(bridge((X1,Y1)-(X2,Y2)), Bridges) ; 
      member(bridge((X2,Y2)-(X1,Y1)), Bridges) ),
    % Simulate removing the bridge
    canonical_bridge(X1, Y1, X2, Y2, CanX1, CanY1, CanX2, CanY2),
    select(bridge((CanX1,CanY1)-(CanX2,CanY2)), Bridges, NewBridges),
    % Check if an opponent’s goblin is isolated
    member(goblin(Opponent, X, Y), Goblins),
    Opponent \= Player,
    ( (X = X1, Y = Y1) ; (X = X2, Y = Y2) ),
    \+ (member(bridge((X,Y)-(_,_)), NewBridges) ; 
        member(bridge((_,_)-(X,Y)), NewBridges)).


% Select a random move from available moves
select_random_move(Moves, Move, Action) :-
    length(Moves, Length),
    Length > 0,
    random(0, Length, Index),
    nth0(Index, Moves, Move-Action).

% Quiescence search to handle horizon effect
quiescence(State, Alpha, Beta, Score, Depth, Player) :-
    ( debug_quiescence(true) ->
        format('Quiescence search at depth ~w~n', [Depth])
    ; true
    ),
    
    % Simple evaluation function for quiescence
    evaluate_board(State, Player, StaticScore),
    
    % Check if this is a significant move worth exploring deeper
    ( is_significant_state(State) ->
        % Look one level deeper for significant positions
        collect_quiescence_moves(State, QMoves),
        ( QMoves = [] ->
            Score = StaticScore
        ; quiescence_alpha_beta(State, QMoves, Alpha, Beta, StaticScore, Player, FinalScore),
          Score = FinalScore
        )
    ; % Not significant, return static evaluation
      Score = StaticScore
    ).

% Determine if a state is significant enough for quiescence search
is_significant_state(state(Goblins, Bridges, _, _, _)) :-
    % Consider a state significant if any player has fewer than 2 connections
    findall(Player-Count,
            (member(Player, [green, yellow, blue, red]),
             count_connected_goblins(Player, Goblins, Bridges, Count),
             Count < 2),
            LowConnections),
    LowConnections \= [].

% Count connected goblins for a player
count_connected_goblins(Player, Goblins, Bridges, Count) :-
    findall(goblin(Player, X, Y),
            (member(goblin(Player, X, Y), Goblins),
             (member(bridge((X,Y)-(_,_)), Bridges);
              member(bridge((_,_)-(X,Y)), Bridges))),
            ConnectedGoblins),
    length(ConnectedGoblins, Count).

% Collect only significant moves for quiescence search
collect_quiescence_moves(State, QMoves) :-
    State = state(_, _, CurrentPlayer, _, _),
    collect_valid_moves(State, CurrentPlayer, AllMoves),
    % Filter to keep only significant moves
    include(is_significant_move(State), AllMoves, QMoves).

% Determine if a move is significant for quiescence
is_significant_move(_, _-Action) :-
    % Any bridge manipulation is considered significant
    Action \= none.

% Alpha-beta for quiescence search
quiescence_alpha_beta(_, [], Alpha, Beta, CurrentBest, _, CurrentBest) :-
    Alpha < Beta.  % No pruning occurred

quiescence_alpha_beta(State, [Move-Action|Moves], Alpha, Beta, CurrentBest, Player, FinalBest) :-
    copy_state(State, StateCopy),
    valid_move(StateCopy, TempState, Move),
    apply_bridge_action(TempState, Move, Action, NewState),
    
    % Recursively evaluate the new state
    evaluate_board(NewState, Player, MoveScore),
    
    % Update CurrentBest
    NewCurrentBest is max(CurrentBest, MoveScore),
    
    % Update Alpha
    NewAlpha is max(Alpha, NewCurrentBest),
    
    % Check for pruning
    ( NewAlpha >= Beta ->
        % Pruning - return current best
        FinalBest = NewCurrentBest
    ; % Continue search
      quiescence_alpha_beta(State, Moves, NewAlpha, Beta, NewCurrentBest, Player, FinalBest)
    ).

% Hash a state for transposition table
hash_state(state(Goblins, Bridges, Player, Phase, _), Hash) :-
    term_hash(Goblins, GoblinHash),
    term_hash(Bridges, BridgeHash),
    term_hash(Player, PlayerHash),
    term_hash(Phase, PhaseHash),
    Hash is (GoblinHash * 31 + BridgeHash) * 31 + PlayerHash * 7 + PhaseHash.

% Evaluate a board position
evaluate_board(state(Goblins, Bridges, _, _, _), Player, Score) :-
    % Count connections for each player
    findall(P-Count,
            (member(P, [green, yellow, blue, red]),
             count_connected_goblins(P, Goblins, Bridges, Count)),
            Connections),
    
    % Get specific player connections
    member(Player-PlayerCount, Connections),
    
    % Calculate relative advantage
    findall(OpponentCount,
            (member(Opponent-OpponentCount, Connections),
             Opponent \= Player),
            OpponentCounts),
    sum_list(OpponentCounts, TotalOpponentCount),
    
    % Score based on connection advantage
    ( TotalOpponentCount > 0 ->
        AvgOpponentCount is TotalOpponentCount / 3,
        ConnectionAdvantage is PlayerCount - AvgOpponentCount
    ; ConnectionAdvantage is PlayerCount
    ),
    
    % Factor in goblins at risk of isolation
    count_goblins_at_risk(Player, Goblins, Bridges, AtRiskCount),
    RiskFactor is AtRiskCount * -2,
    
    % Factor in opponent goblins that can be isolated
    count_opponent_isolatable(Player, Goblins, Bridges, IsolatableCount),
    IsolationOpportunity is IsolatableCount * 5, % Increased weight
    
    % Penalize redundant rotations
    count_redundant_rotations(Goblins, Bridges, Player, RedundantCount),
    RedundantPenalty is RedundantCount * -3,
    
    % Combined score
    RawScore is ConnectionAdvantage * 10 + RiskFactor + IsolationOpportunity + RedundantPenalty,
    
    % Check for win/loss states
    ( player_eliminated(Player, Goblins, Bridges) ->
        Score = -500  % Heavy penalty for being eliminated
    ; % Count eliminated opponents
      findall(1, 
              (member(Opponent, [green, yellow, blue, red]), 
               Opponent \= Player,
               player_eliminated(Opponent, Goblins, Bridges)), 
              EliminatedList),
      length(EliminatedList, EliminatedCount),
      EliminationBonus is EliminatedCount * 200,
      Score is RawScore + EliminationBonus
    ).

count_redundant_rotations(Goblins, Bridges, Player, Count) :-
    findall(1,
            (member(goblin(Player, X, Y), Goblins),
             member(bridge((X,Y)-(X2,Y2)), Bridges),
             % Check if rotating this bridge creates a redundant position
             can_rotate(bridge((X,Y)-(X2,Y2)), _, _, bridge((NewX1,NewY1)-(NewX2,NewY2))),
             % Redundant if new bridge already exists or connects same goblin
             ( member(bridge((NewX1,NewY1)-(NewX2,NewY2)), Bridges)
             ; (member(goblin(Player, NewX1, NewY1), Goblins),
                member(goblin(Player, NewX2, NewY2), Goblins))
             )),
            RedundantList),
    length(RedundantList, Count).

% Count goblins at risk (with only one bridge connection)
count_goblins_at_risk(Player, Goblins, Bridges, Count) :-
    findall(1,
            (member(goblin(Player, X, Y), Goblins),
             % Find bridges connected to this goblin
             findall(bridge(_-_),
                     ((member(bridge((X,Y)-(_,_)), Bridges);
                      member(bridge((_,_)-(X,Y)), Bridges)),
                     ConnectedBridges),
             length(ConnectedBridges, 1)),  % Only one bridge connection
            AtRiskList),
    length(AtRiskList, Count).

% Count opponent goblins that can be isolated with one bridge removal
count_opponent_isolatable(Player, Goblins, Bridges, Count) :-
    findall(1,
            (member(goblin(Opponent, X, Y), Goblins),
             Opponent \= Player,
             % Find bridges connected to this goblin
             findall(bridge(_-_),
                     ((member(bridge((X,Y)-(_,_)), Bridges);
                      member(bridge((_,_)-(X,Y)), Bridges)),
                     ConnectedBridges),
             length(ConnectedBridges, 1)),  % Only one bridge connection
            IsolatableList),
    length(IsolatableList, Count).

% Collect all valid moves for a player
collect_valid_moves(State, Player, Moves) :-
    % First collect simple goblin moves
    findall(Move-none,
            (Move = move(goblin(Player, X1, Y1), (X2, Y2), _),
             valid_move(State, _, Move)),
            SimpleMoves),
    
    % If no moves possible, player can still do bridge actions
    ( SimpleMoves = [] ->
        BaseMoves = [move(none, none, _)-none]
    ; BaseMoves = SimpleMoves
    ),
    
    % Add possible bridge actions
    State = state(Goblins, Bridges, _, _, _),
    findall(Move-remove(X1, Y1, X2, Y2),
            (member(Move-_, BaseMoves),
             member(bridge((X1,Y1)-(X2,Y2)), Bridges)),
            RemoveMoves),
    
    findall(Move-rotate(X1, Y1, X2, Y2, PX, PY),
            (member(Move-_, BaseMoves),
             member(bridge((X1,Y1)-(X2,Y2)), Bridges),
             (PX = X1, PY = Y1 ; PX = X2, PY = Y2),  % Pivot on either end
             can_rotate(bridge((X1,Y1)-(X2,Y2)), PX, PY, NewBridge),
             % Ensure rotation creates a non-redundant bridge
             \+ member(NewBridge, Bridges),
             % Ensure rotation affects connectivity
             ( member(goblin(Player, X1, Y1), Goblins) ;
               member(goblin(Player, X2, Y2), Goblins) ;
               member(goblin(Opponent, X1, Y1), Goblins), Opponent \= Player ;
               member(goblin(Opponent, X2, Y2), Goblins), Opponent \= Player )),
            RotateMoves),
    
    % Combine all moves
    append([BaseMoves, RemoveMoves, RotateMoves], AllMoves),
    sort(AllMoves, Moves).

% Helper predicate for summing a list
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, TailSum),
    Sum is H + TailSum.

% --- Main Entry Point ---
:- initialization(play_pontuxl).