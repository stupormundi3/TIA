:- module(pontuxl_server, [start_server/1]).
:- set_prolog_flag(encoding, utf8).
:- dynamic session/2.
:- discontiguous handle_chat/1.


% --- Imports HTTP / JSON / CORS ---
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).


% Votre jeu et votre chatbot
:- use_module(pontuxl_game).
:- use_module(pbot, [produire_reponse/2]).


% --- CORS helper
pontuxl_cors_enable(Request) :-
    cors_enable(Request,
      [ methods([get,post,options])
      , origins([*])
      , headers([authorization,content-type])
      ]).

% --- Déclaration des routes API avec CORS ---
:- http_handler('/api/chat',        handle_chat,       [methods([post,options]), cors(true)]).
:- http_handler('/api/new_game',    handle_new_game,   [methods([post,options]), cors(true)]).
:- http_handler('/api/game_state',  handle_game_state, [methods([get,options]),  cors(true)]).
:- http_handler('/api/valid_moves', handle_valid_moves,[methods([get,options]),  cors(true)]).
:- http_handler('/api/make_move',   handle_make_move,  [methods([post,options]), cors(true)]).
:- http_handler('/api/place_goblin',handle_place_goblin,[methods([post,options]), cors(true)]).
:- http_handler('/api/ai_move',     handle_ai_move,    [methods([get,options]),  cors(true)]).
:- http_handler('/api/game_over',   handle_game_over,  [methods([get,options]),  cors(true)]).
:- http_handler('/api/test',        handle_test,       [methods([get,options]),  cors(true)]).
:- http_handler('/api/debug_valid', handle_debug_valid,[methods([get,options]),  cors(true)]).

%% --- Chat – Bot explicateur ---
%% --- Handler /api/chat ---
handle_chat(Request) :-
    pontuxl_cors_enable(Request),
    catch(http_read_json_dict(Request, Params),
          _,
          ( reply_json_dict(_{status:error, message:"Invalid JSON payload"}, [status(400)]),
            fail)),
    (   get_dict(question, Params, QString)
    ->  true
    ;   reply_json_dict(_{status:error, message:"Missing 'question' field"}, [status(400)]),
        !
    ),
    % --- Appel du nouveau bot ------------------------------------
    pbot:produire_reponse(QString, RepList),
    atomic_list_concat(RepList, " ", Reply),
    reply_json_dict(_{reply:Reply}).

   
% --- Nouvelle partie ---
handle_new_game(Request) :-
    catch(
      http_read_json_dict(Request, JSONTerm),
      _,
      ( reply_json_dict(_{ status: error, message: "Invalid JSON payload" }, [status(400)]),
        fail
      )
    ),
    ( get_dict(mode, JSONTerm, ModeAtom),
      atom_string(ModeAtom, Mode0),
      member(Mode0, ["random","manual"])
    -> Mode = Mode0
    ;  Mode = "random"
    ),
    pontuxl_game:initial_state(state(Goblins, Bridges, Player, Phase, Mode)),
    uuid(SessionId),
    assertz(session(SessionId, state(Goblins, Bridges, Player, Phase, Mode))),
    findall(_{player:P, x:X, y:Y}, member(goblin(P,X,Y), Goblins), GoblinsJSON),
    findall(_{x1:X1, y1:Y1, x2:X2, y2:Y2}, member(bridge((X1,Y1)-(X2,Y2)), Bridges), BridgesJSON),
    reply_json_dict(_{
      session_id:     SessionId,
      goblins:        GoblinsJSON,
      bridges:        BridgesJSON,
      current_player: Player,
      phase:          Phase,
      mode:           Mode
    }).

% --- État de la partie ---
handle_game_state(Request) :-
    http_parameters(Request, [ session_id(SessionId, []) ]),
    ( session(SessionId, state(Goblins, Bridges, CurrentPlayer, Phase, Mode)) ->
        findall(_{player:P, x:X, y:Y}, member(goblin(P,X,Y), Goblins), GoblinsJSON),
        findall(_{x1:X1, y1:Y1, x2:X2, y2:Y2}, member(bridge((X1,Y1)-(X2,Y2)), Bridges), BridgesJSON),
        reply_json_dict(_{
          current_player: CurrentPlayer,
          phase:          Phase,
          mode:           Mode,
          goblins:        GoblinsJSON,
          bridges:        BridgesJSON
        })
    ; reply_json_dict(_{ status: error, message: "Session not found" }, [status(404)])
    ).

% --- Coups valides ---
% ------------------------------------------------------------------
%  Get valid moves
% ------------------------------------------------------------------
handle_valid_moves(Request) :-
    pontuxl_cors_enable(Request),
    http_parameters(Request, [ session_id(SessionId, []) ]),
    ( session(SessionId, state(Goblins, Bridges, CurrentPlayer, Phase, _Mode)) ->
        ( Phase == placement ->
            findall(_{ x:X, y:Y },
                    ( pontuxl_game:valid_square(X,Y),
                      \+ member(goblin(_,X,Y), Goblins)
                    ),
                    ValidPlacements),
            reply_json_dict(_{ phase: placement, valid_placements: ValidPlacements })
        ;
            pontuxl_game:collect_valid_moves(
              state(Goblins, Bridges, CurrentPlayer, Phase, _), 
              CurrentPlayer, Moves),
            findall(_{ move:MJ, action:AJ },
                    ( member(Move-Action, Moves),
                      move_to_json(Move, MJ),
                      action_to_json(Action, AJ)
                    ),
                    MovesJSON),
            reply_json_dict(_{ phase: play, valid_moves: MovesJSON })
        )
    ; reply_json_dict(_{ status: error, message: "Session not found" }, [status(404)])
    ).

% ------------------------------------------------------------------
%  Make a move (with optional removal or rotation)
% ------------------------------------------------------------------
%% --- Jouer un coup ---
handle_make_move(Request) :-
    pontuxl_cors_enable(Request),
    catch(
      http_read_json_dict(Request, Params),
      _,
      ( reply_json_dict(_{status:error, message:"Invalid JSON payload"}, [status(400)]), !, fail )
    ),

    % --- Session ID ---
    ( get_dict(session_id, Params, Raw)
    -> ( atom(Raw) -> SessionId = Raw ; atom_string(SessionId, Raw) )
    ;  reply_json_dict(_{status:error, message:"Missing session ID"}, [status(400)]), !, fail
    ),

    % --- Session must exist ---
    ( session(SessionId, State0)
    -> true
    ; reply_json_dict(_{status:error, message:"Session not found"}, [status(404)]), !, fail
    ),

    State0 = state(_,_,CurrentPlayer,_,_),

    % --- Coordonnées ---
    catch(
      convert_coordinates(Params, FromX, FromY, ToX, ToY),
      type_error(coord, _Bad),
      ( reply_json_dict(_{status:error, message:"Bad coordinate format"}, [status(400)]), !, fail )
    ),

    % --- Construire Move ---
    ( FromX == none ->
        Move = move(none, none, _)
    ; Move = move(goblin(CurrentPlayer, FromX, FromY), (ToX, ToY), _)
    ),

    % --- Construire Action ---
    catch(
      build_action(Params, Action),
      missing_params,
      ( reply_json_dict(_{status:error, message:"Invalid action parameters"}, [status(400)]), !, fail )
    ),

    % --- Appliquer ---
    ( pontuxl_game:apply_move_and_action(State0, Move, Action, State1)
    ->  retract(session(SessionId, State0)),
        assertz(session(SessionId, State1)),
        reply_json_dict(_{status:success, message:"Move successful"})
    ;   reply_json_dict(_{status:error, message:"Invalid move or action"}, [status(400)])
    ).

%% --- Sous-prédicat pour construire l’action ---
build_action(Params, Action) :-
    get_dict(action_type, Params, AType),
    ( AType == "none" ->
        Action = none
    ; AType == "remove" ->
        maplist({Params}/[Key,Val]>>get_dict(Key,Params,Val),
                [bridge_x1,bridge_y1,bridge_x2,bridge_y2],
                [X1,Y1,X2,Y2]),
        Action = remove(X1,Y1,X2,Y2)
    ; AType == "rotate" ->
        maplist({Params}/[Key,Val]>>get_dict(Key,Params,Val),
                [bridge_x1,bridge_y1,bridge_x2,bridge_y2,pivot_x,pivot_y],
                [X1,Y1,X2,Y2,PX,PY]),
        Action = rotate(X1,Y1,X2,Y2,PX,PY)
    ; throw(missing_params)
    ).% Helper to build Action from action_type/value map
build_action(none,   _,           none).
build_action(remove, Params, remove(X1,Y1,X2,Y2)) :-
    maplist({Params}/[K,V]>>get_dict(K,Params,V),
            [bridge_x1-X1, bridge_y1-Y1, bridge_x2-X2, bridge_y2-Y2]).
build_action(rotate, Params, rotate(X1,Y1,X2,Y2,PX,PY)) :-
    maplist({Params}/[K,V]>>get_dict(K,Params,V),
            [bridge_x1-X1, bridge_y1-Y1, bridge_x2-X2, bridge_y2-Y2,
             pivot_x-PX,    pivot_y-PY]).
build_action(_,_,_) :-
    fail.

% Reusing convert_coordinates/5 and move_to_json/2, action_to_json/2 as before
% --- Placer un gobelin (phase placement) ---
handle_place_goblin(Request) :-
    http_read_json_dict(Request, Params),
    get_dict(session_id, Params, SessionId),
    get_dict(x, Params, X),
    get_dict(y, Params, Y),
    ( session(SessionId, state(Goblins,Br,Player,Phase,Mode)) ->
        ( Phase == placement,
          pontuxl_game:valid_square(X,Y),
          \+ member(goblin(_,X,Y), Goblins)
        -> NewGoblins = [goblin(Player,X,Y)|Goblins],
           pontuxl_game:next_player(Player,Next),
           ( length(NewGoblins,16) -> NewPhase = play ; NewPhase = placement ),
           NewState = state(NewGoblins,Br,Next,NewPhase,Mode),
           retract(session(SessionId,_)),
           assertz(session(SessionId,NewState)),
           reply_json_dict(_{ status: success, message: "Goblin placed" })
        ;  reply_json_dict(_{ status: error,   message: "Invalid placement" }, [status(400)])
        )
    ; reply_json_dict(_{ status: error, message: "Session not found" }, [status(404)]) ).

% --- Coup IA ---
handle_ai_move(Request) :-
    http_parameters(Request, [ session_id(SessionId, []) ]),
    ( session(SessionId, state(G,Br,P,Ph,M)) ->
        ( pontuxl_game:ai_select_move(state(G,Br,P,Ph,M), Move, Action) ->
            move_to_json(Move, MJ), action_to_json(Action, AJ),
            reply_json_dict(_{ status: success, move: MJ, action: AJ })
        ; reply_json_dict(_{ status: error, message: "AI failed" }, [status(500)]) )
    ; reply_json_dict(_{ status: error, message: "Session not found" }, [status(404)]) ).

% --- Fin de partie ---
handle_game_over(Request) :-
    http_parameters(Request, [ session_id(SessionId, []) ]),
    ( session(SessionId, state(_,_,_,_,_)) ->
        ( pontuxl_game:game_over(_,Winner) ->
            reply_json_dict(_{ game_over: true, winner: Winner })
        ; reply_json_dict(_{ game_over: false }) )
    ; reply_json_dict(_{ status: error, message: "Session not found" }, [status(404)]) ).

% --- Helpers JSON pour moves/actions ---
move_to_json(move(goblin(P,FX,FY),(TX,TY),_), MJ) :-
    MJ = _{ type: goblin, player:P, from_x:FX, from_y:FY, to_x:TX, to_y:TY }.
move_to_json(move(none,none,_), MJ) :-
    MJ = _{ type: none }.

action_to_json(none, AJ) :-
    AJ = _{ type: none }.
action_to_json(remove(X1,Y1,X2,Y2), AJ) :-
    AJ = _{ type: remove, x1:X1, y1:Y1, x2:X2, y2:Y2 }.
action_to_json(rotate(X1,Y1,X2,Y2,PX,PY), AJ) :-
    AJ = _{ type: rotate, x1:X1, y1:Y1, x2:X2, y2:Y2, pivot_x:PX, pivot_y:PY }.


uuid(UUID) :-
    random_between(100000,999999,Num),
    atom_concat('session-',Num,UUID).

%% --- Helper pour parser from_x, from_y, to_x, to_y ---
convert_coordinates(Params, FX, FY, TX, TY) :-
    % Récupère brut
    get_dict(from_x, Params, FX0),
    get_dict(from_y, Params, FY0),
    get_dict(to_x,   Params, TX0),
    get_dict(to_y,   Params, TY0),

    % FX
    (   FX0 == none
    ->  FX = none
    ;   number(FX0)
    ->  FX = FX0
    ;   atom(FX0)
    ->  atom_number(FX0, FX)
    ;   type_error(coord, FX0)
    ),
    % FY
    (   FY0 == none
    ->  FY = none
    ;   number(FY0)
    ->  FY = FY0
    ;   atom(FY0)
    ->  atom_number(FY0, FY)
    ;   type_error(coord, FY0)
    ),

    % TX
    (   number(TX0)
    ->  TX = TX0
    ;   atom(TX0)
    ->  atom_number(TX0, TX)
    ;   type_error(coord, TX0)
    ),
    % TY
    (   number(TY0)
    ->  TY = TY0
    ;   atom(TY0)
    ->  atom_number(TY0, TY)
    ;   type_error(coord, TY0)
    ).


%% --- Démarrage du serveur HTTP ---
start_server(Port) :-
    http_server(http_dispatch, [port(Port), workers(4)]),
    format('Serveur PontuXL lancé sur le port ~w~n', [Port]).

