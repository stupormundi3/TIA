:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- http_handler('/ping', handle_ping, [method(get)]).

handle_ping(_Request) :-
    reply_json_dict(_{pong: true}).

start :-
    http_server(http_dispatch, [port(8085), workers(2)]).
