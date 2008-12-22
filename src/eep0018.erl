-module(eep0018).

-export([start_driver/1]).
-export([json_to_term/1, term_to_json/1]).

% Public API

json_to_term(Json) when is_list(Json) ->
    json_to_term(list_to_binary(Json));
json_to_term(Json) when is_binary(Json) ->
    %io:format("IN: ~p~n", [Json]),
    Size = size(Json)+1,
    Bin = <<Size:32/native, Json/binary, <<0>>/binary>>,
    Result = erlang:port_control(drv_port(), 0, Bin),
    %io:format("RECEIVED: ~p~n", [Result]),
    binary_to_term(list_to_binary(Result)).

term_to_json(Term) ->
    %io:format("TERM: ~p~n", [Term]),
    Result = erlang:port_control(drv_port(), 1, term_to_binary(Term)),
    Result.

% Implementation

start_driver(LibDir) ->
    case erl_ddll:load_driver(LibDir, "eep0018_drv") of
    ok ->
        ok;
    {error, alread_loaded} ->
        ok = erl_ddll:reload_driver(LibDir, "eep0018_drv");
    {error, Error} ->
        exit(erl_ddll:format_error(Error))
    end.

drv_port() ->
    case get(eep0018_drv) of
    undefined ->
        Port = open_port({spawn, "eep0018_drv"}, []),
        put(eep0018_drv, Port),
        Port;
    Port ->
        Port
    end.

