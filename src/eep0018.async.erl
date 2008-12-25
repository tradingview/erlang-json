-module(eep0018).

-export([start_driver/1]).
-export([json_to_term/1, term_to_json/1]).

% Public API

json_to_term(Json) when is_list(Json) ->
    json_to_term(list_to_binary(Json));
json_to_term(Json) when is_binary(Json) ->
    Port = drv_port(),
    erlang:port_control(Port, 1, <<Json/binary, 0:8>>),
    receive
    {Port, {data, Bin}} ->
        binary_to_term(list_to_binary(Bin))
    end.

term_to_json(Term) ->
    Bin = term_to_binary(Term),
    Port = drv_port(),
    erlang:port_control(Port, 2, Bin),
    receive
    {Port, {data, Json}} ->
        Json
    end.

% Implementation

start_driver(LibDir) ->
    case erl_ddll:load_driver(LibDir, "eep0018_drv") of
    ok ->
        ok;
    {error, already_loaded} ->
        ok;
        %ok = erl_ddll:reload_driver(LibDir, "eep0018_drv");
    {error, Error} ->
        exit(erl_ddll:format_error(Error))
    end.

drv_port() ->
    case get(eep0018_drv_port) of
    undefined ->
        Port = open_port({spawn, "eep0018_drv"}, []),
        put(eep0018_drv_port, Port),
        Port;
    Port ->
        Port
    end.
