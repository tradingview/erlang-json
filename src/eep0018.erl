-module(eep0018).

-export([start_driver/1]).
-export([json_to_term/1, term_to_json/1]).

% Public API

term_to_json(Term) ->
    Bin = term_to_binary(Term),
    erlang:port_control(drv_port(), 0, Bin).

json_to_term(Json) when is_list(Json) ->
    json_to_term(list_to_binary(Json));
json_to_term(Json) when is_binary(Json) ->
    % The null byte is important for bare literals. Without it
    % yajl will throw a fit because it doesn't think it's finished
    % parsing correctly.
    [] = erlang:port_control(drv_port(), 1, <<Json/binary, 0:8>>),
    receive
    Term ->
        Term
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
        Port = open_port({spawn, "eep0018_drv"}, [binary]),
        put(eep0018_drv_port, Port),
        Port;
    Port ->
        Port
    end.
