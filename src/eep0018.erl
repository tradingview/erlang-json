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
    case (catch build_term()) of
    {json_error, Reason} ->
        io:format("JSON ERROR: ~p~n", [Reason]),
        throw({json_error, Reason});
    Else ->
        Else
    end.

build_term() ->
    receive
    {error, Error} ->
        throw({json_error, Error});
    {value, Resp} ->
        Resp;
    map_open ->
        build_term([{in_map, []}]);
    list_open ->
        build_term([{in_list, []}])
    end.
build_term([Curr | Rest]) ->
    receive
    {error, Error} ->
        throw({json_error, Error});
    {value, Value} ->
        case Curr of
        {in_key, Key} ->
            [{in_map, Props} | RestTerm] = Rest,
            build_term([{in_map, [{Key, Value} | Props]} | RestTerm]);
        {in_list, RestValues} ->
            build_term([{in_list, [Value | RestValues]} | Rest])
        end;
    map_open ->
        build_term([{in_map, []}, Curr | Rest]);
    map_close ->
        {in_map, Props} = Curr,
        case Rest of
        [] ->
            {lists:reverse(Props)};
        [{in_key, Key}, {in_map, RestProps} | RestTerm] ->
            build_term([{in_map, [{Key, {lists:reverse(Props)}} | RestProps]} | RestTerm]);
        [{in_list, Values} | RestTerm] ->
            build_term([{in_list, [{Props} | Values]} | RestTerm])
        end;
    {map_key, Key} ->
        build_term([{in_key, Key}, Curr | Rest]);
    list_open ->
        build_term([{in_list, []}, Curr | Rest]);
    list_close ->
        {in_list, Values} = Curr,
        case Rest of
        [] ->
            lists:reverse(Values);
        [{in_key, Key}, {in_map, RestProps} | RestTerm] ->
            build_term([{in_map, [{Key, lists:reverse(Values)} | RestProps]} | RestTerm]);
        [{in_list, RestValues} | RestTerm] ->
            build_term([{in_list, [lists:reverse(Values) | RestValues]} | RestTerm])
        end
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
