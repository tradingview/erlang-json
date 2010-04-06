-module(json).
-export([encode/1, decode/1]).
-on_load(init/0).

init() ->
    SoName = case code:priv_dir(json) of
        {error, bad_name} ->
            case filelib:is_file(filename:join(["..", priv, json])) of
                true ->
                    filename:join(["..", priv, json]);
                _ ->
                    filename:join([priv, json])
            end;
        Dir ->
            filename:join(Dir, json)
    end,
    erlang:load_nif(SoName, 0).

decode(_) ->
    not_loaded(?LINE).

encode(_) ->
    not_loaded(?LINE).

not_loaded(Line) ->
    exit({json_not_loaded, module, ?MODULE, line, Line}).
