% This file is part of eep0018 released under the MIT license. 
% See the LICENSE file for more information.
-module(json).
-export([encode/1, decode/1, fuzz/0, fuzz/1]).
-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "json"), 0).

decode(_) ->
    not_loaded(?LINE).

encode(_) ->
    not_loaded(?LINE).

fuzz() ->
    json_fuzz:fuzz(fun json_fuzz:choose/4).
fuzz(Chooser) ->
    json_fuzz:fuzz(Chooser).

not_loaded(Line) ->
    exit({json_not_loaded, module, ?MODULE, line, Line}).
