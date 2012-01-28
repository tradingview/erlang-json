% This file is part of eep0018 released under the MIT license. 
% See the LICENSE file for more information.
-module(json).
-export([encode/1, decode/1, decode/2]).
-export_type([decode_options/0]).
-on_load(init/0).

-type decode_options() :: [ decode_option() ].
-type decode_option() :: allow_comments | {allow_comments, boolean()}.

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

decode(JsonText) ->
    erlang:nif_error(module_not_loaded, [JsonText]).

decode(JsonText, Options) ->
    erlang:nif_error(module_not_loaded, [JsonText, Options]).

encode(JsonTerm) ->
    erlang:nif_error(module_not_loaded, [JsonTerm]).

