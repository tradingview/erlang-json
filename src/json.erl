% This file is part of eep0018 released under the MIT license. 
% See the LICENSE file for more information.
-module(json).
-export([encode/1, decode/1, decode/2]).
-export_type([decode_options/0]).
-export_type([encode_options/0]).
-export_type([value/0]).
-export_type([object/2]).
-export_type([key/0]).
-export_type([array/1]).
-export_type([json_string/0]).
-export_type([json_number/0]).
-export_type([text/0]).

-on_load(init/0).

-type value() :: json_string()
               | json_number()
               | object(key(), value())
               | array(value())
               | boolean()
               | null
               .
-type object(K,T)   :: {[ {K, T} ]}.
-type key()         :: json_string().
-type array(T)      :: [T].
-type json_string() :: unicode:unicode_binary().
-type json_number() :: integer() | float().
-type text()        :: iodata().

-type decode_options() :: [ decode_option() ].
-type decode_option()  ::
        allow_comments
      | {allow_comments, boolean()}
      .

-type encode_options() :: [].

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

-spec decode(text()) -> {ok, value()} | {error, term()}.
decode(JsonText) ->
    decode(JsonText, []).

-spec decode(text(), decode_options()) -> {ok, value()} | {error, term()}.
decode(JsonText, Options) ->
    case decode_nif(JsonText, Options) of
        {ok, Value} ->
            {ok, Value};
        {error, _Reason} = Err ->
            Err
    end.

-spec decode_nif(text(), decode_options()) -> {ok, value()} | {error, term()}.
decode_nif(JsonText, Options) ->
    erlang:nif_error(module_not_loaded, [JsonText, Options]).

-spec encode(value()) -> {ok, text()} | {error, term()}.
encode(JsonTerm) ->
    case encode_nif(JsonTerm) of
        {ok, Value} ->
            {ok, Value};
        {error, _Reason} = Err ->
            Err
    end.

-spec encode_nif(value()) -> {ok, text()} | {error, term()}.
encode_nif(JsonTerm) ->
    erlang:nif_error(module_not_loaded, [JsonTerm]).

