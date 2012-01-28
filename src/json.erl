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
      | {key_decode, existent_atom | binary | [existent_atom | binary | ignore]}
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
    case decode_nif(JsonText, Options, []) of
        {ok, Value} ->
            {ok, Value};
        {badvals, BadValues} ->
            decode_retry(JsonText, Options, BadValues);
        {error, _Reason} = Err ->
            Err
    end.

-type predecoded_values() :: [{binary(), term()}].
-type position() :: non_neg_integer().
-spec decode_nif(text(), decode_options(), predecoded_values())
    -> {ok, value()} | {error, term()} | {badvals, [{bigval, text(), position()}]}.
decode_nif(JsonText, Options, PreDecodedValues) ->
    erlang:nif_error(module_not_loaded, [JsonText, Options, PreDecodedValues]).

decode_retry(JsonTerm, Options, Values) ->
    case pre_decode(Values) of
        {ok, PreDecodedValues} ->
            decode_retry_2(JsonTerm, Options, PreDecodedValues);
        {error, _Reason} = Err ->
            Err
    end.

decode_retry_2(JsonTerm, Options, PreDecodedValues) ->
    case decode_nif(JsonTerm, Options, PreDecodedValues) of
        {ok, Value} ->
            {ok, Value};
        {error, _Reason} = Err ->
            Err
    end.

pre_decode(Values) ->
    pre_decode(Values, []).
pre_decode([{bigval, Text, Pos} | Rest], Result) ->
    Text_2 = binary_to_list(Text),
    try
        list_to_integer(Text_2)
    of
        Value ->
            Result_2 = [{Text, Value}| Result],
            pre_decode(Rest, Result_2)
    catch
        error:badarg ->
            try
                list_to_float(Text_2)
            of
                Value ->
                    Result_2 = [{Text, Value}| Result],
                    pre_decode(Rest, Result_2)
            catch
                error:badarg ->
                    case re:run(Text, "^(\\d+)(e[-+]?\\d+)$",[dollar_endonly, {capture, all_but_first, list}]) of
                        {match, [Int, Exp]} ->
                            try
                                list_to_float(Int++".0"++Exp)
                            of
                                Value ->
                                    Result_2 = [{Text, Value}| Result],
                                    pre_decode(Rest, Result_2)
                            catch
                                error:badarg ->
                                   {error, {Pos, numeric_overflow, Text}}
                            end;
                        nomatch ->
                            {error, {Pos, numeric_overflow, Text}}
                    end
            end
    end;
pre_decode([], Result) ->
    {ok, lists:reverse(Result)}.


-spec encode(value()) -> {ok, text()} | {error, term()}.
encode(JsonTerm) ->
    case encode_nif(JsonTerm, []) of
        {ok, Value} ->
            {ok, Value};
        {badvals, BadValues} ->
            encode_retry(JsonTerm, BadValues);
        {error, _Reason} = Err ->
            Err
    end.

-type preencoded_values() :: [{term(), binary()}].
-spec encode_nif(value(), preencoded_values())
    -> {ok, text()} | {error, term()} | {badvals, [value()]}.
encode_nif(JsonTerm, PreEncodedValues) ->
    erlang:nif_error(module_not_loaded, [JsonTerm, PreEncodedValues]).

encode_retry(JsonTerm, Values) ->
    case pre_encode(Values) of
        {ok, PreEncodedValues} ->
            encode_retry_2(JsonTerm, PreEncodedValues);
        {error, _Reason} = Err ->
            Err
    end.

encode_retry_2(JsonTerm, PreEncodedValues) ->
    case encode_nif(JsonTerm, PreEncodedValues) of
        {ok, Value} ->
            {ok, Value};
        {error, _Reason} = Err ->
            Err
    end.

pre_encode(Values) ->
    pre_encode(Values, []).
pre_encode([Value | Rest], Result) when is_integer(Value) ->
    Text = list_to_binary(integer_to_list(Value)),
    Result_2 = [{Value, Text}| Result],
    pre_encode(Rest, Result_2);
pre_encode([Value | _Rest], _Result) ->
    {error, {badarg, Value}};
pre_encode([], Result) ->
    {ok, lists:reverse(Result)}.
