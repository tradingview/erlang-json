% vim: encoding=utf-8 fileencoding=utf-8

-module(runner).
-export([main/0]).

main() ->
    eep0018:start_driver("."),
    run("eep0018", fun eep0018:term_to_json/1, fun eep0018:json_to_term/1, 25, 1000),
    run("mochijson2", fun mochijson2:encode/1, fun mochijson2:decode/1, 25, 1000),
    init:stop().

run(Name, Encode, Decode, Procs, Repeat) ->
    Start = micro(),
    spawn_procs(Encode, Decode, Procs, Repeat),
    wait_for(Procs),
    End = micro(),
    io:format("~p: ~p~n", [Name, (End-Start)/1000000]).

spawn_procs(_, _, 0, _) ->
    ok;
spawn_procs(Encode, Decode, Procs, Repeat) ->
    Notify = self(),
    spawn(fun() ->
        repeat(Encode, Decode, Repeat),
        Notify ! done
    end),
    spawn_procs(Encode, Decode, Procs-1, Repeat).

wait_for(0) ->
    ok;
wait_for(N) ->
    receive
    done ->
        wait_for(N-1)
    end.

repeat(_, _, 0) ->
    ok;
repeat(Encode, Decode, N) ->
    test_all(Encode, Decode),
    repeat(Encode, Decode, N-1).

micro() ->
    {Mega, Secs, Micro} = erlang:now(),
    (Mega * 1000000 + Secs) * 1000000 + Micro.

obj_new() ->
    {[]}.

is_obj({Props}) ->
    F = fun ({K, _}) when is_binary(K) ->
                true;
            (_) ->
                false
        end,    
    lists:all(F, Props).

obj_from_list(Props) ->
    Obj = {Props},
    case is_obj(Obj) of
        true -> Obj;
        false -> exit({json_bad_object, Obj})
    end.

%% Test for equivalence of Erlang terms.
%% Due to arbitrary order of construction, equivalent objects might
%% compare unequal as erlang terms, so we need to carefully recurse
%% through aggregates (tuples and objects).

equiv({Props1}, {Props2}) ->
    equiv_object(Props1, Props2);
equiv(L1, L2) when is_list(L1), is_list(L2) ->
    equiv_list(L1, L2);
equiv(N1, N2) when is_number(N1), is_number(N2) -> N1 == N2;
equiv(B1, B2) when is_binary(B1), is_binary(B2) -> B1 == B2;
equiv(true, true) -> true;
equiv(false, false) -> true;
equiv(null, null) -> true.

%% Object representation and traversal order is unknown.
%% Use the sledgehammer and sort property lists.

equiv_object(Props1, Props2) ->
    L1 = lists:keysort(1, Props1),
    L2 = lists:keysort(1, Props2),
    Pairs = lists:zip(L1, L2),
    true = lists:all(fun({{K1, V1}, {K2, V2}}) ->
                             equiv(K1, K2) and equiv(V1, V2)
                     end, Pairs).

%% Recursively compare tuple elements for equivalence.

equiv_list([], []) ->
    true;
equiv_list([V1 | L1], [V2 | L2]) ->
    case equiv(V1, V2) of
        true ->
            equiv_list(L1, L2);
        false ->
            false
    end.

test_all(Encode, Decode) ->
    test_one(Encode, Decode, e2j_test_vec(utf8), 1).

test_one(_, _, [], _N) ->
    %io:format("~p tests passed~n", [N-1]),
    ok;
test_one(Encode, Decode, [{E, J} | Rest], N) ->
    %io:format("[~p] ~p ~p~n", [N, E, J]),
    DJ = Decode(J),
    ok = case equiv(E, DJ) of
    false ->
        io:format("~p != ~p~n", [E, DJ]),
        error;
    true ->
        ok
    end,
    
    DEJ = Decode(Encode(E)),
    ok = case equiv(E, DEJ) of
    false ->
        io:format("~p != ~p~n", [E, DEJ]),
        error;
    true ->
        ok
    end,
    
    test_one(Encode, Decode, Rest, 1+N).

e2j_test_vec(utf8) ->
    [
     {[1199344435545.0, 1], "[1199344435545.0,1]"},
     {1, "1"},
     {3.1416, "3.14160"}, %% text representation may truncate, trail zeroes
     {-1, "-1"},
     {-3.1416, "-3.14160"},
     {12.0e10, "1.20000e+11"},
     {1.234E+10, "1.23400e+10"},
     {-1.234E-10, "-1.23400e-10"},
     {10.0, "1.0e+01"},
     {123.456, "1.23456E+2"},
     {10.0, "1e1"},
     {<<"foo">>, "\"foo\""},
     {<<"foo", 5, "bar">>, "\"foo\\u0005bar\""},
     {<<"">>, "\"\""},
     {<<"\n\n\n">>, "\"\\n\\n\\n\""},
     {<<"\" \b\f\r\n\t\"">>, "\"\\\" \\b\\f\\r\\n\\t\\\"\""},
     {obj_new(), "{}"},
     {obj_from_list([{<<"foo">>, <<"bar">>}]), "{\"foo\":\"bar\"}"},
     {obj_from_list([{<<"foo">>, <<"bar">>}, {<<"baz">>, 123}]),
      "{\"foo\":\"bar\",\"baz\":123}"},
     {[], "[]"},
     {[[]], "[[]]"},
     {[1, <<"foo">>], "[1,\"foo\"]"},
     
     %% json array in a json object
     {obj_from_list([{<<"foo">>, [123]}]),
      "{\"foo\":[123]}"},
     
     %% json object in a json object
     {obj_from_list([{<<"foo">>, obj_from_list([{<<"bar">>, true}])}]),
      "{\"foo\":{\"bar\":true}}"},
     
     %% fold evaluation order
     {obj_from_list([{<<"foo">>, []},
                     {<<"bar">>, obj_from_list([{<<"baz">>, true}])},
                     {<<"alice">>, <<"bob">>}]),
      "{\"foo\":[],\"bar\":{\"baz\":true},\"alice\":\"bob\"}"},
     
     %% json object in a json array
     {[-123, <<"foo">>, obj_from_list([{<<"bar">>, []}]), null],
      "[-123,\"foo\",{\"bar\":[]},null]"},

     %% Unicode characters
     {[<<"¡">>, <<"ü">>], "[\"\\u00A1\",\"\\u00FC\"]"}
   ].
