#! /usr/bin/env escript

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("test"),
    
    etap:plan(6),
    etap:is(json:decode(<<"true">>), {ok, true}, "DEC: true -> true"),
    etap:is(json:encode(true), {ok, <<"true">>}, "ENC: true -> true"),
    
    etap:is(json:decode(<<"false">>), {ok, false}, "DEC: false -> false"),
    etap:is(json:encode(false), {ok, <<"false">>}, "ENC: false -> false"),
    
    etap:is(json:decode(<<"null">>), {ok, null}, "DEC: null -> null"),
    etap:is(json:encode(null), {ok, <<"null">>}, "ENC: null -> null"),

    etap:end_tests().


