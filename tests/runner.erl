% Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
% Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
%
% This file is part of EEP0018, which is released under the MIT
% license.

-module(runner).
-export([main/0]).

-define(PROCS, 5).

main() ->
    eep0018:start("."),
    Cases = read_cases(),

    io:format("eep0018 =>~n", []),
    run(Cases, fun eep0018:encode_json/1, fun eep0018:decode_json/1),

    %lists:foreach(fun(Num) ->
    %    Encode = fun eep0018:encode_json/1,
    %    Decode = fun eep0018:decode_json/1,
    %    Time = timed(fun() -> multiple(Num, Cases, Encode, Decode) end),
    %    io:format("~p, ~p~n", [Num, Time])
    %end, lists:seq(1, 25)),

    %io:format("mochijson =>~n", []),
    %run(Cases, fun mochijson2:encode/1, fun mochijson2:decode/1),

    %io:format("rabbitmq =>~n", []),
    %run(Cases, fun rabbitmq:encode/1, fun rabbitmq:decode/1),

    init:stop().

run(Cases, Encode, Decode) ->
    T1 = timed(fun() -> single(Cases, Encode, Decode) end),
    io:format("\tsingle -> ~6.3f~n", [T1]),
    T2 = timed(fun() -> multiple(?PROCS, Cases, Encode, Decode) end),
    io:format("\tmulti  -> ~6.3f~n", [T2]).

timed(Func) ->
    Start = micro(),
    Func(),
    End = micro(),
    (End-Start)/1000000.

single(Cases, Encode, Decode) ->
    lists:foldl(fun({Case, Json, Term}, nil) ->
        try
            Decode(Json),
            case Term of
                nil -> ok;
                _ ->
                    compare:equiv(Term, Decode(Json)),
                    compare:equiv(Term, Decode(Encode(Term)))
            end
        catch
            _ ->
                io:format("\tFailed case: ~p~n", [Case])
        end,
        nil
    end, nil, Cases).

multiple(Procs, Cases, Encode, Decode) ->
    RunFun = fun() -> single(Cases, Encode, Decode) end,
    spawn_procs(Procs, RunFun),
    ok = wait_for_procs(Procs).

spawn_procs(0, _) ->
    ok;
spawn_procs(N, Func) ->
    S = self(),
    spawn(fun() -> Func(), S ! finished end),
    spawn_procs(N-1, Func).

wait_for_procs(0) ->
    ok;
wait_for_procs(N) ->
    receive finished -> ok end,
    wait_for_procs(N-1).

micro() ->
    {Mega, Secs, Micro} = erlang:now(),
    (Mega * 1000000 + Secs) * 1000000 + Micro.

read_cases() ->
    {ok, Unsorted} = file:list_dir("./cases"),
    FileNames = lists:sort(Unsorted),
    JsonFiles = [Fname || Fname <- FileNames, lists:suffix(".json", Fname)],
    [
        begin
            {ok, Json} = file:read_file("./cases/" ++ Case),
            {Pred, _} = lists:splitwith(fun(C) -> C /= $. end, Case),
            ErlFname = "./cases/" ++ Pred ++ ".erl",
            Gold =
            case file:read_file_info(ErlFname) of
            {ok, _} ->
                {ok, [Term]} = file:consult(ErlFname),
                Term;
            {error, _} ->
                nil
            end,
            {Case, Json, Gold}
        end
        || Case <- JsonFiles
    ].
