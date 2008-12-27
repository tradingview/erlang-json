-module(runner).
-export([main/0]).

-define(PROCS, 5).

main() ->
    eep0018:start_driver("."),
    Cases = read_cases(),
    run("eep0018", Cases, fun eep0018:term_to_json/1, fun eep0018:json_to_term/1),
    run("mochijson2", Cases, fun mochijson2:encode/1, fun mochijson2:decode/1),
    run("rabbitmq", Cases, fun rabbitmq:encode/1, fun rabbitmq:decode/1),
    init:stop().

run(Name, Cases, Encode, Decode) ->
    timed(single_proc, Name, fun() -> single(Cases, Encode, Decode) end),
    timed(multi_proc, Name, fun() -> multiple(Cases, Encode, Decode) end).

timed(Prefix, Name, Func) ->
    Start = micro(),
    Func(),
    End = micro(),
    display(Prefix, Name, Start, End).

single(Cases, Encode, Decode) ->
    lists:foreach(fun({Json, Term}) ->
        case Term of
        nil ->
            Decode(Json);
        _ ->
            true = compare:equiv(Term, Decode(Json)),
            true = compare:equiv(Term, Decode(Encode(Term)))
        end
    end, Cases).

multiple(Cases, Encode, Decode) ->
    RunFun = fun() -> single(Cases, Encode, Decode) end,
    spawn_procs(?PROCS, RunFun),
    wait_for_procs(?PROCS).

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

display(Prefix, Name, Start, End) ->
    io:format("~11s => ~10s: ~6.3f s~n", [Prefix, Name, (End-Start)/1000000]).

read_cases() ->
    {ok, FileNames} = file:list_dir("./cases"),
    JsonFiles = [Fname || Fname <- FileNames, lists:suffix(".json", Fname)],
    [
        begin
            {ok, Json} = file:read_file("./cases/" ++ Case),
            {Pred, _} = lists:splitwith(fun(C) -> C == "." end, Case),
            ErlFname = "./cases/" ++ Pred ++ ".erl",
            Gold =
            case file:read_file_info(ErlFname) of
            {ok, _} ->
                {ok, [Term]} = file:consult(ErlFname),
                Term;
            {error, _} ->
                nil
            end,
            {Json, Gold}
        end
        || Case <- JsonFiles
    ].
