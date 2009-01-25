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
    Result = Func(),
    End = micro(),
    display(Prefix, Name, Start, End, Result).

single(Cases, Encode, Decode) ->
    lists:foldl(fun({_Case, Json, Term}, {AccDec, AccRound}) ->
        case Term of
        nil ->
            Decode(Json),
            {0, 0};
        _ ->
            AccDec2 = case (catch compare:equiv(Term, Decode(Json))) of
            true ->
                AccDec;
            _ ->
                AccDec+1
            end,
            AccRound2 = case (catch compare:equiv(Term, Decode(Encode(Term)))) of
            true ->
                AccRound;
            _ ->
                AccRound+1
            end,
            {AccDec2, AccRound2}
        end
    end, {0, 0}, Cases).

multiple(Cases, Encode, Decode) ->
    RunFun = fun() -> single(Cases, Encode, Decode) end,
    spawn_procs(?PROCS, RunFun),
    wait_for_procs(?PROCS, {0, 0}).

spawn_procs(0, _) ->
    ok;
spawn_procs(N, Func) ->
    S = self(),
    spawn(fun() -> S ! {finished, Func()} end),
    spawn_procs(N-1, Func).

wait_for_procs(0, Acc) ->
    Acc;
wait_for_procs(N, {Dec, Round}) ->
    receive {finished, {Dec2, Round2}} -> ok end,
    wait_for_procs(N-1, {Dec+Dec2, Round+Round2}).

micro() ->
    {Mega, Secs, Micro} = erlang:now(),
    (Mega * 1000000 + Secs) * 1000000 + Micro.

display(Prefix, Name, Start, End, Result) ->
    case Result of
    {0, 0} ->
        io:format("~11s => ~10s: ~6.3f s [success]~n", [Prefix, Name, (End-Start)/1000000]);
    {Dec, Round} ->
        io:format(
            "~11s => ~10s: ~6.3f s [failure]~n\t~p decoding errors.~n\t~p round trip errors.~n",
            [Prefix, Name, (End-Start)/1000000, Dec, Round]
        )
    end.

read_cases() ->
    {ok, FileNames} = file:list_dir("./cases"),
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
