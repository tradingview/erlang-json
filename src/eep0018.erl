% Copyright (c) 2009 Zhou Li <echou327@gmail.com>
% Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
% Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
%
% This file is part of EEP0018, which is released under the MIT license.

-module(eep0018).

-export([start/0, start_link/0, start/1, start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([encode_json/1, decode_json/1]).

-define(DEFAULT_LIB_PATH, "priv/lib").

-define(DRIVER_TABLE, eep0018_table).
-define(DRIVER_TAG, eep0018_port).
-define(DRIVER_NAME, eep0018_drv).

start(LibPath) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [LibPath], []).

start() ->
    start(?DEFAULT_LIB_PATH).

start_link(LibPath) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [LibPath], []).

start_link() ->
    start_link(?DEFAULT_LIB_PATH).


% Callbacks

init([LibPath]) ->
    case erl_ddll:load_driver(LibPath, ?DRIVER_NAME) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> exit(erl_ddll:format_error(Error))
    end,
    ets:new(?DRIVER_TABLE, [set, public, named_table]),
    PortCount = erlang:system_info(schedulers),
    lists:foreach(fun(I) ->
                    Port = open_port({spawn_driver, ?DRIVER_NAME}, []),
                    ets:insert(?DRIVER_TABLE, {{?DRIVER_TAG, I}, Port})
                end, 
                lists:seq(1, PortCount)),
    {ok, []}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, _Reason}, State) ->
    case ets:match(?DRIVER_TABLE, {'$1', Port}) of
        [] ->
            ok;
        [[PortName]|_] ->
            NewPort = open_port({spawn_driver, ?DRIVER_NAME}, []),
            ets:insert(?DRIVER_TABLE, {PortName, NewPort});
        _Any ->
            ok
    end,
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    Ports = ets:tab2list(?DRIVER_TABLE),
    lists:foreach(fun(_PortName, Port) -> port_close(Port) end, Ports),
    ok.

% internal functions

control(Cmd, Data) ->
    PortCount = erlang:system_info(schedulers),
    PortIndex = erlang:system_info(scheduler_id) rem PortCount + 1,
    [{_, Port} | _] = ets:lookup(?DRIVER_TABLE, {?DRIVER_TAG, PortIndex}),
    erlang:port_control(Port, Cmd, Data).

%%%% API %%%%

encode_json(Term) ->
	control(0, term_to_binary(Term)).

decode_json(Json) -> % Json is an iolist
	case control(1, [Json, 0]) of
		[] ->
			receive {json, Decoded} -> Decoded end;
		Error ->
			throw({invalid_json, binary_to_list(Error)})
	end.
