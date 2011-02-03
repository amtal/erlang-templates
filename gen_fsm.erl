%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.
%%% @author JSmith <john.smith@gmail.com>
-module(gen_fsm).
-export([start_link/1]).
-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).
-export([first_state/2, first_state/3]).
-vsn(0).


-spec start_link(term())->{ok,pid()} | ignore | {error,term()}.
start_link(Args) -> gen_fsm:start_link(?MODULE, Args, []).


init(_Args) ->
    {ok, first_state, state_data}.

first_state(_Event, StData) ->
    {stop, unimplemented, StData}.
first_state(_Event, _From, StData) ->
    {stop, unimplemented, StData}.

handle_event(_Event, _StName, StData) -> {stop, unimplemented, StData}.
handle_sync_event(_Event, _From, _StName, StData) ->
    {stop, unimplemented, StData}.
handle_info(_Info, _StName, StData) -> {stop, unimplemented, StData}.
terminate(_Reason, _StName, _StData) -> ok.
code_change(_OldVsn, StName, StData, _Extra) -> {ok, StName, StData}.
