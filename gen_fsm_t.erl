%%% @doc One sentence blurb.
%%%
%%% More detailed description.
%%%
%%% @author JSmith <john.smith@gmail.com>
-module(gen_fsm_t).
-export([start/1, start_link/1]).
-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).
-export([first_state/2, first_state/3]).
-vsn(0).


%% @doc 
%% @spec term()->{ok,pid()} | ignore | {error,term()}
start(Args) -> gen_fsm:start(?MODULE, Args, []).
%% @spec term()->{ok,pid()} | ignore | {error,term()}
start_link(Args) -> gen_fsm:start_link(?MODULE, Args, []).


init(_Args) ->
    {ok, first_state, state_data}.

first_state(_Event, StateData) ->
    {stop, unimplemented, StateData}.

first_state(_Event, _From, StateData) ->
    {stop, unimplemented, StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop, unimplemented, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, unimplemented, StateData}.

handle_info(_Info, _StateName, StateData) ->
    {stop, unimplemented, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
