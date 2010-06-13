%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.
%%% @author JSmith <john.smith@gmail.com>
-module(gen_fsm_t).
-export([start/1, start_link/1]).
-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).
-export([first_state/2, first_state/3]).
-vsn(0).


%% @doc 
%% @spec (term())->{ok,pid()} | ignore | {error,term()}
start(Args) -> gen_fsm:start(?MODULE, Args, []).
%% @doc See: {@link start/1}
%% @spec (term())->{ok,pid()} | ignore | {error,term()}
start_link(Args) -> gen_fsm:start_link(?MODULE, Args, []).


%% @hidden
init(_Args) ->
    {ok, first_state, state_data}.

%% @hidden
first_state(_Event, StateData) ->
    {stop, unimplemented, StateData}.

%% @hidden
first_state(_Event, _From, StateData) ->
    {stop, unimplemented, StateData}.

%% @hidden
handle_event(_Event, _StateName, StateData) ->
    {stop, unimplemented, StateData}.

%% @hidden
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, unimplemented, StateData}.

%% @hidden
handle_info(_Info, _StateName, StateData) ->
    {stop, unimplemented, StateData}.

%% @hidden
terminate(_Reason, _StateName, _StateData) ->
    ok.

%% @hidden
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
