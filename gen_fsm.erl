%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.
%%% @author JSmith <john.smith@gmail.com>
-module(gen_fsm).
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


%% @hidden gen_fsm
init(_Args) ->
    {ok, first_state, state_data}.

%% @hidden gen_fsm
first_state(_Event, StData) ->
    {stop, unimplemented, StData}.

%% @hidden gen_fsm
first_state(_Event, _From, StData) ->
    {stop, unimplemented, StData}.

%% @hidden gen_fsm
handle_event(_Event, _StName, StData) ->
    {stop, unimplemented, StData}.

%% @hidden gen_fsm
handle_sync_event(_Event, _From, _StName, StData) ->
    {stop, unimplemented, StData}.

%% @hidden gen_fsm
handle_info(_Info, _StName, StData) ->
    {stop, unimplemented, StData}.

%% @hidden gen_fsm
terminate(_Reason, _StName, _StData) ->
    ok.

%% @hidden gen_fsm
code_change(_OldVsn, StName, StData, _Extra) ->
    {ok, StName, StData}.
