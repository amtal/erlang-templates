%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.
%%% @author JSmith <john.smith@gmail.com>
-module(gen_server_t).
-export([start/1, start_link/1]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/3, code_change/4]).
-vsn(0).

-record(state, ( field  %
               )).

%% @doc 
%% @spec (term())->{ok,pid()} | ignore | {error,term()}
start(Args) -> gen_fsm:start(?MODULE, Args, []).
%% @doc See: {@link start/1}
%% @spec (term())->{ok,pid()} | ignore | {error,term()}
start_link(Args) -> gen_fsm:start_link(?MODULE, Args, []).


%% @hidden gen_server
init(_Args) ->
    State = #state{
        field = undefined
    },
    {ok, State}.

%% @hidden gen_server
handle_call(_Request, _From, State) ->
    {stop, unimplemented, State}.

%% @hidden gen_server
handle_cast(_Request, State) ->
    {stop, unimplemented, State}.

%% @hidden gen_server
handle_info(_Info, State) ->
    {stop, unimplemented, State}.

%% @hidden gen_server
terminate(_Reason, State) ->
    ok.

%% @hidden gen_server
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
