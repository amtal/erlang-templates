%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.
%%% @author JSmith <john.smith@gmail.com>
-module(gen_server).
-export([start/1, start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-vsn(0).

-record(s, { field  %
           }).

%% @doc 
%% @spec (term())->{ok,pid()} | ignore | {error,term()}
start(Args) -> gen_server:start(?MODULE, Args, []).
%% @doc See: {@link start/1}
%% @spec (term())->{ok,pid()} | ignore | {error,term()}
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).


%% @hidden gen_server
init(_Args) ->
    State = #s{
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
terminate(_Reason, _State) ->
    ok.

%% @hidden gen_server
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
