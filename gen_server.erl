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

-record(ate, 
    { field = undefined :: any() %
    }).

%% @doc 
-spec start(any())->{ok,pid()} | ignore | {error,any()}
start(Args) -> gen_server:start(?MODULE, Args, []).
%% @doc See: {@link start/1}
-spec start_link(any())->{ok,pid()} | ignore | {error,any()}
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).


%% @hidden gen_server
init(_Args) ->
    St = #ate{
        field = undefined
    },
    {ok, St}.

%% @hidden gen_server
handle_call(_Request, _From, St) ->
    {stop, unimplemented, St}.

%% @hidden gen_server
handle_cast(_Request, St) ->
    {stop, unimplemented, St}.

%% @hidden gen_server
handle_info(_Info, St) ->
    {stop, unimplemented, St}.

%% @hidden gen_server
terminate(_Reason, _St) ->
    ok.

%% @hidden gen_server
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
