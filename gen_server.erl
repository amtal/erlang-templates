%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.
%%% @author JSmith <john.smith@gmail.com>
-module(gen_server).
-export([start_link/1]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-vsn(0).

-record(ate, 
    { field = undefined :: any() %
    }).

-spec start_link(any())->{ok,pid()} | ignore | {error,any()}.
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).


init(_Args) ->
    St = #ate{
        field = undefined
    },
    {ok, St}.

handle_call(_Request, _From, St) -> {stop, unimplemented, St}.
handle_cast(_Request, St) -> {stop, unimplemented, St}.
handle_info(_Info, St) -> {stop, unimplemented, St}.
terminate(_Reason, _St) -> ok.
code_change(_OldVsn, St, _Extra) -> {ok, St}.
