%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.
%%% @author JStmith <john.smith@gmail.com>
-module(gen_event).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).
-vsn(0).

-record(ate, 
    { field = undefined :: any() %
    }).


init(_Args) ->
    St = #ate{
    },
    {ok, St}.

handle_event(_Event, St) -> {ok, St}.
handle_call(_Request, St) -> {ok,unimplemented,St}.
handle_info(_Info, St) -> {ok, St}.
terminate(_Arg, _St) -> arbitrary_term.
code_change(_OldVsn, St, _Extra) -> {ok, St}.
