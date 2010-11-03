%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.
%%% @author JStmith <john.smith@gmail.com>
-module(gen_event).

-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/3, handle_info/2, terminate/2,
         code_change/3]).
-vsn(0).

-record(s, { field = undefined %
           }).


%% @hidden gen_event
init(_Args) ->
    St = #s{
    },
    {ok, St}.

%% @hidden gen_event
handle_event(_Event, St) ->
    {ok, St}.

%% @hidden gen_event
handle_call(_Request, St) ->
    {ok,unimplemented,St).

%% @hidden gen_event
handle_info(_Info, St) ->
    {ok, St}.

%% @hidden gen_event
terminate(_Arg, _St) ->
    arbitrary_term.

%% @hidden gen_event
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
