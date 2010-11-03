%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.
%%% @author JStmith <john.smith@gmail.com>
-module(supervisor).

-behaviour(supervisor).
-export([start_link/1, init/1]).
-vsn(0).


%% @doc Short description.
%% @spec (term())->{ok,Pid}|ignore|{error,Reason}
init(Args) ->
    supervisor:start_link(?MODULE, Args).

%% @hidden supervisor
init(Args) ->
    Restart = {one_for_one, 2, 5},
    C0 = { arbitrary_internal_name_term
         , {mod,func,args}
         , permanent
         , 200 % ms
         , worker
         , [mod]
         },
    {ok,{Restart,[C0]}
