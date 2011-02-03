%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.
%%% @author JStmith <john.smith@gmail.com>
-module(behaviour).
-export([start/2]).
-export([behaviour_info/0]).
-vsn(0).


%% @doc Short description.
%% @spec (atom(),term())->term()
start(Module,Args) ->
    ?MODULE:func_arity_tuples(Args).

behaviour_info() ->
    [ {func_arity_tuples, 1}
    ].
