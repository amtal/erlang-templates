%%% @doc Erlang-Haskell integration test.
%%%
%%% Lets a Haskell node register a pid, then sends ping messages.
%%%
%%% Usage:
%%%         (erlang node, start first)
%%% > erl -sname erl
%%% >>> c(node).
%%% >>> node:start().
%%%         (haskell node)
%%% > runghc node.hs
%%% @author Amtal <alex.kropivny@gmail.com>
-module(node).
-export([start/0, start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-vsn(0).

-record(ate, 
    { h_node = unknown :: pid() %
    }).

%% @doc 
-spec start()->{ok,pid()} | ignore | {error,any()}.
start() -> gen_server:start({local,?MODULE}, ?MODULE, none, []).
%% @doc See: {@link start/1}
-spec start_link()->{ok,pid()} | ignore | {error,any()}.
start_link() -> gen_server:start_link({local,?MODULE}, ?MODULE, none, []).


%% @hidden gen_server
init(_Args) ->
    {ok, #ate{}}.

%% @hidden gen_server
handle_cast({register, Pid}, St) ->
    io:format("Cast: ~p~n", [Pid]),
    Pid ! ping,
    {noreply, St#ate{h_node=Pid}}.

%% @hidden gen_server
handle_call(_Request, _From, St) ->
    io:format("Call: ~n", []),
    {stop, unimplemented, St}.

%% @hidden gen_server
handle_info(pong, St) ->
    io:format("pong~n", []),
    {noreply, St}.

%% @hidden gen_server
terminate(_Reason, _St) ->
    ok.

%% @hidden gen_server
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
