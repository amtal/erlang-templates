%%% @doc Reference on error catching and types.
-module(err).
-compile(export_all).
-define(F(X), fun()->X end).

go() ->
    %   EXCEPTION CLASSES
    % there are three classes of exceptions: thrown exceptions,
    % exit signals, and run-time errors:
    [ {three_classes,
        [catch F() || F<-[ ?F(throw({my_throw, 1}))
                         , ?F(exit(2))
                         , ?F(1/0)]
                         ]}
    % note how throws have the least information: they're there for
    % user-defined early exit behaviours
    %
    % exits are meant to do just that, so there's the extra term
    % 
    % run-time errors are meant to be handled by some
    % supervising/monitoring process, thus they're meant to exit:
    % they include a stack trace due to their purpose

    %   IN-PROCESS RUN-TIME EXCEPTIONS
    % in-process exceptions are of class 'error', and don't come
    % with a call stack if caught via try-catch:
    , {in_process_try_catch,
        [ try F() catch error:Reason->Reason end || F<-error()]}
    % but do come with a call stack with just catch
    , {in_process_catch,
        [ case catch F() of 
              {'EXIT', {Reason,_Stack}}->Reason
          end || F<-error()]}
    % catch doesn't differentiate between errors,catches,and throws
    % though (since they're actually all throws?)
    %
    % Using a catch inside a case statement is a very bad idea,
    % since unexpected exceptions will 1. get caught 2. raise a
    % new, unrelated {badmatch,UnhandledCaughtE} runtime exception.
    % Along with a whole new stack trace. Nested exceptions are NOT
    % fun to read, use catch very carefully.
    %
    % You could use catch safely by re-throwing anything you don't
    % explicitly match: this is probably what 'try' is syntax sugar
    % for :)
    , {simu_errors, [catch F() || F<-
    % errors can be simulated via the BIF error/1 or /2, and exits
    % via exit/1
    % 
    % you can also just throw/1 runtime errors and exits, since
    % that's the base on which they're built
        [ ?F(error(err_kind,err_reason))
        , ?F(exit(e_reason))
        , ?F(throw({'EXIT',e_reason}))
        ]]}
    ] .

%% Most of the different types of errors.
%% See:
%% http://www.erlang.org/doc/reference_manual/errors.html#id77486
error()->
    [ ?F(abs(not_a_number))
    , ?F(1/0)
    , ?F({ok,X} = {not_ok,at_all})
      % function_clause: how to get it quickly?
    , ?F(case 42 of 1->true; 2->false end)
    , ?F(if 42==49->never end)
    , ?F(try 1+1 of 3->ok catch _:_-> error end)
    , ?F(does_not_exist:f(1,2,3))
      % {badfun,F}: no clue what this is
    , ?F(apply(fun abs/1, [1,2]))
    , ?F(receive nothing->ok after "1"++"2"->fail end)
    , ?F(link(list_to_pid("<0.1.2>")))
      % system_limit is probably number of procs and whatnot
    ].

%% can't be caught inside shell process, must be spawned
%% off+monitored
%    , ?F(spawn(fun()->throw("catch this!") end))
%% Note how throwing in a process with no catch ends up raising a runtime error. Which is actually just a throw, with an extra atom at the start. Slightly confusing, but zero hidden magic as far as I can tell.

% TODO: add some monitor tests. This stuff is an extremely crucial
% part of the "let it fail, and have another process handle the
% failure" methodology, but seems kinda poorly documented.

% VERIFY:
%   all errors/exits are built on throw/1 + catch statement
%   all the variations of try are just syntax sugar over
% case+catch, except possibly the 'after' stuff
