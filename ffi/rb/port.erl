-module(port).
-compile(export_all).

go()->
    Port = open_port({spawn,"ruby port.rb"}, 
                     [use_stdio,hide,binary,{line,1000}]),
    f(Port).

f(Port)->
    Call = bert:encode({square,25}),
    io:format(standard_error, "~p~n",[[{sending,Call},{size,size(Call)}]]),
    port_command(Port, [Call]),
    receive
        {Port,{data,<<Q>>}} ->
            {reply,N} = bert:decode(Q),
            io:format(standard_error, "~p~n",[N]);
        Other ->
            io:format(standard_error, "~p~n",[{other,Other}])
    end,
    f(Port).
