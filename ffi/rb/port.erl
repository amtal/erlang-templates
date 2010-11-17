-module(port).
-compile(export_all).

go()->
    Port = open_port({spawn,"ruby port.rb"}, 
                     [use_stdio,hide,binary,{packet,1}]),
    Call = bert:encode({square,25}),
    port_command(Port, [size(Call), Call]),
    receive
        {Port,{data,<<_Len:8/integer, Q/binary>>}} ->
            {reply,N} = bert:decode(Q),
            io:format("~p~n",[N])
    end.
