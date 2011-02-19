#! /usr/bin/env escript
%%! -smp enable -pa ebin -pa ../deps/bson/ebin -boot start_sasl

main([]) ->
    Options = [binary, {packet, 2}, {active, true}],
    {ok, Socket} = gen_tcp:connect(localhost, 13770, Options), 
    
    Doc = {username, <<"brendonh">>, password, <<"arthur">>},
    
    BSON = bson_binary:put_document(Doc),

    Type = 0,

    Packet = <<Type:2/integer-big-unsigned-unit:8, BSON/binary>>,

    io:format("Packet: ~p~n", [Packet]),

    gen_tcp:send(Socket, Packet),
    gen_tcp:send(Socket, Packet).
