#! /usr/bin/env escript
%%! -smp enable -pa ebin -pa ../deps/bson/ebin -boot start_sasl

main([]) ->
    Options = [binary, {packet, 2}, {active, true}],
    {ok, Socket} = gen_tcp:connect(localhost, 13770, Options), 
    
    Doc = {username, <<"brendonh">>, password, <<"arthur">>},
    
    BSON = bson_binary:put_document(Doc),

    Type = 0,

    Packet = <<Type:2/integer-big-unsigned-unit:8, BSON/binary>>,

    gen_tcp:send(Socket, Packet),
    
    flush().



flush() ->
	receive 
        {tcp, _Port, Bin} ->
            <<Type:2/integer-big-unsigned-unit:8, Content/binary>> = Bin,
            {Doc, <<>>} = bson_binary:get_document(Content),
            io:format("Message ~p: ~p~n", [Type, bson:fields(Doc)]);
		Any ->
            io:format("Unknown: ~p~n", [Any]),
			flush()
	after 
		2000 ->
			true
	end.
