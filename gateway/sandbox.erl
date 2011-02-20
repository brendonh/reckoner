#! /usr/bin/env escript
%%! -smp enable -pa ebin -pa ../deps/bson/ebin

main([]) ->
    Options = [binary, {packet, 2}, {active, true}],
    {ok, Socket} = gen_tcp:connect(localhost, 13770, Options), 
    
    Doc = {username, <<"brendonh">>, password, <<"arthur">>, rpcid, 1234},
    
    BSON = bson_binary:put_document(Doc),

    Type = 0,

    Packet = <<Type:2/integer-big-unsigned-unit:8, BSON/binary>>,

    gen_tcp:send(Socket, Packet),
    
    {ok, User} = get_rpc_reply(1234),
    
    {[Avatar|_Rest]} = bson:lookup(avatars, User),
    
    {AvatarID} = bson:lookup('_id', Avatar),

    io:format("Avatar ID: ~p~n", [AvatarID]),
    
    BSON2 = bson_binary:put_document({id, AvatarID, rpcid, 5432}),

    Packet2 = <<2:2/integer-big-unsigned-unit:8, BSON2/binary>>,

    gen_tcp:send(Socket, Packet2),

    {ok, StatusDoc} = get_rpc_reply(5432),

    io:format("Avatar reply: ~p~n", [StatusDoc]),

    %receive after 1000 -> ok end,

    ok.


get_rpc_reply(ID) ->
	receive 
        {tcp, _Port, Bin}=Message ->
            <<1:2/integer-big-unsigned-unit:8, Content/binary>> = Bin,
            {Doc, <<>>} = bson_binary:get_document(Content),
            case bson:lookup(rpcid, Doc) of
                {ID} -> {ok, Doc};
                _ ->
                    self() ! Message,
                    get_rpc_reply(ID)
            end
    after 2000 -> 
            io:format("Timeout~n")
    end.
