%%%-------------------------------------------------------------------
%%% File    : rkgate_conn.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 20 Feb 2011 by Brendon <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rkgate_conn).

-behaviour(gen_server).

-include("rkgate_util.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  socket,
  userID=none,
  avatar=none,
  space=none
}).

%%====================================================================
%% API
%%====================================================================

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Socket]) ->
    process_flag(trap_exit, true),
    {ok, {IP, _Port}} = inet:peername(Socket),
    ?DBG({client_conn_starting, Socket, IP}),
    {ok, #state{socket=Socket}}.

%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast(socket_ready, #state{socket=Socket}=State) ->
    inet:setopts(Socket, [{active, once}]),    
    {noreply, State};

handle_cast(Other, State) ->
    ?DBG({unexpected_cast, Other}),
    {noreply, State}.

%%--------------------------------------------------------------------

% Pre-login
handle_info({tcp, Socket, Bin}, #state{socket=Socket, userID=none}=State) ->
    inet:setopts(Socket, [{active, once}]),
    <<Type:2/integer-big-unsigned-unit:8, Content/binary>> = Bin,
    case lookup_type(Type) of
        login ->
            handle_message(login, Content, State);
        Other ->
            ?DBG({invalid_pre_login_message, Other, Type}),
            {noreply, State}
    end;

% Post-login
handle_info({tcp, Socket, Bin}, #state{socket=Socket}=State) ->
    inet:setopts(Socket, [{active, once}]),
    <<Type:2/integer-big-unsigned-unit:8, Content/binary>> = Bin,
    case lookup_type(Type) of
        unknown ->
            ?DBG({unknown_type, Type, Content}),
            {noreply, State};
        TypeAtom ->
            handle_message(TypeAtom, Content, State)
    end;

handle_info({tcp_closed, Socket}, #state{socket=Socket}=State) ->
    ?DBG({client_disconnected, Socket}),
    {stop, normal, State};

handle_info(Info, State) ->
    ?DBG({unexpected_info, Info}),
    {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%====================================================================
%% Dispatch
%%====================================================================

handle_message(login, Content, #state{userID=none}=State) ->
    {Doc, <<>>} = bson_binary:get_document(Content),
    {Username} = bson:lookup(username, Doc),
    {Password} = bson:lookup(password, Doc),
    case {Username, Password} of
        {Un, Pw} when is_binary(Un) andalso is_binary(Pw) ->
            {ok, UserDoc} = rkgate_login:login(Username, Password),

            {UID} = bson:lookup('_id', UserDoc),

            ?DBG({login, Username}),
            NewState = State#state{userID=UID},

            % XXX TODO: Generalize safety
            Avatars = [bson:include(['_id', name, location], A) ||
                          A <- rkgate_login:get_avatars(UID)],
            
            rpc_reply(Doc, [{avatars, Avatars}], NewState),
            {noreply, NewState};
        _ ->
            {stop, normal, State}
    end;

handle_message(avatar, Content, #state{avatar=none}=State) ->
    {Doc, <<>>} = bson_binary:get_document(Content),
    {AvatarID} = bson:lookup(id, Doc),
    case AvatarID of
        X when is_binary(X) ->
            {ok, AvatarDoc} = rkgate_login:avatar(
                                State#state.userID, AvatarID),
            {AID} = bson:lookup('_id', AvatarDoc),
            ?DBG({avatar, AID}),
            NewState = State#state{avatar=AID},
            rpc_reply(Doc, [{success, 1}], NewState),
            {noreply, NewState};
        _ ->
            {stop, normal, State}
    end;
            

handle_message(login, _Content, State) ->
    ?DBG(repeated_login_attempt),
    {noreply, State};

handle_message(avatar, _Content, State) ->
    ?DBG(repeated_avatar_attempt),
    {noreply, State}.


%%====================================================================
%% RPC
%%====================================================================

flatten_proplist(Proplist) ->
    list_to_tuple(
      lists:reverse(
        lists:foldl(
          fun({K, V}, Acc) -> [V, K | Acc] end,
          [], Proplist))).
                   

rpc_reply(RPCDoc, Reply, State) ->
    ID = case bson:lookup(rpcid, RPCDoc) of
             {RPCID} when is_integer(RPCID) -> RPCID;
             _ -> 0
         end,
    ReplyDoc = [{rpcid, ID} | Reply],
    BSON = bson_binary:put_document(flatten_proplist(ReplyDoc)),
    Type = 1,
    Packet = <<Type:2/integer-big-unsigned-unit:8, BSON/binary>>,
    gen_tcp:send(State#state.socket, Packet).
    

%%====================================================================
%% Utility
%%====================================================================

%% XXX TODO: Not this!
lookup_type(0) -> login;
lookup_type(1) -> rpc_reply;
lookup_type(2) -> avatar;
lookup_type(_) -> unknown.
     
