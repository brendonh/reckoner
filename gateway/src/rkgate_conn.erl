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
  userID=none
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
            {ok, UID} = rkgate_login:login(Username, Password),
            ?DBG({login, Username, rkgate_util:format_oid(UID)}),
            {noreply, State#state{userID=UID}};
        _ ->
            {stop, normal, State}
    end;

handle_message(login, _Content, State) ->
    ?DBG(repeated_login_attempt),
    {noreply, State}.


%%====================================================================
%% Utility
%%====================================================================

%% To be improved!
lookup_type(0) -> login;
lookup_type(_) -> unknown.
     
