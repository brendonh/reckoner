%%%-------------------------------------------------------------------
%%% File    : rkgate_conn_sup.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 20 Feb 2011 by Brendon <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rkgate_conn_sup).

-behaviour(supervisor).

-include("rkgate_util.hrl").

%% API
-export([start_link/0, start_client/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_client(Socket) ->
    supervisor:start_child(?MODULE, [Socket]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    Conn = {conn,{rkgate_conn,start_link,[]},
            temporary,2000,worker,[rkgate_conn]},
    {ok,{{simple_one_for_one,0,1}, [Conn]}}.


%%====================================================================
%% Internal functions
%%====================================================================
