%%%-------------------------------------------------------------------
%%% File    : rkgate_sup.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 20 Feb 2011 by Brendon <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rkgate_sup).

-behaviour(supervisor).

-include("rkgate_util.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).


%%====================================================================
%% Supervisor callbacks
%%====================================================================
init(Args) ->

    ListenPort = ?GV(listen_port, Args),
    Listener = {rkgate_listener,{rkgate_listener,start_link,[ListenPort]},
                permanent,2000,worker,[rkgate_listener]},

    ConnSup = {rkgate_conn_sup, {rkgate_conn_sup, start_link, []},
               permanent,2000,supervisor,[rkgate_conn_sup]},

    {ok,{{one_for_one,0,1}, [ConnSup, Listener]}}.


%%====================================================================
%% Internal functions
%%====================================================================
