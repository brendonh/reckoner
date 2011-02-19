%%%-------------------------------------------------------------------
%%% File    : rkgate_app.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 20 Feb 2011 by Brendon <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rkgate_app).

-behaviour(application).

-include("rkgate_util.hrl").

-export([launch/0]).

%% Application callbacks
-export([start/2, stop/1]).

launch() ->
    application:start(mongodb),
    application:start(rkgate).

%%====================================================================
%% Application callbacks
%%====================================================================
start(_Type, _StartArgs) ->

    {ok, MongoConf} = application:get_env(mongodb),
    {ok, Conn} = mongo:connect(MongoConf),
    
    register(rkgate_mongodb, Conn),

    case rkgate_sup:start_link(application:get_all_env()) of
        {ok, Pid} -> 
            {ok, Pid};
        Error ->
            Error
                end.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
