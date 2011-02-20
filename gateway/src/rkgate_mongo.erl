%%%-------------------------------------------------------------------
%%% File    : rkgate_mongo.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : Convenience stuff
%%%
%%% Created : 20 Feb 2011 by Brendon <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rkgate_mongo).

-include("rkgate_util.hrl").

-compile(export_all).

-define(SERVER, rkgate_mongodb).
-define(DB, reckoner).

find(Table, Params) ->
    ?DBG({mquery, Table, Params}),
    {ok, Cur} = mongo:do(
                  safe, master, ?SERVER, ?DB,
                  fun() -> mongo:find(Table, Params) end),
    mongo:rest(Cur).

get(Table, ID) ->
    ?DBG({get, Table, ID}),
    {ok, {Doc}} = mongo:do(safe, master, ?SERVER, ?DB,
                        fun() ->
                                mongo:find_one(Table, {'_id', ID})
                        end),
    Doc.
    
