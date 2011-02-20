%%%-------------------------------------------------------------------
%%% File    : rkgate_login.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 20 Feb 2011 by Brendon <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rkgate_login).

-include("rkgate_util.hrl").

-export([login/2, get_avatars/1]).

-define(QUERY(T, P), rkgate_mongo:find(T, P)).

login(Username, Password) ->
    case ?QUERY(reckoner.users, {username, Username}) of
        [UserDoc] ->
            {Pass} = bson:lookup(password, UserDoc),
            ?DBG({checking, Pass, Password}),
            case verify_password(Pass, Password) of
                true -> 
                    {ok, UserDoc};
                _ -> {error, wrong_pass}
            end;
        [] ->
            ?DBG(no_results),
            {error, not_found};
        Other ->
            ?DBG({login_error, results, Other}),
            {error, multiple_results}
    end.

get_avatars(UID) ->
    ?QUERY(reckoner.avatars, {owner, {UID}}).

verify_password(Real, Real) -> true;
verify_password(_, _) -> false.
    
