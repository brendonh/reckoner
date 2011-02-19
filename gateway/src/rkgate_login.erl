%%%-------------------------------------------------------------------
%%% File    : rkgate_login.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 20 Feb 2011 by Brendon <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rkgate_login).

-include("rkgate_util.hrl").

-export([login/2]).

login(Username, Password) ->
    {ok, Cur} = mongo:do(
                  safe, master, rkgate_mongodb, reckoner,
                  fun() -> mongo:find(reckoner.users, {username, Username}) end),
    case mongo:rest(Cur) of
        [UserDoc] ->
            {Pass} = bson:lookup(password, UserDoc),
            ?DBG({checking, Pass, Password}),
            case verify_password(Pass, Password) of
                true -> 
                    {{UID}} = bson:lookup('_id', UserDoc),
                    {ok, UID};
                _ -> {error, wrong_pass}
            end;
        [] ->
            ?DBG(no_results),
            {error, not_found};
        Other ->
            ?DBG({login_error, results, Other}),
            {error, multiple_results}
    end.


verify_password(Real, Real) -> true;
verify_password(_, _) -> false.
    
