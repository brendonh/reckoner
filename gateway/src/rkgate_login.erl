%%%-------------------------------------------------------------------
%%% File    : rkgate_login.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 20 Feb 2011 by Brendon <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rkgate_login).

-include("rkgate_util.hrl").

-export([login/2, get_avatars/1, avatar/2]).

-define(QUERY(T, P), rkgate_mongo:find(T, P)).

login(Username, Password) ->
    case rkgate_mongo:find(reckoner.users, {'_id', Username}) of
        [UserDoc] ->
            {Pass} = bson:lookup(password, UserDoc),
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

verify_password(Real, Real) -> true;
verify_password(_, _) -> false.

get_avatars(UID) ->
    rkgate_mongo:find(reckoner.avatars, {owner, UID}).


avatar(UserID, AvatarID) ->
    case rkgate_mongo:find(reckoner.avatars, {'_id', AvatarID}) of
        [AvatarDoc] ->
            {Owner} = bson:lookup(owner, AvatarDoc),
            case Owner of
                UserID ->
                    {ok, AvatarDoc};
                Other ->
                    ?DBG({invalid_avatar, Other}),
                    {error, not_owner}
            end;
        Other ->
            ?DBG({not_found, Other}),
            {error, not_found}
    end.
