-define(GV(E, P), proplists:get_value(E, P)).
-define(GVD(E, P, D), proplists:get_value(E, P, D)).
-define(DBG(T), io:format("~p ~p~n", [self(), T])).
-define(DBGS(T), io:format("~p ~s~n", [self(), T])).
