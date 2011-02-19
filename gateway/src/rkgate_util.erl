%%%-------------------------------------------------------------------
%%% File    : rkgate_util.erl
%%% Author  : Brendon <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 20 Feb 2011 by Brendon <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(rkgate_util).

-compile(export_all).

parse_uuid(S) ->
    I = erlang:list_to_integer([C || C <- S, C /= $-], 16),
    <<I:16/unsigned-integer-unit:8>>.

format_uuid(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) -> 
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", 
                                [TL, TM, THV, CSR, CSL, N])).


format_oid(OID) ->
    format_oid(OID, []).

format_oid(<<Hi:4, Lo:4, Rest/binary>>, Acc) ->
    format_oid(Rest, [hexdigit(Lo), hexdigit(Hi) | Acc]);
format_oid(<<>>, Acc) ->
    lists:reverse(Acc).


hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $a + (C - 10).
