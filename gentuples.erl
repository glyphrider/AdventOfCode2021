-module(gentuples).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([gentuples/1,gentriples/1]).

gentuples([]) ->
	[];
gentuples([_]) ->
	[];
gentuples([_|B]=A) ->
	gentuples(A,B,[]).

gentuples([A|_],[C],Tuples) ->
	lists:reverse([{A,C}|Tuples]);
gentuples([A|B],[C|D],Tuples) ->
	gentuples(B,D,[{A,C}|Tuples]).

gentriples([]) ->
	[];
gentriples([_]) ->
	[];
gentriples([_,_]) ->
	[];
gentriples([_|[_|C]=B]=A) ->
	gentriples(A,B,C,[]).

gentriples([A|_],[B|_],[C],L) ->
	lists:reverse([{A,B,C}|L]);
gentriples([A|B],[C|D],[E|F],L) ->
	gentriples(B,D,F,[{A,C,E}|L]).

-ifdef(EUNIT).
empty_test() ->
	[] = gentuples([]).

single_test() ->
	[] = gentuples([x]).


one_pair_test() ->
	[{a,b}] = gentuples([a,b]).

two_pair_test() ->
	[{a,b},{b,c}] = gentuples([a,b,c]).

empty_triples_test() ->
	[] = gentriples([]).

single_triples_test() ->
	[] = gentriples([a]).

double_triples_test() ->
	[] = gentriples([a,b]).

one_triple_test() ->
	[{a,b,c}] = gentriples([a,b,c]).

two_triple_test() ->
	[{a,b,c},{b,c,d}] = gentriples([a,b,c,d]).

-endif.
