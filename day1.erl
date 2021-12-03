-module(day1).

-export([day1/0,day1b/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SONAR_DATA_FILE,"day1.txt").

day1() ->
    Result = count_increases(load_sonar_data(?SONAR_DATA_FILE)),
    io:format("sonar depth increases -> ~p~n",[Result]),
    Result.

day1b() ->
    Result = count_increases(lists:map(fun({A,B,C}) -> A + B + C end,gentuples:gentriples(load_sonar_data(?SONAR_DATA_FILE)))),
    io:format("sonar depth increases of sums -> ~p~n",[Result]),
    Result.

count_increases(ListOfSonarDepths) ->
    lists:foldl(fun({A, B},Increase) when B > A -> Increase + 1;
		   (_,Increase) -> Increase end, 0, gentuples:gentuples(ListOfSonarDepths)).

load_sonar_data(Filename) ->
        {ok,MP} = re:compile("([0-9]*).*"),
        {ok,IoDevice} = file:open(Filename,read),
        load_sonar_data(file:read_line(IoDevice),IoDevice,MP,[]).

load_sonar_data(eof,_IoDevice,_MP,NavigationData) ->
        lists:reverse(NavigationData);
load_sonar_data({ok,Data},IoDevice,MP,NavigationData) ->
        {match,[_,{DepthStart,DepthLen}]} = re:run(Data,MP),
        Depth = string:substr(Data,DepthStart+1,DepthLen),
        load_sonar_data(file:read_line(IoDevice),IoDevice,MP,[list_to_integer(Depth)|NavigationData]).

-ifdef(EUNIT).
-include("day1.hrl").

no_data_test() ->
    0 = count_increases([]).

no_increase_test() ->
    0 = count_increases([2,1]).

increase_test() ->
    1 = count_increases([1,2]).

load_sonar_data_test() ->
	?SONAR_DATA = load_sonar_data(?SONAR_DATA_FILE).
-endif.
