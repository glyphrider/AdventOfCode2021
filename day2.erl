-module(navigation).

-export([aoc2a/0,aoc2b/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(NAVIGATION_DATA_FILE,"day2.txt").

aoc2a() ->
    {X,Y} = calc_location(load_navigation_data(?NAVIGATION_DATA_FILE)),
    io:format("resultant position -> ~p ~p~n",[X,Y]),
    io:format("horizontal * depth == ~p~n",[X*Y]).

aoc2b() ->
    {X,Y,_Aim} = calc_location2(load_navigation_data(?NAVIGATION_DATA_FILE)),
    io:format("resultant position -> ~p ~p~n",[X,Y]),
    io:format("horizontal * depth == ~p~n",[X*Y]).

calc_location(MovementList) ->
lists:foldl(fun({forward,N},{X,Y}) ->
        {X+N,Y};
    ({down,N},{X,Y}) -> {X,Y+N};
    ({up,N},{X,Y}) -> {X,Y-N} end, {0,0}, MovementList).

calc_location2(MovementList) ->
    lists:foldl(fun({forward,N},{X,Y,Aim}) -> {X+N,Y+N*Aim,Aim};
    ({down,N},{X,Y,Aim}) -> {X,Y,Aim+N};
    ({up,N},{X,Y,Aim}) -> {X,Y,Aim-N} end, {0,0,0}, MovementList).

load_navigation_data(Filename) ->
	{ok,MP} = re:compile("([a-z]*) ([0-9]*).*"),
	{ok,IoDevice} = file:open(Filename,read),
	load_navigation_data(file:read_line(IoDevice),IoDevice,MP,[]).

load_navigation_data(eof,_IoDevice,_MP,NavigationData) ->
	lists:reverse(NavigationData);
load_navigation_data({ok,Data},IoDevice,MP,NavigationData) ->
	{match,[_,{DirStart,DirLen},{MagStart,MagLen}]} = re:run(Data,MP),
	Direction = string:substr(Data,DirStart+1,DirLen),
	Magnitude = string:substr(Data,MagStart+1,MagLen),
	load_navigation_data(file:read_line(IoDevice),IoDevice,MP,[{safe_atom(Direction),list_to_integer(Magnitude)}|NavigationData]).

safe_atom(List) ->
	case list_to_existing_atom(List) of
		badarg -> list_to_atom(List);
		Atom -> Atom
	end.

-ifdef(EUNIT).
-include("navigation_data.hrl").

empty_test() ->
    {0,0} = calc_location([]).

dive_test() ->
    {0,2} = calc_location([{down,2}]).

surface_test() ->
    {0,0} = calc_location([{down,2},{up,2}]).

move_forward_test() ->
    {5,0} = calc_location([{forward,5}]).

calc2_empty_test() ->
    {0,0,0} = calc_location2([]).

calc2_angle_down_test() ->
    {0,0,2} = calc_location2([{down,2}]).

calc2_angle_up_test() ->
    {0,0,-1} = calc_location2([{up,1}]).

calc2_move_test() ->
    {5,10,2} = calc_location2([{down,2},{forward,5}]).

load_navigation_data_test() ->
	?NAVIGATION_DATA = load_navigation_data("day2.txt").

-endif.
