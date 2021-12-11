-module(day4).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([day4/0,day4b/0,runboard/5]).

-define(BINGO_DATA_FILE_NAME,"day4.txt").

day4() ->
    {CallLine,Pids} = load_bingo_data(),
    io:format("Launched ~p boards.~n",[length(Pids)]),
    io:format("CallLine -> ~p~n",[CallLine]),
    run_game(CallLine,Pids,true).

day4b() ->
    {CallLine,Pids} = load_bingo_data(),
    io:format("Launched ~p boards.~n",[length(Pids)]),
    io:format("CallLine -> ~p~n",[CallLine]),
    run_game(CallLine,Pids,false).

load_bingo_data() ->
    load_bingo_data(?BINGO_DATA_FILE_NAME).

load_bingo_data(Filename) ->
    {ok, IoDevice} = file:open(Filename,read),
    {ok, CallLine} = file:read_line(IoDevice),
    Pids = load_bingo_data(file:read_line(IoDevice),IoDevice,[]),
    {parse_line(CallLine),Pids}.

load_bingo_data(eof,_IoDevice,Pids) ->
    Pids;
load_bingo_data({ok,_BlankLine},IoDevice,Pids) ->
    {ok,Line1} = file:read_line(IoDevice),
    {ok,Line2} = file:read_line(IoDevice),
    {ok,Line3} = file:read_line(IoDevice),
    {ok,Line4} = file:read_line(IoDevice),
    {ok,Line5} = file:read_line(IoDevice),
    Pid = spawn(day4,runboard,[Line1,Line2,Line3,Line4,Line5]),
    io:format("spawned board ~p ~p~n",[length(Pids),Pid]),
    load_bingo_data(file:read_line(IoDevice),IoDevice,[Pid|Pids]).

parse_line(String) ->
    {Line,_} = lists:foldl(
		 fun(Digit,{Line,unknown}) when Digit >= $0, Digit =< $9 ->
			 {Line,(Digit-$0)};
		    (Digit,{Line,Buffer}) when Digit >= $0, Digit =< $9 ->
			 {Line,Buffer*10+(Digit-$0)};
		    (_Digit,{Line,unknown}) ->
			 {Line,unknown};
		    (_Digit,{Line,Buffer}) ->
			 {[Buffer|Line],unknown}
		 end,{[],unknown},String),
    lists:reverse(Line).

parse_board(Line1,Line2,Line3,Line4,Line5) ->
    Wins = lists:map(fun(E) -> parse_line(E) end,[Line1,Line2,Line3,Line4,Line5]),
    BoardValue = lists:foldl(
		   fun(Line,Sum) ->
			   lists:foldl(fun(Square,InnerSum) -> InnerSum + Square end,0,Line) + Sum end,0,Wins),
    VWins = lists:map(fun(E) -> lists:reverse(E) end,lists:foldl(fun(Line,VWins) ->
									 put_line_into_vwins(Line,VWins,[])
								 end,[[],[],[],[],[]],Wins)),
    {board,BoardValue,Wins ++ VWins}.

put_line_into_vwins([],[],VWins) ->
    lists:reverse(VWins);
put_line_into_vwins([Elem|Line],[VWin|VWins],Result) ->
    put_line_into_vwins(Line,VWins,[[Elem|VWin]|Result]).

runboard(Line1,Line2,Line3,Line4,Line5) ->
						% setup board
						% parse lines
						% ten ways to win
						% board value
						% number of moves
    BoardState = parse_board(Line1,Line2,Line3,Line4,Line5),
    {board,BoardValue,_} = BoardState,
    io:format("spawned board ~p with a value of ~p~n",[self(),BoardValue]),
    runboard(BoardState).

runboard(BoardState) ->
    receive
	die ->
	    %% io:format("board ~p lost.~n",[self()]),
	    ok;
	{N,Controller} ->
	    marknumber(BoardState,N,Controller)
    end.

marknumber({board,BoardValue,Wins},N,Controller) ->
    {MarkedValues,NewWins,N} = lists:foldl(
			       fun(WinLine,{MarkedTotal,NewWins,Marker}) ->
				       {LineTotal,NewLine,Marker} =lists:foldl(fun(InnerMarker,{MarkedValue,Win,InnerMarker}) ->
										{MarkedValue+Marker,[x|Win],InnerMarker};
									   (WinSquare,{MarkedValue,Win,InnerMarker}) ->
										{MarkedValue,[WinSquare|Win],InnerMarker}
									end,{0,[],Marker},WinLine),
				       {MarkedTotal+LineTotal,[NewLine|NewWins],Marker}
			       end,{0,[],N},Wins),
    checkwin({board,BoardValue-MarkedValues/2,NewWins},N,MarkedValues/2,Controller).

checkwin({board,_,_}=Board,_N,0,Controller) ->
    %% io:format("  pid ~p scores no marks~n",[self()]),
    erlang:send(Controller,{ack,self()}),
    runboard(Board);
checkwin({board,BoardValue,_},N,true,Controller) ->
    io:format("  pid ~p declaring a win ~p * ~p = ~p~n",[self(),N,BoardValue,N*BoardValue]),
    erlang:send(Controller,{win,N*BoardValue,self()});
checkwin({board,_,_}=Board,_N,false,Controller) ->
    %% io:format("  pid ~p marks ~p~n",[self(),N]),
    erlang:send(Controller,{ack,self()}),
    runboard(Board);
checkwin({board,_BoardValue,Wins}=BoardState,N,_MarkedValue,Controller) ->
    DidWin = lists:any(fun(Win) ->
		      lists:all(fun(Square) -> Square == x end,Win)
	      end,Wins),
    checkwin(BoardState,N,DidWin,Controller).

run_game([],Pids,_Aggressive) ->
    io:format("No winners, but ~p board remain.~n",[length(Pids)]),
    lists:foreach(fun(Pid) ->
			  erlang:send(Pid,die)
		  end,Pids);
run_game(_CallList,[],_Aggressive) ->
    io:format("No more boards....~n",[]);
run_game([Call|CallList],Pids,true) ->
    io:format("Posting call ~p~n",[Call]),
    lists:foreach(fun(Pid) ->
			  erlang:send(Pid,{Call,self()})
		  end,Pids),
    io:format("Waiting for results from call ~p~n",[Call]),
    case get_results(Pids) of
	[] ->
	    run_game(CallList,Pids,true);
	_Winners ->
	    lists:foreach(
	      fun(Pid) ->
		      erlang:send(Pid,die)
	      end,Pids),
	    ok
    end;
run_game([Call|CallList],Pids,false) ->
    io:format("Posting call ~p (trying to lose)",[Call]),
    lists:foreach(fun(Pid) ->
			  erlang:send(Pid,{Call,self()})
		  end,Pids),
    io:format("Watiing for results from call ~p (trying to lose)~n",[Call]),
    case get_results(Pids) of
	Winners ->
	    run_game(CallList,
		     lists:filter(
		       fun(Keeper) ->
			       not lists:any(fun(Winner) ->
						     Keeper == Winner
					     end,Winners)
		       end,Pids),false)
    end.

get_results(Pids) ->
    get_results(Pids,[]).
get_results([],Winners) ->
    Winners;
get_results(Pids,Winners) ->
    receive
	{ack,Pid} ->
	    %% io:format("  got an ack from pid ~p~n",[Pid]),
	    get_results(lists:filter(fun(Keeper) ->
					     Keeper /= Pid
				     end,Pids),Winners);
	{win,Score,Pid} ->
	    io:format("Pid ~p won, with a score of ~p~n",[Pid,Score]),
	    get_results(lists:filter(fun(Keeper) ->
					     Keeper /= Pid
				     end,Pids),[Pid|Winners])
        end.
    
-ifdef(EUNIT).

parse_line_test() ->
    [1,2,3,4] = parse_line("1,2,3,4\n").

parse_board_line_easy_test() ->
    [10,11,12] = parse_line("10 11 12\n").

parse_board_line_test() ->
    [1,2,3,4] = parse_line(" 1  2  3  4\n").

parse_board_test() ->
    {board,
     325,
     [
      [1,2,3,4,5],
      [6,7,8,9,10],
      [11,12,13,14,15],
      [16,17,18,19,20],
      [21,22,23,24,25],
      [1,6,11,16,21],
      [2,7,12,17,22],
      [3,8,13,18,23],
      [4,9,14,19,24],
      [5,10,15,20,25]]
    } = parse_board(" 1  2  3  4  5\n"," 6  7  8  9 10\n","11 12 13 14 15\n","16 17 18 19 20\n","21 22 23 24 25\n").

-endif.
