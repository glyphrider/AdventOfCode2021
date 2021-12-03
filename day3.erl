-module(day3).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([day3/0,day3b/0]).

-define(BINARY_DATA_FILE_NAME,"day3.txt").

gamma(L) ->
    Half = length(L) / 2,
    [FirstElement | _ ] = L,
    Length = length(FirstElement),

    lists:map(
      fun(Count) when Count > Half ->
	      1;
	 (_Count) ->
	      0
      end,
      lists:foldl(
	fun(E,A) ->
		lists:zipwith(
		  fun(D,AD) ->
			  D+AD
		  end,E,A)
	end, zero_list(Length), L)).

zero_list(Length) ->
    zero_list(Length,[]).

zero_list(0,List) ->
    List;
zero_list(Length,List) ->
    zero_list(Length-1,[0|List]).

epsilon(Gamma) ->
    lists:map(
      fun(0) ->
	      1;
	 (1) ->
	      0
      end,Gamma).

convert_binary(L) ->
    {B,_} = lists:foldr(fun(D,{A,M}) ->
				{A+D*M,M*2}
			end,{0,1},L),
    B.

load_binary_data(Filename) ->
    {ok,IoDevice} = file:open(Filename,read),
    load_binary_data(file:read_line(IoDevice),IoDevice,[]).

load_binary_data(eof,_IoDevice,List) ->
    lists:reverse(List);
load_binary_data({ok,Line},IoDevice,List) ->
    load_binary_data(
      file:read_line(IoDevice),
      IoDevice,
      [
       lists:foldr(fun($0,A) ->
			   [0|A];
		      ($1,A) ->
			   [1|A];
		      (_,A) -> A
		   end,[],Line)
      |List
      ]).

o2gen_rating(BinaryData) ->
    o2gen_rating(BinaryData,1).

o2gen_rating([Result],_) ->
    Result;
o2gen_rating(BinaryData,Digit) ->
    o2gen_rating(o2gen_picker(split_list(BinaryData,Digit)),Digit+1).

o2gen_picker({Zero,One}) ->
    o2gen_picker(Zero,One,length(Zero),length(One)).
o2gen_picker(Zero,_One,LenZero,LenOne) when LenZero > LenOne ->
    Zero;
o2gen_picker(_Zero,One,_LenZero,_LenOne) ->
    One.

co2_scrubber_rating(BinaryData) ->
    co2_scrubber_rating(BinaryData,1).

co2_scrubber_rating([Result],_) ->
    Result;
co2_scrubber_rating(BinaryData,Digit) ->
    co2_scrubber_rating(co2_scrubber_picker(split_list(BinaryData,Digit)),Digit+1).

co2_scrubber_picker({Zero,One}) ->
    co2_scrubber_picker(Zero,One,length(Zero),length(One)).
co2_scrubber_picker(_Zero,One,LenZero,LenOne) when LenZero >  LenOne ->
    One;
co2_scrubber_picker(Zero,_One,_LenZero,_LenOne) ->
    Zero.

split_list(BinaryData,Digit) ->
    lists:foldl(                                                                                                                               
      fun(E,{Zero,One}) ->                                                                                                                     
	      case lists:nth(Digit,E) of                                                                                                       
		  1 ->                                                                                                                         
		      {Zero,[E|One]};                                                                                                          
		  0 ->                                                                                                                         
		      {[E|Zero],One}                                                                                                           
	      end                                                                                                                              
      end,                                                                                                                                     
      {[],[]},                                                                                                                                 
      BinaryData).

day3() ->
    day3(load_binary_data(?BINARY_DATA_FILE_NAME)).

day3(BinaryData) ->
    Gamma = gamma(BinaryData),
    Epsilon = epsilon(Gamma),
    Result = convert_binary(Gamma) * convert_binary(Epsilon),
    io:format("gamma ~p * epsilon ~p == ~p~n",[Gamma,Epsilon,Result]),
    Result.

day3b() ->
    day3b(load_binary_data(?BINARY_DATA_FILE_NAME)).

day3b(BinaryData) ->
    O2Gen = o2gen_rating(BinaryData),
    CO2Scrub = co2_scrubber_rating(BinaryData),
    Result = convert_binary(O2Gen) * convert_binary(CO2Scrub),
    io:format("oxygen generator rating ~p * co2 scrubber rating ~p == ~p~n",[O2Gen,CO2Scrub,Result]),
    Result.
    
-ifdef(EUNIT).
-include("day3.hrl").

gamma_0_test() ->
    [0,0,0,0,0] = gamma([[0,0,0,0,0]]).

gamma_1_test() ->
    [1,0,0,1,0] = gamma([[1,0,0,0,0],[1,0,0,1,1],[0,1,0,1,0]]).

epsilon_test() ->
    [0,1,1,0,1] = epsilon(gamma([[1,0,0,0,0],[1,0,0,1,1],[0,1,0,1,0]])).

zipwith_test() ->
    [1,0,0,0,0] = lists:zipwith(
		    fun(A,B) ->
			    A+B
		    end,
		    [0,0,0,0,0],
		    [1,0,0,0,0]).

o2gen_rating_0_test() ->
    [1,0] = o2gen_rating([[1,0],[0,0]]).

o2gen_rating_1_test() ->
    [1,0] = o2gen_rating([[0,1],[1,0]]).

o2gen_rating_2_test() ->
    [1,1] = o2gen_rating([[0,1],[1,0],[1,1]]).

co2_scrubber_rating_0_test() ->
    [0,0] = co2_scrubber_rating([[1,0],[0,0]]).

data_load_test() ->
    ?BINARY_DATA = load_binary_data(?BINARY_DATA_FILE_NAME).

-endif.
