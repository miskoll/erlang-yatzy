-module(yatzy_score).
-export([calc/2]).
-spec calc(yatzy:slot(), yatzy:roll()) -> non_neg_integer().

calc(chance, Roll) ->
    lists:sum(Roll);
calc(ones, Roll) ->
    score_upper(1, Roll);
calc(twos, Roll) ->
    score_upper(2, Roll);
calc(threes, Roll) ->
    score_upper(3, Roll);
calc(fours, Roll) ->
    score_upper(4, Roll);
calc(fives, Roll) ->
    score_upper(5, Roll);
calc(sixes, Roll) ->
    score_upper(6, Roll);
calc(one_pair, Roll) ->
    score_one_pair(lists:reverse(lists:sort(Roll)));
calc(two_pair, Roll) -> 
    score_two_pair(lists:sort(Roll));
calc(three_of_a_kind, Roll) ->
   score_three_of_a_kind(lists:sort(Roll));
calc(four_of_a_kind, Roll) ->
    score_four_of_a_kind(lists:sort(Roll));
calc(small_straight, Roll) ->
    score_small_straight(Roll);
calc(large_straight, Roll) ->
    score_large_straight(Roll);
calc(full_house, Roll) ->
    score_full_house(Roll);
calc(chance, Roll) ->
    score_chance(Roll).

score_one_pair([X,X|_]) ->
    X + X;
score_one_pair([_,X,X|_]) ->
    X + X;
score_one_pair([_,_,X,X|_]) ->
    X + X;
score_one_pair([_,_,_X,X]) ->
    X + X.

score_two_pair([X,X,Y,Y,_]) ->
    X + X + Y + Y;
score_two_pair([X,X,_,Y,Y]) ->
    X + X + Y + Y;
score_two_pair([_,X,X,Y,Y]) ->
    X + X + Y + Y.

score_three_of_a_kind([X,X,X,_,_]) -> 
    X * 3;
score_three_of_a_kind([_,X,X,X,_]) ->
    X * 3;
score_three_of_a_kind([_,_,X,X,X]) ->
    X * 3.

score_four_of_a_kind([X,X,X,X,_]) ->
    X * 4;
score_four_of_a_kind([_,X,X,X,X]) ->
    X * 4.

score_small_straight(Roll) when Roll == [1,2,3,4,5] -> lists:sum(Roll).

score_large_straight(Roll) when Roll == [2,3,4,5,6] -> lists:sum(Roll).

score_full_house([X,X,Y,Y,Y]) -> 
    (X * 2) + (Y * 3);
score_full_house([X,X,X,Y,Y]) ->
    (X * 3) + (Y * 2).

score_yatzy([X,X,X,X,X]) ->
    50.

score_chance(Roll) ->
    lists:sum(Roll).

is_match(X, Y) ->
    X =:= Y.

score_upper(N, Roll) ->
    lists:sum(lists:filter(fun(R) -> is_match(R, N) end, Roll)).
