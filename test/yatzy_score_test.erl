-module(yatzy_score_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

upper_test_() ->
    [?_assertEqual(2, yatzy_score:calc(ones, [3,2,1,5,1])),
     ?_assertEqual(6, yatzy_score:calc(twos, [2,4,2,3,2])),
     ?_assertEqual(12, yatzy_score:calc(threes, [3,3,4,3,3])),
     ?_assertEqual(4, yatzy_score:calc(fours, [6,2,4,3,2])),
     ?_assertEqual(25, yatzy_score:calc(fives, [5,5,5,5,5])),
     ?_assertEqual(0, yatzy_score:calc(sixes, [1,2,5,4,3]))].

one_pair_test_() ->
    [?_assertEqual(10, yatzy_score:calc(one_pair, [5,6,4,3,5])),
     ?_assertEqual(0, yatzy_score:calc(one_pair, [1,2,3,4,6])),
     ?_assertEqual(12, yatzy_score:calc(one_pair, [5,5,6,6,5]))].


two_pairs_test_() ->
    [?_assertEqual(0, yatzy_score:calc(two_pairs, [1,3,4,2,3])),
     ?_assertEqual(14, yatzy_score:calc(two_pairs, [4,3,3,1,4])),
     ?_assertEqual(0, yatzy_score:calc(two_pairs, [5,5,3,5,5]))].

three_of_a_kind_test_() ->
    [?_assertEqual(0, yatzy_score:calc(three_of_a_kind, [5,5,3,2,4])),
     ?_assertEqual(15, yatzy_score:calc(three_of_a_kind, [3,5,4,5,5])),
     ?_assertEqual(18, yatzy_score:calc(three_of_a_kind, [6,6,3,6,6]))].

four_of_a_kind_test_() ->
    [?_assertEqual(0, yatzy_score:calc(four_of_a_kind, [6,6,6,4,5])),
     ?_assertEqual(20, yatzy_score:calc(four_of_a_kind, [5,5,5,3,5])),
     ?_assertEqual(12, yatzy_score:calc(four_of_a_kind, [3,3,3,3,3]))].

small_straight_test_() ->
    [?_assertEqual(0, yatzy_score:calc(small_straight, [2,3,4,5,6])),
     ?_assertEqual(15, yatzy_score:calc(small_straight, [1,4,3,2,5]))].


large_straight_test_() ->
    [?_assertEqual(20, yatzy_score:calc(large_straight, [2,3,4,5,6])),
     ?_assertEqual(0, yatzy_score:calc(large_straight, [1,4,3,2,5]))].

full_house_test_() ->
    [?_assertEqual(23, yatzy_score:calc(full_house, [4,5,5,5,4])),
     ?_assertEqual(0, yatzy_score:calc(full_house, [4,4,4,4,4]))].

chance_test_() ->
    [?_assertEqual(23, yatzy_score:calc(chance, [4,5,3,6,5])),
     ?_assertEqual(13, yatzy_score:calc(chance, [2,3,4,1,3]))].

yatzy_test_() ->
    [?_assertEqual(0, yatzy_score:calc(yatzy, [4,5,4,4,4])),
     ?_assertEqual(50, yatzy_score:calc(yatzy, [3,3,3,3,3]))].
