-module(yatzy_sheet_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

setup()->
	S = yatzy_sheet:new(),
	{ok, S1} = yatzy_sheet:fill(ones, [1,1,2,2,3], S),
	{ok, S2} = yatzy_sheet:fill(twos, [1,2,2,2,3], S1),
	{ok, S3} = yatzy_sheet:fill(full_house, [3,2,2,2,3], S2),
	{ok, S4} = yatzy_sheet:fill(threes, [1,3,3,3,3], S3),
	{ok, S5} = yatzy_sheet:fill(fours, [1,4,4,4,4], S4),
	{ok, S6} = yatzy_sheet:fill(fives, [1,5,5,3,3], S5),
	{ok, S7} = yatzy_sheet:fill(sixes, [1,6,6,6,6], S6),
	{ok, S8} = yatzy_sheet:fill(one_pair, [2,2,1,3,4], S7),
	{ok, S9} = yatzy_sheet:fill(two_pairs, [2,2,4,4,1], S8),
	{ok, S10} = yatzy_sheet:fill(three_of_a_kind, [3,3,3,1,1], S9),
	{ok, S11} = yatzy_sheet:fill(four_of_a_kind, [5,5,5,5,1], S10),
	{ok, S12} = yatzy_sheet:fill(yatzy, [1,1,1,1,1], S11),
	{ok, S13} = yatzy_sheet:fill(small_straight, [1,2,3,4,5], S12),
	{ok, S14} = yatzy_sheet:fill(chance, [1,2,3,4,1], S13),
	{ok, S15} = yatzy_sheet:fill(large_straight, [2,3,4,5,6], S14),
	S15.

fill_test() ->
	Sheet = setup(),
	?assertEqual(already_filled, yatzy_sheet:fill(twos, [1,2,2,2,2], Sheet)),
	?assertEqual(already_filled, yatzy_sheet:fill(threes, [1,3,2,2,2], Sheet)),
	?assertEqual(already_filled, yatzy_sheet:fill(fours, [1,4,2,2,2], Sheet)),
	?assertEqual(already_filled, yatzy_sheet:fill(fives, [1,5,2,2,2], Sheet)),
	?assertEqual(already_filled, yatzy_sheet:fill(sixes, [1,6,2,2,2], Sheet)),
	?assertEqual(already_filled, yatzy_sheet:fill(twos, [1,2,2,2,2], Sheet)).


get_test() ->
	Sheet = setup(),
	?assertEqual({filled, 2}, yatzy_sheet:get(ones,Sheet)),
	?assertEqual({filled, 6}, yatzy_sheet:get(twos,Sheet)),
	?assertEqual({filled, 12}, yatzy_sheet:get(threes,Sheet)),
	?assertEqual({filled, 16}, yatzy_sheet:get(fours,Sheet)),
	?assertEqual({filled, 10}, yatzy_sheet:get(fives,Sheet)),
	?assertEqual({filled, 24}, yatzy_sheet:get(sixes,Sheet)),
	?assertEqual({filled, 4}, yatzy_sheet:get(one_pair,Sheet)),
	?assertEqual({filled, 12}, yatzy_sheet:get(two_pairs,Sheet)),
	?assertEqual({filled, 9}, yatzy_sheet:get(three_of_a_kind,Sheet)),
	?assertEqual({filled, 20}, yatzy_sheet:get(four_of_a_kind,Sheet)),
	?assertEqual({filled, 50}, yatzy_sheet:get(yatzy,Sheet)),
	?assertEqual({filled, 15}, yatzy_sheet:get(small_straight,Sheet)),
	?assertEqual({filled, 20}, yatzy_sheet:get(large_straight,Sheet)),
	?assertEqual({filled, 11}, yatzy_sheet:get(chance,Sheet)),
	?assertEqual({filled, 12}, yatzy_sheet:get(full_house,Sheet)).

upper_total_test()->
	Sheet = setup(),
	?assertEqual(70, yatzy_sheet:upper_total(Sheet)).

bonus_test()->
	Sheet = setup(),
	?assertEqual(50, yatzy_sheet:bonus(Sheet)).

lower_total_test()->
	Sheet = setup(),
	?assertEqual(153, yatzy_sheet:lower_total(Sheet)).

total_test()->
	Sheet = setup(),
	?assertEqual(273, yatzy_sheet:total(Sheet)).
