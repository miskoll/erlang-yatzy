-module(yatzy_sheet).
-export([new/0, get/2, fill/3, upper_total/1, lower_total/1, total/1, bonus/1]).

-type t() :: map().
-spec new() -> t().
-spec fill(yatzy:slot(), yatzy:roll(), t()) -> {'ok', t()}
                       | 'already_filled'
                       | 'invalid_slot'.
-spec get(yatzy:slot(), t()) -> {'filled', non_neg_integer()}
                | 'invalid_slot'
                | 'empty'.
-spec upper_total(t()) -> non_neg_integer().
-spec bonus(t()) -> 0 | 50.
-spec lower_total(t()) -> non_neg_integer().
-spec total(t()) -> non_neg_integer().

new() ->
  maps:new().

fill(Slot, Roll, Sheet) -> 
  case is_valid_slot(Slot) of
    false -> 
      invalid_slot;
    true ->
      case is_already_filled(Slot, Sheet) of
        true -> 
          already_filled;
        false -> 
          {ok, Sheet#{Slot => yatzy_score:calc(Slot, Roll) }}
      end
  end.

is_already_filled(Slot, Sheet) -> 
  case maps:find(Slot, Sheet) of
    {ok,_} ->
      true;
    error ->
      false
  end.

get(Slot, Sheet) -> 
  case is_valid_slot(Slot) of
    false ->
      invalid_slot;
    true ->
      case maps:get(Slot, Sheet, empty) of
        empty -> 
          empty;
        Value -> 
          {filled, Value}
      end
  end.

is_valid_slot(Slot) ->
  ValidSlots = [ones, twos, threes, fours, fives, sixes, 
          one_pair, two_pairs, three_of_a_kind, four_of_a_kind, 
          small_straight, large_straight, full_house, chance, yatzy],
  lists:member(Slot, ValidSlots).

%-spec upper_total(t()) -> non_neg_integer().
upper_total(Sheet) -> 
  Uppers = [ones, twos, threes, fours, fives, sixes], % not drugs
  lists:sum(maps:values(maps:with(Uppers, Sheet))).
 
%-spec lower_total(t()) -> non_neg_integer().
lower_total(Sheet) ->
  Lowers = [one_pair, two_pairs, three_of_a_kind, four_of_a_kind,
        small_straight, large_straight, full_house, chance, yatzy],
  lists:sum(maps:values(maps:with(Lowers, Sheet))).

%-spec total(t()) -> non_neg_integer().
total(Sheet) -> 
  lists:sum(maps:values(Sheet)) + bonus(Sheet).

%-spec bonus(t()) -> 0 | 50.
bonus(Sheet) ->
  case upper_total(Sheet) >= 63 of
    true -> 
      50;
    false ->
      0
  end.
