-module(yatzy_score).
-export([calc/2]).
-spec calc(yatzy:slot(), yatzy:roll()) -> non_neg_integer().

calc(Slot, Roll) ->
  case Slot of 
    ones ->
      score_upper(1, Roll);
    twos ->
      score_upper(2, Roll);
    threes -> 
      score_upper(3, Roll);
    fours ->
      score_upper(4, Roll);
    fives -> 
      score_upper(5, Roll);
    sixes ->
      score_upper(6, Roll);
    one_pair ->
      score_one_pair(Roll);
    two_pairs ->
      score_two_pair(Roll);     
    three_of_a_kind ->
      score_three_of_a_kind(Roll);
    four_of_a_kind ->
      score_four_of_a_kind(Roll);
    small_straight -> 
      score_small_straight(Roll);  
    large_straight ->
      score_large_straight(Roll);
    full_house ->
      score_full_house(Roll);
    chance ->
      score_chance(Roll);
    yatzy ->
      score_yatzy(Roll)
  end.
    
score_one_pair(Roll) ->
  case lists:reverse(lists:sort(Roll)) of 
    [X,X,_,_,_] -> 
      X + X;
    [_,X,X,_,_] ->
      X + X;
    [_,_,X,X,_] ->
      X + X;
    [_,_,_,X,X] ->
      X + X;
    _ ->
      0
  end.

score_two_pair(Roll) ->
  case lists:sort(Roll) of 
    [X,X,X,X,_] ->
      0;
    [_,X,X,X,X] ->
      0;
    [X,X,_,X,X] ->
      0;
    [X,X,Y,Y,_] ->
      X + X + Y + Y;
    [X,X,_,Y,Y] ->
      X + X + Y + Y;
    [_,X,X,Y,Y] ->
      X + X + Y + Y;
    _ ->
      0
  end.

score_three_of_a_kind(Roll) ->
  case lists:sort(Roll) of
    [X,X,X,_,_] -> 
      X * 3;
    [_,X,X,X,_] ->
      X * 3;
    [_,_,X,X,X] ->
      X * 3;
    _ ->
      0
  end.

score_four_of_a_kind(Roll) ->
   case lists:sort(Roll) of
    [X,X,X,X,_] ->
      X * 4;
    [_,X,X,X,X] ->
      X * 4;
    _ ->
      0
  end.

score_small_straight(Roll) ->
  case lists:sort(Roll) of 
    [1,2,3,4,5] ->
      lists:sum(Roll);
    _ ->
      0
  end.

score_large_straight(Roll) -> 
  case lists:sort(Roll) of
    [2,3,4,5,6] ->
      lists:sum(Roll);
    _ -> 
      0
  end.

score_full_house(Roll) ->
   case lists:sort(Roll) of 
    [X,X,X,X,X] ->
      0;
    [X,X,Y,Y,Y] -> 
      (X * 2) + (Y * 3);
    [X,X,X,Y,Y] ->
      (X * 3) + (Y * 2);
    _ ->
      0
  end.

score_yatzy(Roll) ->
  case Roll of
    [X,X,X,X,X] ->
      50;
    _ ->
      0
  end.

score_chance(Roll) ->
  lists:sum(Roll).

is_match(X, Y) ->
  X =:= Y.

score_upper(N, Roll) ->
  lists:sum(lists:filter(fun(R) -> is_match(R, N) end, Roll)).
