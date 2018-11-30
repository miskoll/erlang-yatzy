-module(yatzy_turn).
-export([start/0, roll/2, dice/1, stop/1, rolls_left/1]).
-spec start() -> {'ok', TurnPid::pid()}.

-spec roll(TurnPid::pid(), Keep::[1..6]) -> 'ok' | 'invalid_keepers' 
                                                 | 'finished'.
%% Once the player has selected which dice to keep roll the remaining 
%% dice unless they have already been rolled twice.

-spec dice(TurnPid::pid()) -> yatzy:roll().
%% Just the rolled dice as it stands at this point.

-spec stop(TurnPid::pid()) -> yatzy:roll().
%% Just stop the procees and get out what was rolled.

-spec rolls_left(TurnPid::pid()) -> yatzy:roll().
%% Returns the number of rolls remaining in the turn `TurnPid'.

start() ->
  NumDice = 5,
  TurnPid = spawn(fun() -> first_roll(randomize_dice(NumDice)) end),
  {ok, TurnPid}.

dice(Pid) ->
  call(Pid, {self(), dice}).

stop(Pid) ->
  call(Pid, {self(), stop}).

roll(Pid, Keep) ->
  call(Pid, {self(), {roll, Keep}}).

rolls_left(Pid) ->
  call(Pid, {self(), rolls_left}).

first_roll(Roll) ->
  RollsLeft = 2,
  receive
    {From, dice} ->
      From ! Roll,
      first_roll(Roll);
    {From, stop} ->
      From ! Roll;
    {From, rolls_left} ->
      From ! RollsLeft,
      first_roll(Roll);
    {From, {roll, Keep}} ->
      case is_valid_keep(Keep, Roll) of
        true -> 
          NewRoll = randomize_dice(5 - length(Keep)) ++ Keep,
          From ! {ok, NewRoll},
          second_roll(NewRoll);
        false -> 
          From ! invalid_keepers,
          first_roll(Roll)
      end
  end.

second_roll(Roll) ->
  RollsLeft = 1,
  receive
    {From, dice} ->
      From ! Roll,
      second_roll(Roll);
    {From, stop} ->
      From ! Roll;
    {From, rolls_left} ->
      From ! RollsLeft,
      second_roll(Roll);
    {From, {roll, Keep}} ->
      case is_valid_keep(Keep, Roll) of
        true ->
          NewRoll = randomize_dice(5 - length(Keep)) ++ Keep,
          From ! {ok, NewRoll},
          third_roll(NewRoll);
        false ->
          From ! invalid_keepers,
          second_roll(Roll)
      end
  end.

third_roll(Roll) ->
  RollsLeft = 0,
  receive
    {From, dice} ->
      From ! Roll,
      third_roll(Roll);
    {From, {roll, _Keep}} ->
      From ! finished,
      third_roll(Roll);
    {From, rolls_left} ->
      From ! RollsLeft,
      third_roll(Roll);
    {From, stop} ->
      From ! Roll
  end.

is_valid_keep(Keep, Roll) ->
  length(Keep -- Roll) == 0.

call(Pid, Msg) ->
  case is_process_alive(Pid) of
    true ->
      Pid ! Msg,
      receive
        Response ->
          Response
      end;
    false ->
      {error, dead_process}
  end.

randomize_dice(NumberOfDice) ->
  NumPossibleValues = 6,
  [rand:uniform(NumPossibleValues) || _ <- lists:seq(1,NumberOfDice)].
