-module(yatzy_turn).
-export([roll/1,
         dice/0,
         stop/0,
         rolls_left/0]).
-behavior(gen_statem).
-export([start_link/0,
         init/1,
         callback_mode/0
        ]).

%% States
-export([second_roll/3,
         third_roll/3,
         end_state/3
        ]).


%% SPEC -------------------------------------------
-spec start_link() -> {'ok', TurnPid::pid()}.

-spec roll(Keep::[1..6]) -> 'ok' | 'invalid_keepers'
                                 | 'finished'.
%% Once the player has selected which dice to keep roll the remaining
%% dice unless they have already been rolled twice.

-spec dice() -> yatzy:roll().
%% Just the rolled dice as it stands at this point.

-spec stop() -> yatzy:roll().
%% Just stop the procees and get out what was rolled.

-spec rolls_left() -> yatzy:roll().
%% Returns the number of rolls remaining in the turn `TurnPid'.

start_link() ->
    %% start_link(name, module_containing_callback_functions, args, options)
    gen_statem:start_link({local, ?MODULE}, ?MODULE, null, []).

init(null) ->
    NumDice = 5,
    FirstRoll = randomize_dice(NumDice),
    {ok, second_roll, FirstRoll}.

callback_mode() ->
    state_functions.

dice() ->
    gen_statem:call(?MODULE, dice).

stop() ->
    gen_statem:call(?MODULE, stop).

roll(Keep) ->
    gen_statem:call(?MODULE, {roll, Keep}).

rolls_left() ->
    gen_statem:call(?MODULE, rolls_left).

%% FIRST ROLL =====================================
%% dice
first_roll({call, From}, dice, Roll) ->
    {keep_state_and_data, [{reply, From, Roll}]};
%% {roll, Keep}
first_roll({call, From}, {roll, Keep}, Roll) ->
    case is_valid_keep(Keep, Roll) of
        true ->
            NewRoll = randomize_dice(5 - length(Keep)) ++ Keep,
            {next_state, second_roll, NewRoll, [{reply, From, NewRoll}]};
        false ->
            {keep_state_and_data, [{reply, From, invalid_keepers}]}
    end;
%% rolls_left
first_roll({call, From}, rolls_left, _Roll) ->
    RollsLeft = 2,
    {keep_state_and_data, [{reply, From, RollsLeft}]};
%% stop
first_roll({call, From}, stop, Roll) ->
    {stop_and_reply, normal, {reply, From, Roll}}.

%% SECOND ROLL =====================================
%% dice
second_roll({call, From}, dice, Roll) ->
    {keep_state_and_data, [{reply, From, Roll}]};
%% {roll, Keep}
second_roll({call, From}, {roll, Keep}, Roll) ->
    case is_valid_keep(Keep, Roll) of
        true ->
            NewRoll = randomize_dice(5 - length(Keep)) ++ Keep,
            {next_state, third_roll, NewRoll, {reply, From, NewRoll}};
        false ->
            {keep_state_and_data, [{reply, From, invalid_keepers}]}
    end;
%% rolls_left
second_roll({call, From}, rolls_left, _Roll) ->
    RollsLeft = 1,
    {keep_state_and_data, [{reply, From, RollsLeft}]};
%% stop
second_roll({call, From}, stop, Roll) ->
    {stop_and_reply, normal, {reply, From, Roll}}.

%% THIRD ROLL =======================================
%% dice
third_roll({call, From}, dice, Roll) ->
    {keep_state_and_data, [{reply, From, Roll}]};
%% {roll, Keep}
third_roll({call, From}, {roll, _Keep}, _Roll) ->
    {keep_state_and_data, [{reply, From, finished}]};
%% rolls_left
third_roll({call, From}, rolls_left, _Roll) ->
    RollsLeft = 0,
    {keep_state_and_data, [{reply, From, RollsLeft}]};
%% stop
third_roll({call, From}, stop, Roll) ->
    {stop_and_reply, normal, {reply, From, Roll}}.

is_valid_keep(Keep, Roll) ->
    length(Keep -- Roll) == 0.

randomize_dice(NumberOfDice) ->
    NumPossibleValues = 6,
    [rand:uniform(NumPossibleValues) || _ <- lists:seq(1,NumberOfDice)].
