%% src/yatzy_player.erl
-module(yatzy_player).
-export([fill/3, sheet/1, init/1]).
-export([handle_call/3, handle_cast/2, start_link/1, stop/1]).
-spec start_link(Name::atom()) -> {ok, pid()}.
-spec fill(Name::atom(), yatzy:slot(), yatzy:roll()) -> {ok, Score::integer()}
                                                            | {error, Reason::any()}.
-spec sheet(Name::atom()) -> yatzy_sheet:t().
-behavior(gen_server).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, null, []).

init(null) ->
    Sheet = yatzy_sheet:new(),
    {ok, Sheet}.

handle_cast(stop, Sheet) ->
    {stop, normal, Sheet}.

handle_call(sheet, _From, Sheet) ->
    {reply, Sheet, Sheet};
handle_call({fill, Slot, Roll}, _From, Sheet) ->
    case yatzy_sheet:fill(Slot, Roll, Sheet) of
        {ok, NewSheet} ->
            {filled, Score} = yatzy_sheet:get(Slot, NewSheet),
            {reply, {ok, Score}, NewSheet};
        ErrorReason ->
            {reply, {error, ErrorReason}, Sheet}
    end.

stop(Name) ->
    gen_server:cast(Name, stop).

fill(Name, Slot, Roll) ->
    gen_server:call(Name, {fill, Slot, Roll}).

sheet(Name) ->
    gen_server:call(Name, sheet).
