% src/yatzy_player.erl
-module(yatzy_player).
-export([new/1, fill/3, sheet/1]).
-spec new(Name::atom()) -> {ok, pid()}.
-spec fill(Name::atom(), yatzy:slot(), yatzy:roll()) -> {ok, Score::integer()}
                            | {error, Reason::any()}.
-spec sheet(Name::atom()) -> yatzy_sheet:t().

new(Name) ->
  % Cannot do this in one line because register returns true, and we want
  % to capture the pid returned by spawn/2.

  % This creates a new process and tells it to run the loop/1 function.
  % loop/1 is passed the player's sheet, and takes care of all actions
  % on the sheet.
  Pid = spawn(fun() -> loop(yatzy_sheet:new()) end),
  % Register the process with the player's name `Name'
  register(Name, Pid),
  % Finally return tagged tuple with the newly created process's pid
  {ok, Pid}.

fill(Name, Slot, Roll) ->
  % This sends the process called `Name' a tuple with the pid of the
  % calling process (e.g. the erlang shell's pid), and another tuple
  % asking the process `Name' to process `fill', with args `Slot' and
  % `Roll'.  We send the calling process's pid so that the `Name' process
  % knows where to reply.
  Name ! {self(), {fill, Slot, Roll}},
  % After sending the message to `Name' (i.e. loop/1), fill/3 then waits
  % for a response, which it then returns.
  receive
    Msg ->
      Msg
  end.  

sheet(Name) ->
  % Here we ask the process called `Name' for the sheet, and then sheet/1
  % waits for a reply matching the pattern `Sheet', and returns same.
  Name ! {self(), sheet},
  receive
    Sheet ->
      Sheet
  end.

loop(Sheet) ->
  % loop/1 is the process called `Name'.  loop/1 just waits to receive
  % messages.
  receive
    {From, {fill, Slot, Roll}} ->
      case yatzy_sheet:fill(Slot, Roll, Sheet) of
        {ok, NewSheet} ->
          {filled, Score} = yatzy_sheet:get(Slot, NewSheet),
          From ! {ok, Score},
          loop(NewSheet);
        ErrorReason -> 
          From ! {error, ErrorReason},
          loop(Sheet)
      end;
    {From, sheet} ->
      From ! Sheet,
      loop(Sheet);
    die ->
      true
  end.
