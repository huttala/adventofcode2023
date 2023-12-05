-module(day2).
-export([part1/1, part2/1]).

part1(File) ->
    Games = get_formatted_game_data(File),
    get_valid_games(Games).

part2(File) ->
    Games = get_formatted_game_data(File),
    get_power_of_games(Games).

-spec get_formatted_game_data(File :: binary()) -> [{integer(), [[{}]]}].
get_formatted_game_data(File) ->
    {ok, FileContent} = file:read_file(File),
    Lines = binary:split(FileContent, <<"\n">>, [global]),
    {Games, _} = lists:foldl(
        fun(X, {Accu, Count}) ->
            ColorsAndValues = separate_colors(remove_game_text(X)),
            case is_list(ColorsAndValues) of
                true ->
                    {[{Count, ColorsAndValues} | Accu], Count + 1};
                _ ->
                    Accu
            end
        end,
        {[], 1},
        Lines
    ),
    Games.

get_power_of_games(Games) ->
    Result = lists:foldl(
        fun({_GameNr, GameSets}, SumOfGames) ->
            {GreenInt, BlueInt, RedInt} = lists:foldl(
                fun(Y, {CurrentGreenInt, CurrentBlueInt, CurrentRedInt}) ->
                    {get_highest_int(CurrentGreenInt, get_int(<<"green">>, Y)),
                     get_highest_int(CurrentBlueInt, get_int(<<"blue">>, Y)),
                     get_highest_int(CurrentRedInt, get_int(<<"red">>, Y))}
                end,
                {1, 1, 1},
                GameSets),
            GameResult = GreenInt * BlueInt * RedInt,
            SumOfGames + GameResult
        end,
        0, Games),
    Result.

get_valid_games(Games) ->
    MaxBlue = 14,
    MaxGreen = 13,
    MaxRed = 12,
    ValidGames = lists:foldl(
        fun({GameNr, List}, TotalNrOfValidGames) ->
            OkList = lists:filter(
                fun(Y) ->
                    GreenInt = get_int(<<"green">>, Y),
                    BlueInt = get_int(<<"blue">>, Y),
                    RedInt = get_int(<<"red">>, Y),
                    BlueOk = BlueInt < MaxBlue orelse BlueInt == MaxBlue,
                    GreenOk = GreenInt < MaxGreen orelse GreenInt == MaxGreen,
                    RedOk = RedInt < MaxRed orelse RedInt == MaxRed,
                    case {BlueOk, GreenOk, RedOk} of
                        {true, true, true} ->
                            true;
                        _ ->
                            false
                    end
            end, List),
            case length(List) == length(OkList) of
                true ->
                    TotalNrOfValidGames + GameNr;
                false ->
                    TotalNrOfValidGames
            end
        end,
        0, Games),
    ValidGames.

-spec get_int(Color :: binary(), Data :: map()) -> integer().
get_int(Color, Data) ->
    case lists:keyfind(Color, 1, Data) of
        false ->
            0;
        {_, GreenValue0} ->
            list_to_integer(GreenValue0)
    end.

-spec get_highest_int(Current :: integer(), New :: integer()) -> integer().
get_highest_int(Current, New) ->
    CurrentIsHigher = Current > New,
    case CurrentIsHigher of
        true ->
            Current;
        false ->
            New
    end.
remove_game_text(Game) ->
    {Position, _Length} = binary:match(Game, <<":">>),
    SpaceAndColonRemovedPosition = Position +2,
    GameStringLength = byte_size(Game) - SpaceAndColonRemovedPosition,
    Extracted = binary:part(Game, SpaceAndColonRemovedPosition, GameStringLength),
    Extracted.

separate_colors(Game) ->
    GameSplits = binary:split(Game, <<";">>, [global]),
    ColorValues = lists:foldl(
        fun(X, Acc) ->
            Colors = binary:split(X, <<",">>, [global]),
            ColorValues = lists:foldl(
                fun(Color, Acc1) ->
                    [extract_color_and_value(Color) | Acc1]
                end,
                [], Colors),
            [ColorValues | Acc]
        end,
        [],
        GameSplits),
    ColorValues.

extract_color_and_value(Input) ->
    Colors = [<<"green">>, <<"blue">>, <<"red">>],
    lists:foldl(
        fun(Color, Acc) ->
            case string:find(Input, Color) of
                nomatch ->
                    Acc;
                _ ->
                    {Color, get_digits(Input)}
            end
        end,
        [], Colors).

get_digits(Input) ->
     InputString = binary_to_list(Input),
     lists:filtermap(
        fun(Char) ->
            lists:member(Char, "0123456789")
        end,
        InputString).
