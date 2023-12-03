-module(day1).

-export([part1/1, part2/1, find_numbers_from_text1/1, read_file/1]).

part1(InputFile) ->
    List = case InputFile of
        example ->
            read_file("example2.txt");
        _ ->
            read_file(InputFile)
        end,
    Numbers = lists:foldl(
        fun(X, NrAccu) ->
            Numbers0 = lists:filtermap(
                    fun(Char) ->
                        lists:member(Char, "0123456789")
                    end,
                    X),
            case length(Numbers0) of
                0 ->
                    NrAccu;
                _ ->
                    Numbers1 = list_to_binary(Numbers0),
                    case string:length(Numbers1) of
                        1 ->
                                [binary_to_integer(<<Numbers1/binary, Numbers1/binary>>) | NrAccu];
                        2 ->
                                [binary_to_integer(Numbers1) | NrAccu];
                        _ ->
                            First = binary:part(Numbers1, {0,1}),
                            Last = binary:part(Numbers1, {byte_size(Numbers1),-1}),
                            [binary_to_integer(<<First/binary, Last/binary>>) | NrAccu]
                    end
            end
        end,
        [], List),
    lists:sum(Numbers).

part2(InputFile) ->
    List = case InputFile of
        example ->
            read_file("example2.txt");
        _ ->
            read_file(InputFile)
        end,
    Numbers = lists:foldl(
        fun(X, NrAccu) ->
            Numbers0 = find_numbers_from_text1(X),
            case length(Numbers0) of
                0 ->
                    NrAccu;
                _ ->
                    Numbers1 = list_to_binary(Numbers0),
                    case string:length(Numbers1) of
                        1 ->
                            [binary_to_integer(<<Numbers1/binary, Numbers1/binary>>) | NrAccu];
                        2 ->
                            [binary_to_integer(Numbers1) | NrAccu];
                        _ ->
                            First = binary:part(Numbers1, {0,1}),
                            Last = binary:part(Numbers1, {byte_size(Numbers1),-1}),
                            [binary_to_integer(<<First/binary, Last/binary>>) | NrAccu]
                    end
            end
        end,
        [], List),
    lists:sum(Numbers).

read_file(Filename) ->
    {ok, FileContent} = file:read_file(Filename),
    Lines = binary:split(FileContent, <<"\n">>, [global]),
    Strings = [binary_to_list(Line) || Line <- Lines],
    Strings.

find_numbers_from_text1(Input) ->
    List = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "1", "2", "3", "4", "5", "6", "7", "8", "9"],
    List1 = lists:map(
        fun(Word) ->
            Word1 = string:to_lower(Word),
            Input1 = string:to_lower(Input),
            {Word1, string:str(Input1, Word1), string:rstr(Input1, Word1)}
        end, List),
    NumberList = lists:filter(
        fun(X) ->
            case X of
                {_, 0, _} ->
                    false;
                _ ->
                    true
            end
        end,
        List1),
    Sorted = lists:keysort(2, NumberList),
    {{First, _},{Last, _}} = lists:foldl(
        fun(X, {{LowestValue, LowestPos}, {HighestValue, HighestPos}}) ->
            case X of
                {_, 0, _} ->
                    {{LowestValue, LowestPos}, {HighestValue, HighestPos}};
                {Value, First, Last} ->
                    {CurrentLowestValue, CurrentLowestPos} =
                    if
                        LowestPos == 0 ->
                            {only_digits(Value), First};
                        LowestPos > First ->
                            {only_digits(Value), First};
                        true ->
                            {LowestValue, LowestPos}
                        end,
                    {CurrentHighestValue, CurrentHighestPos} =
                    if
                        HighestPos < Last ->
                            {only_digits(Value), Last};
                        true ->
                            {HighestValue, HighestPos}
                    end,
                    {{CurrentLowestValue, CurrentLowestPos}, {CurrentHighestValue, CurrentHighestPos}}
            end
        end,
        {{"", 0},{"", 0}}, Sorted),
    First ++ Last.


only_digits(Input) ->
    OnlyNumbers1 = re:replace(Input, "one", "1", [global, {return, list}]),
    OnlyNumbers2 = re:replace(OnlyNumbers1, "two", "2", [global, {return, list}]),
    OnlyNumbers3 = re:replace(OnlyNumbers2, "three", "3", [global, {return, list}]),
    OnlyNumbers4 = re:replace(OnlyNumbers3, "four", "4", [global, {return, list}]),
    OnlyNumbers5 = re:replace(OnlyNumbers4, "five", "5", [global, {return, list}]),
    OnlyNumbers6 = re:replace(OnlyNumbers5, "six", "6", [global, {return, list}]),
    OnlyNumbers7 = re:replace(OnlyNumbers6, "seven", "7", [global, {return, list}]),
    OnlyNumbers8 = re:replace(OnlyNumbers7, "eight", "8", [global, {return, list}]),
    OnlyNumbers9 = re:replace(OnlyNumbers8, "nine", "9", [global, {return, list}]),
    OnlyNumbers9.