-module(deserialise).
-export([start/0]).

rVarInt(Binary, I, R) ->
    <<V:8, Rest/binary>> = Binary,

    NewR = R bor (V band 2#0111_1111) bsl (I * 7),
    if
        V band 2#1000_0000 == 0 ->
            {Rest, I + 1, NewR};
        I == 3 ->
            {Rest, I + 1, NewR};
        true ->
            rVarInt(Rest, I + 1, NewR)
    end.

rVarInt(Binary) ->
    {Rest, _, R} = rVarInt(Binary, 0, 0),
    {R, Rest}.

rString(Binary) ->
    {Size, Rest} = rVarInt(Binary),
	io:format("String size: ~p~n", [Size]),
    <<Str:Size/binary, Rest2/binary>> = Rest,
    {binary_to_list(Str), Rest2}.

% Erlang handles bitstrings really nicely, still getting the hang of looping with recursion tho
rStrings(Binary, Count, Strings) ->
	if
		Count == 0 ->
			{lists:reverse(Strings), Binary};
		true ->
			{Str, Rest} = rString(Binary),
			io:format("String: ~p~n", [Str]),
			rStrings(Rest, Count - 1, [Str | Strings])
	end.

rStrings(Binary, Count) ->
	rStrings(Binary, Count, []).

read_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            Binary;
        {error, Reason} ->
            error(Reason)
    end.

main() ->
    FileContent = read_file("hello.bytecode"),

    <<LuauVersion:8, TypesVersion:8, Rest1/binary>> = FileContent,

    case LuauVersion of
        0 -> error("the provided bytecode is an error message");
        6 -> ok;
        _ -> error("the version of the provided bytecode is unsupported")
    end,
    io:format("Luau Version: ~p~n", [LuauVersion]),

    case TypesVersion of
        3 -> ok;
        _ -> error("the types version of the provided bytecode is unsupported")
    end,
    io:format("Types Version: ~p~n", [TypesVersion]),

    {StringCount, Rest2} = rVarInt(Rest1),
    io:format("String count: ~p~n", [StringCount]),

    {StringList, Rest3} = rStrings(Rest2, StringCount),
    io:format("Strings: ~p~n", [StringList]),

    io:format("Rest: ~p~n", [Rest3]),

    io:format("Deserialisation completed~n").

start() ->
    try
        main()
    catch
        error:Reason:Stack ->
            io:format("Error: ~p~nStack: ~p~n", [Reason, Stack])
    end,
    io:format("~n"),
    init:stop().
