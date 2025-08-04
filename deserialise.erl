-module(deserialise).
-export([start/0]).

rByte(<<B:8, Rest/binary>>) ->
    {B, Rest}.

rBool(<<B:8, Rest/binary>>) ->
    {B /= 0, Rest}.

rUint32(<<W:32, Rest/binary>>) ->
    {W, Rest}.

skipUint32(<<_:32, Rest/binary>>) ->
    Rest.

rVarInt(Binary, I, R) ->
    <<V:8, Rest/binary>> = Binary,

    NewR = R bor (V band 2#0111_1111) bsl (I * 7),
    if
        V band 2#1000_0000 == 0; I == 3 ->
            {Rest, I + 1, NewR};
        true ->
            rVarInt(Rest, I + 1, NewR)
    end.

rVarInt(Binary) ->
    {Rest, _, R} = rVarInt(Binary, 0, 0),
    {R, Rest}.

skipVarInt(<<V:8, Rest/binary>>, I) ->
    if
        V band 2#1000_0000 == 0; I == 3 ->
            {Rest, I + 1};
        true ->
            skipVarInt(Rest, I + 1)
    end.

skipVarInt(Binary) ->
    {Rest, _} = skipVarInt(Binary, 0),
    Rest.

rString(Binary) ->
    {Size, Rest} = rVarInt(Binary),
    io:format("String size: ~p~n", [Size]),
    <<Str:Size/binary, Rest2/binary>> = Rest,
    {binary_to_list(Str), Rest2}.

% Erlang handles bitstrings really nicely, still getting the hang of looping with recursion tho
rStrings(Binary, 0, Strings) ->
    {Strings, Binary};
rStrings(Binary, Count, Strings) ->
    {Str, Rest} = rString(Binary),
    io:format("String: ~p~n", [Str]),
    rStrings(Rest, Count - 1, Strings ++ [Str]).

rStrings(Binary, Count) ->
    rStrings(Binary, Count, []).

userdataTypeRemapping(Binary) ->
    case rBool(Binary) of
        {true, Rest} ->
            userdataTypeRemapping(skipVarInt(Rest));
        {false, Rest} ->
            Rest
    end.

readInst(Binary) ->
    {Value, Rest} = rUint32(Binary),
    {[], false, Rest}.

readInsts(Binary, 0, Code) ->
    {Code, Binary};
readInsts(Binary, Sizecode, Code) ->
    case readInst(Binary) of
        {Insts, true, Rest} ->
            readInsts(Rest, Sizecode - 2, Insts ++ Code);
        {Insts, false, Rest} ->
            readInsts(Rest, Sizecode - 1, Insts ++ Code)
    end.

readInsts(Binary, Sizecode) ->
    readInsts(Binary, Sizecode, []).

readProto(Binary, StringList) ->
    {MaxStackSize, Rest1} = rByte(Binary),
    {NumParams, Rest2} = rByte(Rest1),
    {Nups, Rest3} = rByte(Rest2),

    <<_:16, Rest4/binary>> = Rest3,
    {Typesize, Rest5} = rVarInt(Rest4),
    <<_:Typesize/binary, Rest6/binary>> = Rest5,
    {Sizecode, Rest7} = rVarInt(Rest6),

    {Code, Rest8} = readInsts(Rest7, Sizecode),

    {1, Rest8}.

readProtos(Binary, _, 0, Protos) ->
    {Protos, Binary};
readProtos(Binary, StringList, Count, Protos) ->
    {Proto, Rest} = readProto(Binary, StringList),
    io:format("Proto: ~p~n", [Proto]),
    readProtos(Rest, StringList, Count - 1, Protos ++ [Proto]).

readProtos(Binary, StringList, Count) ->
    readProtos(Binary, StringList, Count, []).

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

    Rest4 = userdataTypeRemapping(Rest3),
    {ProtoCount, Rest5} = rVarInt(Rest4),
    io:format("Proto count: ~p~n", [ProtoCount]),

    {ProtoList, Rest6} = readProtos(Rest5, StringList, ProtoCount),
    io:format("Rest: ~p~n", [Rest6]),

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
