-module(deserialise).
-export([start/0]).

-record(opinfo, {mode, kMode, hasAux}).

-record(inst, {
    k,
    k0 = "",
    k1 = "",
    k2 = "",
    kc = 0,
    opcode = 0,
    kMode = 0,
    b = 0,
    c = 0,
    a = 0,
    d = 0,
    aux = 0,
    kn = false
}).

-record(proto, {
    dbgname = "",
    code = [],
    instLineInfo = [],
    protos = [],
    maxStackSize = 0,
    numParams = 0,
    nups = 0
}).

uint8(X) ->
    <<R:8>> = <<X:8>>,
    R.

int16(X) ->
    <<R:16/signed>> = <<X:16>>,
    R.

int32(X) ->
    <<R:32/signed>> = <<X:32>>,
    R.

uint32(X) ->
    <<R:32>> = <<X:32>>,
    R.

% // opList contains information about the instruction, each instruction is defined in this format:
% // { Name, Mode, KMode, HasAux }
% // Mode specifies what type of registers the instruction uses if any
% //		0 = NONE
% //		1 = A
% //		2 = AB
% //		3 = ABC
% //		4 = AD
% //		5 = AE
% // KMode specifies if the instruction has a register that holds a constant table index, which will be directly converted to the constant in the 2nd pass
% //		0 = NONE
% //		1 = AUX
% //		2 = C
% //		3 = D
% //		4 = AUX import
% //		5 = AUX boolean low 1 bit
% //		6 = AUX number low 24 bits
% // HasAux boolean specifies whether the instruction is followed up with an AUX word, which may be used to execute the instruction.

opinfo(Opcode) ->
    Opcodes = [
        % NOP
        #opinfo{mode = 0, kMode = 0, hasAux = false},
        % BREAK
        #opinfo{mode = 0, kMode = 0, hasAux = false},
        % LOADNIL
        #opinfo{mode = 1, kMode = 0, hasAux = false},
        % LOADB
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % LOADN
        #opinfo{mode = 4, kMode = 0, hasAux = false},
        % LOADK
        #opinfo{mode = 4, kMode = 3, hasAux = false},
        % MOVE
        #opinfo{mode = 2, kMode = 0, hasAux = false},
        % GETGLOBAL
        #opinfo{mode = 1, kMode = 1, hasAux = true},
        % SETGLOBAL
        #opinfo{mode = 1, kMode = 1, hasAux = true},
        % GETUPVAL
        #opinfo{mode = 2, kMode = 0, hasAux = false},
        % SETUPVAL
        #opinfo{mode = 2, kMode = 0, hasAux = false},
        % CLOSEUPVALS
        #opinfo{mode = 1, kMode = 0, hasAux = false},
        % GETIMPORT
        #opinfo{mode = 4, kMode = 4, hasAux = true},
        % GETTABLE
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % SETTABLE
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % GETTABLEKS
        #opinfo{mode = 3, kMode = 1, hasAux = true},
        % SETTABLEKS
        #opinfo{mode = 3, kMode = 1, hasAux = true},
        % GETTABLEN
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % SETTABLEN
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % NEWCLOSURE
        #opinfo{mode = 4, kMode = 0, hasAux = false},
        % NAMECALL
        #opinfo{mode = 3, kMode = 1, hasAux = true},
        % CALL
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % RETURN
        #opinfo{mode = 2, kMode = 0, hasAux = false},
        % JUMP
        #opinfo{mode = 4, kMode = 0, hasAux = false},
        % JUMPBACK
        #opinfo{mode = 4, kMode = 0, hasAux = false},
        % JUMPIF
        #opinfo{mode = 4, kMode = 0, hasAux = false},
        % JUMPIFNOT
        #opinfo{mode = 4, kMode = 0, hasAux = false},
        % JUMPIFEQ
        #opinfo{mode = 4, kMode = 0, hasAux = true},
        % JUMPIFLE
        #opinfo{mode = 4, kMode = 0, hasAux = true},
        % JUMPIFLT
        #opinfo{mode = 4, kMode = 0, hasAux = true},
        % JUMPIFNOTEQ
        #opinfo{mode = 4, kMode = 0, hasAux = true},
        % JUMPIFNOTLE
        #opinfo{mode = 4, kMode = 0, hasAux = true},
        % JUMPIFNOTLT
        #opinfo{mode = 4, kMode = 0, hasAux = true},
        % ADD
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % SUB
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % MUL
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % DIV
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % MOD
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % POW
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % ADDK
        #opinfo{mode = 3, kMode = 2, hasAux = false},
        % SUBK
        #opinfo{mode = 3, kMode = 2, hasAux = false},
        % MULK
        #opinfo{mode = 3, kMode = 2, hasAux = false},
        % DIVK
        #opinfo{mode = 3, kMode = 2, hasAux = false},
        % MODK
        #opinfo{mode = 3, kMode = 2, hasAux = false},
        % POWK
        #opinfo{mode = 3, kMode = 2, hasAux = false},
        % AND
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % OR
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % ANDK
        #opinfo{mode = 3, kMode = 2, hasAux = false},
        % ORK
        #opinfo{mode = 3, kMode = 2, hasAux = false},
        % CONCAT
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % NOT
        #opinfo{mode = 2, kMode = 0, hasAux = false},
        % MINUS
        #opinfo{mode = 2, kMode = 0, hasAux = false},
        % LENGTH
        #opinfo{mode = 2, kMode = 0, hasAux = false},
        % NEWTABLE
        #opinfo{mode = 2, kMode = 0, hasAux = true},
        % DUPTABLE
        #opinfo{mode = 4, kMode = 3, hasAux = false},
        % SETLIST
        #opinfo{mode = 3, kMode = 0, hasAux = true},
        % FORNPREP
        #opinfo{mode = 4, kMode = 0, hasAux = false},
        % FORNLOOP
        #opinfo{mode = 4, kMode = 0, hasAux = false},
        % FORGLOOP
        #opinfo{mode = 4, kMode = 8, hasAux = true},
        % FORGPREP_INEXT
        #opinfo{mode = 4, kMode = 0, hasAux = false},
        % FASTCALL3
        #opinfo{mode = 3, kMode = 1, hasAux = true},
        % FORGPREP_NEXT
        #opinfo{mode = 4, kMode = 0, hasAux = false},
        % FORGLOOP_NEXT (deprecated)
        #opinfo{mode = 0, kMode = 0, hasAux = false},
        % GETVARARGS
        #opinfo{mode = 2, kMode = 0, hasAux = false},
        % DUPCLOSURE
        #opinfo{mode = 4, kMode = 3, hasAux = false},
        % PREPVARARGS
        #opinfo{mode = 1, kMode = 0, hasAux = false},
        % LOADKX
        #opinfo{mode = 1, kMode = 1, hasAux = true},
        % JUMPX
        #opinfo{mode = 5, kMode = 0, hasAux = false},
        % FASTCALL
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % COVERAGE
        #opinfo{mode = 5, kMode = 0, hasAux = false},
        % CAPTURE
        #opinfo{mode = 2, kMode = 0, hasAux = false},
        % SUBRK
        #opinfo{mode = 3, kMode = 7, hasAux = false},
        % DIVRK
        #opinfo{mode = 3, kMode = 7, hasAux = false},
        % FASTCALL1
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % FASTCALL2
        #opinfo{mode = 3, kMode = 0, hasAux = true},
        % FASTCALL2K
        #opinfo{mode = 3, kMode = 1, hasAux = true},
        % FORGPREP
        #opinfo{mode = 4, kMode = 0, hasAux = false},
        % JUMPXEQKNIL
        #opinfo{mode = 4, kMode = 5, hasAux = true},
        % JUMPXEQKB
        #opinfo{mode = 4, kMode = 5, hasAux = true},
        % JUMPXEQKN
        #opinfo{mode = 4, kMode = 6, hasAux = true},
        % JUMPXEQKS
        #opinfo{mode = 4, kMode = 6, hasAux = true},
        % IDIV
        #opinfo{mode = 3, kMode = 0, hasAux = false},
        % IDIVK
        #opinfo{mode = 3, kMode = 2, hasAux = false}
    ],
    lists:nth(Opcode + 1, Opcodes).

rByte(<<B:8, Rest/binary>>) ->
    {B, Rest}.

rBool(<<B:8, Rest/binary>>) ->
    {B /= 0, Rest}.

rUint32(<<W:32/little, Rest/binary>>) ->
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
    % io:format("String size: ~p~n", [Size]),
    <<Str:Size/binary, Rest2/binary>> = Rest,
    {binary_to_list(Str), Rest2}.

% Erlang handles bitstrings really nicely, still getting the hang of looping with recursion tho
rStrings(Binary, 0, Strings) ->
    {Strings, Binary};
rStrings(Binary, Count, Strings) ->
    {Str, Rest} = rString(Binary),
    % io:format("String: ~p~n", [Str]),
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
    {Value, Rest1} = rUint32(Binary),

    % some1 tell me a better way stat
    Opcode = uint8(Value),
    Opinfo = opinfo(Opcode),

    {A, B, C, D} =
        case Opinfo#opinfo.mode of
            % other A lol
            5 ->
                At = int32(Value bsr 8),
                if
                    At >= 16#800_000 ->
                        {At - 16#1000_0000, 0, 0, 0};
                    true ->
                        {At, 0, 0, 0}
                end;
            % AD
            4 ->
                {uint8(Value bsr 8), 0, 0, int16(Value bsr 16)};
            % ABC
            3 ->
                {uint8(Value bsr 8), uint8(Value bsr 16), uint8(Value bsr 24), 0};
            % AB
            2 ->
                {uint8(Value bsr 8), uint8(Value bsr 16), 0, 0};
            % A
            1 ->
                {uint8(Value bsr 8), 0, 0, 0};
            0 ->
                {0, 0, 0, 0}
        end,
	% io:format("Opcode: ~p, A: ~p, B: ~p, C: ~p, D: ~p~n", [Opcode, A, B, C, D]),

    I = #inst{
        opcode = Opcode,
        kMode = Opinfo#opinfo.kMode,
        a = A,
        b = B,
        c = C,
        d = D
    },

    case Opinfo#opinfo.hasAux of
        true ->
            {Aux, Rest2} = rUint32(Rest1),
            {[I, #inst{aux = Aux}], true, Rest2};
        false ->
            {[I], false, Rest1}
    end.

readInsts(Binary, 0, Code) ->
    {Code, Binary};
readInsts(Binary, Sizecode, Code) ->
    case readInst(Binary) of
        {Insts, true, Rest} ->
			% io:format("Insts (aux):   ~p~n", [Insts]),
            readInsts(Rest, Sizecode - 2, Insts ++ Code);
        {Insts, false, Rest} ->
			% io:format("Insts (noaux): ~p~n", [Insts]),
            readInsts(Rest, Sizecode - 1, Insts ++ Code)
    end.

readInsts(Binary, Sizecode) ->
    readInsts(Binary, Sizecode, []).

getKs(Binary, Sizek) ->
	{1, Binary}.

readProto(Binary, StringList) ->
    {MaxStackSize, Rest1} = rByte(Binary),
    {NumParams, Rest2} = rByte(Rest1),
    {Nups, Rest3} = rByte(Rest2),

    <<_:16, Rest4/binary>> = Rest3,
    {Typesize, Rest5} = rVarInt(Rest4),
    <<_:Typesize/binary, Rest6/binary>> = Rest5,
    {Sizecode, Rest7} = rVarInt(Rest6),
	% rest7 looks good here

    {Code, Rest8} = readInsts(Rest7, Sizecode),

    {Sizek, Rest9} = rVarInt(Rest8),
    io:format("Rest: ~p~n", [Rest9]),

	{K, Rest10} = getKs(Rest9, Sizek),

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
    % io:format("String count: ~p~n", [StringCount]),

    {StringList, Rest3} = rStrings(Rest2, StringCount),
    % io:format("Strings: ~p~n", [StringList]),

    Rest4 = userdataTypeRemapping(Rest3),
    {ProtoCount, Rest5} = rVarInt(Rest4),
    io:format("Proto count: ~p~n", [ProtoCount]),

    {ProtoList, Rest6} = readProtos(Rest5, StringList, ProtoCount),

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
