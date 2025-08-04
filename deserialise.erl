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

-record(vector, {x = 0, y = 0, z = 0, w = 0}).

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

checkkmode(I, K) ->
    Aux = I#inst.aux,
    % io:format("Aux: ~p KMode: ~p~n", [Aux, I#inst.kMode]),
    case I#inst.kMode of
        0 ->
            I;
        % AUX
        1 ->
            if
                Aux < length(K) ->
                    I#inst{k = lists:nth(Aux - 1, K)};
                true ->
                    I
            end;
        % C
        2 ->
            I#inst{k = lists:nth(I#inst.c - 1, K)};
        % D
        3 ->
            I#inst{k = lists:nth(I#inst.d - 1, K)};
        % AUX import
        4 ->
            Count = uint8(Aux bsr 30),

            Id0 = Aux bsr 20 band 16#3ff,
            % io:format("AUX: ~p~n", [Id0]),
            K0 = lists:nth(Id0 + 1, K),
            % io:format("K0: ~p~n", [I3#inst.k0]),

            if
                Count < 2 ->
                    I#inst{kc = Count, k0 = K0};
                true ->
                    Id1 = Aux bsr 10 band 16#3ff,
                    K1 = lists:nth(Id1 + 1, K),

                    if
                        Count < 3 ->
                            I#inst{kc = Count, k0 = K0, k1 = K1};
                        true ->
                            Id2 = Aux band 16#3ff,
                            K2 = lists:nth(Id2 + 1, K),

                            I#inst{kc = Count, k0 = K0, k1 = K1, k2 = K2}
                    end
            end;
        % AUX boolean low 1 bit
        5 ->
            I#inst{
                k = Aux band 1 == 1,
                kn = Aux bsr 31 == 1
            };
        % AUX boolean low 24 bits
        6 ->
            I#inst{
                k = Aux band (1 bsl 24 - 1),
                kn = Aux bsr 31 == 1
            };
        % B
        7 ->
            I#inst{k = lists:nth(I#inst.b - 1, K)};
        % AUX number low 16 bits ig
        8 ->
            % forgloop
            I#inst{k = Aux band 16}
    end.

rByte(<<B:8, Rest/binary>>) ->
    {B, Rest}.

skipByte(<<_:8, Rest/binary>>) ->
    Rest.

rBool(<<B:8, Rest/binary>>) ->
    {B /= 0, Rest}.

% yes the endianness matters here
rUint32(<<W:32/little, Rest/binary>>) ->
    {W, Rest}.

skipUint32(<<_:32, Rest/binary>>) ->
    Rest.

rVector(<<X:32/float, Y:32/float, Z:32/float, W:32/float, Rest/binary>>) ->
    {#vector{x = X, y = Y, z = Z, w = W}, Rest}.

rFloat64(<<F:64/float, Rest/binary>>) ->
    {F, Rest}.

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

skipTable(Binary, 0) ->
    Binary;
skipTable(Binary, Size) ->
    Rest = skipVarInt(Binary),
    skipTable(Rest, Size - 1).

skipTable(Binary) ->
    {Size, Rest} = rVarInt(Binary),
    skipTable(Rest, Size).

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
            readInsts(Rest, Sizecode - 2, Code ++ Insts);
        {Insts, false, Rest} ->
            % io:format("Insts (noaux): ~p~n", [Insts]),
            readInsts(Rest, Sizecode - 1, Code ++ Insts)
    end.

readInsts(Binary, Sizecode) ->
    readInsts(Binary, Sizecode, []).

% Nil
getK(<<0, Rest/binary>>, _) ->
    % yeah
    {undefined, Rest};
% Bool
getK(<<1, Rest/binary>>, _) ->
    rBool(Rest);
% Number
getK(<<2, Rest/binary>>, _) ->
    rFloat64(Rest);
% String
getK(<<3, Rest1/binary>>, StringList) ->
    {V, Rest2} = rVarInt(Rest1),
    % io:format("V: ~p StringList: ~p~n", [V, StringList]),
    {lists:nth(V, StringList), Rest2};
% Import
getK(<<4, Rest/binary>>, _) ->
    % only used with useImportConstants
    {undefined, skipUint32(Rest)};
% Table
getK(<<5, Rest1/binary>>, _) ->
    % moot, whatever
    {undefined, skipTable(Rest1)};
% Closure
getK(<<6, Rest/binary>>, _) ->
    % pain in the cranium
    % ⚠️ not a val ⚠️
    rVarInt(Rest);
getK(<<7, Rest/binary>>, _) ->
    rVector(Rest).

getKs(Binary, _, 0, Ks) ->
    {Ks, Binary};
getKs(Binary, StringList, Sizek, Ks) ->
    case getK(Binary, StringList) of
        {undefined, Rest} ->
            getKs(Rest, StringList, Sizek - 1, Ks);
        {K, Rest} ->
            getKs(Rest, StringList, Sizek - 1, Ks ++ [K])
    end.

getKs(Binary, StringList, Sizek) ->
    getKs(Binary, StringList, Sizek, []).

getProtoIds(Binary, 0, ProtoIds) ->
    {ProtoIds, Binary};
getProtoIds(Binary, Count, ProtoIds) ->
    {ProtoId, Rest} = rVarInt(Binary),
    getProtoIds(Rest, Count - 1, ProtoIds ++ [ProtoId]).

getProtoIds(Binary) ->
    {Sizep, Rest} = rVarInt(Binary),
    getProtoIds(Rest, [], Sizep).

readLineInfo(Binary, Sizecode) ->
    {Linegaplog2, Rest1} = rByte(Binary),

    {Lineinfo, Rest2} = readLineInfoL(Rest1, Sizecode),

    Intervals = (Sizecode - 1) bsr Linegaplog2 + 1,

    {Abslineinfo, Rest3} = readLineInfoAbs(Rest2, Intervals),

    lists:map(
        fun({Index, Value}) ->
            lists:nth(Index > Linegaplog2, Abslineinfo) + Value
        end,
        lists:enumerate(Abslineinfo)
    ).

skipDebugInfoL(Binary, 0) ->
    Binary;
skipDebugInfoL(Binary, Size) ->
    {Rest1, _} = skipVarInt(Binary),
    {Rest2, _} = skipVarInt(Rest1),
    {Rest3, _} = skipVarInt(Rest2),
    skipDebugInfoL(skipByte(Rest3), Size - 1).

skipDebugInfoUpvalues(Binary, 0) ->
    Binary;
skipDebugInfoUpvalues(Binary, Size) ->
    skipDebugInfoUpvalues(skipVarInt(Binary), Size - 1).

skipDebugInfo(Binary) ->
    {Sizel, Rest1} = rVarInt(Binary),
    % io:format("Debug info size: ~p~n", [Size]),
    Rest2 = skipDebugInfoL(Rest1, Sizel),

    {Sizeupvals, Rest3} = rVarInt(Rest2),
    % io:format("Upvalues size: ~p~n", [Sizeupvals]),
    skipDebugInfoUpvalues(Rest3, Sizeupvals).

readProto(Binary, StringList) ->
    <<MaxStackSize:8, NumParams:8, Nups:8, Rest3/binary>> = Binary,

    <<_:16, Rest4/binary>> = Rest3,
    {Typesize, Rest5} = rVarInt(Rest4),
    <<_:Typesize/binary, Rest6/binary>> = Rest5,
    {Sizecode, Rest7} = rVarInt(Rest6),
    % rest7 looks good here

    io:format("Sizecode: ~p~n", [Sizecode]),
    {Code1, Rest8} = readInsts(Rest7, Sizecode),
    io:format("Code: ~p~n", [Code1]),

    {Sizek, Rest9} = rVarInt(Rest8),

    {K, Rest10} = getKs(Rest9, StringList, Sizek),

    Code2 = lists:map(
        fun(I) ->
            io:format("Checking inst - aux: ~p kmode: ~p~n", [I#inst.aux, I#inst.kMode]),
            checkkmode(I, K)
        end,
        Code1
    ),
    io:format("Rest: ~p~n", [Rest10]),

    {Protos, Rest11} = getProtoIds(Rest10),

    Rest12 = skipVarInt(Rest11),
    {Dbgnamei, Rest13} = rVarInt(Rest12),
    Dbgname =
        case Dbgnamei of
            0 -> "(??)";
            _ -> lists:nth(Dbgnamei, StringList)
        end,

    {LineInfoEnabled, Rest14} = rBool(Rest13),
    {InstLineInfo, Rest15} =
        case LineInfoEnabled of
            true ->
                {LineInfo, Rest} = readLineInfo(Rest14, Sizecode),
                {LineInfo, Rest};
            _ ->
                {[], Rest14}
        end,

    {DebugInfoEnabled, Rest16} = rBool(Rest15),
    Rest17 =
        case DebugInfoEnabled of
            true ->
                skipDebugInfo(Rest16);
            _ ->
                Rest16
        end,

    {
        #proto{
            maxStackSize = MaxStackSize,
            numParams = NumParams,
            nups = Nups,
            protos = Protos,
            dbgname = Dbgname,
            code = Code2,
            instLineInfo = InstLineInfo
        },
        Rest17
    }.

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
    % io:format("Luau Version: ~p~n", [LuauVersion]),

    case TypesVersion of
        3 -> ok;
        _ -> error("the types version of the provided bytecode is unsupported")
    end,
    % io:format("Types Version: ~p~n", [TypesVersion]),

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
