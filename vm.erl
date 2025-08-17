-module(vm).
-export([start/0]).

-record(compiler, {o = 0}).

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

-record(deserialised, {mainProto, protoList = []}).

-record(deserpath, {deserialised = #deserialised{}, dbgpath = ""}).

-record(program, {
    deserpath = #deserpath{}, filepath = "", compiler = #compiler{}, requireHistory = []
}).

-record(toWrap, {
    proto,
    protoList = [],
    upvals = [],
    % alive = false,
    env = #{},
    % Store the last return, as it's the only one that's relevant
    requireCache = #{}
}).

-record(function, {
    run,
    name = "",
    co = nil
}).

-record(coroutine, {
    function = #function{},
    env,
    filepath = "",
    dbgpath = "",
    requireHistory = [],
    % yieldChan,
    % resumeChan,
    dbg,
    compiler = #compiler{},
    status = conotstarted
    % programArgs
}).

-record(table, {
    % we'll just use hash for now
    readonly = false,
    data = #{}
}).

a_add(A, B) ->
    A + B.

a_sub(A, B) ->
    A - B.

a_mul(A, B) ->
    A * B.

a_div(A, B) ->
    A / B.

a_mod(A, B) ->
    A - B * math:floor(A / B).

a_pow(A, B) ->
    math:pow(A, B).

a_idiv(A, B) ->
    math:floor(A / B).

a_unm(A) ->
    -A.

falsy(V) ->
    V == nil orelse V == false.

exts() ->
    #{}.

fn(Name, Co, F) ->
    #function{
        run = F,
        name = Name,
        co = Co
    }.

luau_multret() -> -1.

gettable(K, V) ->
    % only tables for now
    case maps:find(K, V#table.data) of
        {ok, Val} -> Val;
        error -> nil
    end.

settable(K, V, Table) ->
    Table#table{
        data = maps:put(K, V, Table#table.data)
    }.

movestackloop(Stack, _, I, B, _) when I == B ->
    Stack;
movestackloop(Stack, Src, I, B, T) ->
    L = array:size(Src),
    Stack2 = array:set(
        T + I,
        if
            I > L -> nil;
            true -> array:get(I, Src)
        end,
        Stack
    ),
    movestackloop(Stack2, Src, I + 1, B, T).

% i don't assume this to be correct first try
movestack(Stack, Src, B, T) ->
    L = max(T + B + 1, array:size(Stack)),
    Stack2 = array:resize(L, Stack),
    % io:format("Resized to ~p ~p ~p~n", [B, T, L]),

    % Src2 = array:resize(B, Src),
    % % Replace section of Stack, from T with length B, with Src
    % Stack3 = array:from_list(lists:sublist(array:to_list(Stack2), T)),
    % Stack4 = array:from_list(lists:sublist(array:to_list(Stack2), T + B + 1, L - (T + B))),
    % array:from_list(array:to_list(Stack3) ++ array:to_list(Src2) ++ array:to_list(Stack4)).
    movestackloop(Stack2, Src, 0, B, T).

call(Top, A, B, C, Towrap, Stack, Co) ->
    F = array:get(A, Stack),

    % TODO: uncallable types

    Params =
        deserialise:int32(
            if
                B == 0 ->
                    Top - A;
                true ->
                    B
            end
        ),

    % io:format("Calling function ~p with params ~p~n", [F, Params]),

    Rco =
        if
            F#function.co == nil ->
                Co;
            true ->
                F#function.co
        end,

    Args = lists:sublist(array:to_list(Stack), A + 2, Params - 1),
    RetList = (F#function.run)(Rco, Args),
    RetCount = array:size(RetList),

    % TODO: require()s

    {Top2, RetCount2} =
        if
            C == 0 -> {A + RetCount, RetCount};
            true -> {Top, deserialise:int32(C - 1)}
        end,

    % io:format("RetList: ~p~n", [RetList]),
    Stack2 = movestack(Stack, RetList, RetCount2, A),
    {Top2, Stack2}.

execloop(Towrap, Pc, Top, Code, Stack, Co) ->
    I = lists:nth(Pc + 1, Code),
    Op = I#inst.opcode,
    % io:format("Executing opcode ~p at pc ~p with stack ~p ~p~n", [
    %     Op, Pc, array:size(Stack), array:to_list(Stack)
    % ]),
    case Op of
        % NOP
        0 ->
            % Do nothing
            execloop(Towrap, Pc + 1, Top, Code, Stack, Co);
        % LOADNIL
        2 ->
            Stack2 = array:set(I#inst.a, nil, Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % LOADB
        3 ->
            Stack2 = array:set(I#inst.a, I#inst.b == 1, Stack),
            execloop(Towrap, Pc + I#inst.c + 1, Top, Code, Stack2, Co);
        % LOADN
        4 ->
            Stack2 = array:set(I#inst.a, I#inst.d, Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % LOADK
        5 ->
            % io:format("K: ~p~n", [I#inst.k]),
            Stack2 = array:set(I#inst.a, I#inst.k, Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % MOVE
        6 ->
            Stack2 = array:set(I#inst.a, array:get(I#inst.b, Stack), Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % GETGLOBAL
        7 ->
            Kv = I#inst.k,

            Stack2 =
                case maps:find(Kv, exts()) of
                    {ok, E} ->
                        array:set(I#inst.a, E, Stack);
                    _ ->
                        case maps:find(Kv, Towrap#toWrap.env) of
                            {ok, E} ->
                                array:set(I#inst.a, E, Stack);
                            _ ->
                                array:set(I#inst.a, nil, Stack)
                        end
                end,
            execloop(Towrap, Pc + 2, Top, Code, Stack2, Co);
        % SETGLOBAL
        8 ->
            % LOL
            error("attempt to set global");
        % GETTABLE
        13 ->
            Stack2 = array:set(
                I#inst.a, gettable(array:get(I#inst.c, Stack), array:get(I#inst.b, Stack)), Stack
            ),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % SETTABLE
        14 ->
            Idx = array:get(I#inst.c, Stack),
            T = array:get(I#inst.b, Stack),
            if
                T#table.readonly ->
                    error("attempt to modify a readonly table");
                Idx == nil ->
                    error("table index is nil");
                true ->
                    ok
            end,

            T2 = settable(Idx, array:get(I#inst.a, Stack), T),
            Stack2 = array:set(I#inst.b, T2, Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % GETTABLEKS
        15 ->
            Stack2 = array:set(
                I#inst.a, gettable(I#inst.k, array:get(I#inst.b, Stack)), Stack
            ),
            % adjust for aux
            execloop(Towrap, Pc + 2, Top, Code, Stack2, Co);
        % SETTABLEKS
        16 ->
            Idx = I#inst.k,
            T = array:get(I#inst.b, Stack),
            if
                T#table.readonly ->
                    error("attempt to modify a readonly table");
                Idx == nil ->
                    error("table index is nil");
                true ->
                    ok
            end,

            T2 = settable(Idx, array:get(I#inst.a, Stack), T),
            Stack2 = array:set(I#inst.b, T2, Stack),
            % adjust for aux
            execloop(Towrap, Pc + 2, Top, Code, Stack2, Co);
        % GETUPVAL
        % SETUPVAL
        % CALL
        21 ->
            % oh no
            {Top2, Stack2} = call(Top, I#inst.a, I#inst.b, I#inst.c, Towrap, Stack, Co),
            execloop(Towrap, Pc + 1, Top2, Code, Stack2, Co);
        % RETURN
        22 ->
            B = deserialise:int32(I#inst.b) - 1,
            MR = luau_multret(),

            % nresults
            B2 =
                if
                    B == MR -> Top - I#inst.a;
                    true -> B
                end,

            % execloop() should pretty much always exit through here
            Stack2 = array:resize(I#inst.a + B2, Stack),
            array:from_list(lists:sublist(array:to_list(Stack2), I#inst.a + 1, B2));
        % JUMP
        23 ->
            execloop(Towrap, Pc + I#inst.d + 1, Top, Code, Stack, Co);
        % JUMPBACK
        24 ->
            execloop(Towrap, Pc + I#inst.d + 1, Top, Code, Stack, Co);
        % JUMPIF
        25 ->
            Pci =
                case falsy(array:get(I#inst.a, Stack)) of
                    true ->
                        1;
                    _ ->
                        I#inst.d + 1
                end,
            execloop(Towrap, Pc + Pci, Top, Code, Stack, Co);
        % JUMPIFNOT
        26 ->
            Pci =
                case falsy(array:get(I#inst.a, Stack)) of
                    true ->
                        I#inst.d + 1;
                    _ ->
                        1
                end,
            execloop(Towrap, Pc + Pci, Top, Code, Stack, Co);
        % jump
        27 ->
            Pci =
                case array:get(I#inst.a, Stack) == array:get(I#inst.aux, Stack) of
                    true ->
                        I#inst.d + 1;
                    _ ->
                        2
                end,
            execloop(Towrap, Pc + Pci, Top, Code, Stack, Co);
        30 ->
            Pci =
                case array:get(I#inst.a, Stack) /= array:get(I#inst.aux, Stack) of
                    true ->
                        I#inst.d + 1;
                    _ ->
                        2
                end,
            execloop(Towrap, Pc + Pci, Top, Code, Stack, Co);
        % arithmetic
        33 ->
            % io:format("Adding ~p and ~p~n", [array:get(I#inst.b, Stack), array:get(I#inst.c, Stack)]),
            Stack2 = array:set(
                I#inst.a, a_add(array:get(I#inst.b, Stack), array:get(I#inst.c, Stack)), Stack
            ),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        34 ->
            Stack2 = array:set(
                I#inst.a, a_sub(array:get(I#inst.b, Stack), array:get(I#inst.c, Stack)), Stack
            ),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        35 ->
            Stack2 = array:set(
                I#inst.a, a_mul(array:get(I#inst.b, Stack), array:get(I#inst.c, Stack)), Stack
            ),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        36 ->
            Stack2 = array:set(
                I#inst.a, a_div(array:get(I#inst.b, Stack), array:get(I#inst.c, Stack)), Stack
            ),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        37 ->
            Stack2 = array:set(
                I#inst.a, a_mod(array:get(I#inst.b, Stack), array:get(I#inst.c, Stack)), Stack
            ),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        38 ->
            Stack2 = array:set(
                I#inst.a, a_pow(array:get(I#inst.b, Stack), array:get(I#inst.c, Stack)), Stack
            ),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        81 ->
            Stack2 = array:set(
                I#inst.a, a_idiv(array:get(I#inst.b, Stack), array:get(I#inst.c, Stack)), Stack
            ),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % arithmetik
        39 ->
            Stack2 = array:set(I#inst.a, a_add(array:get(I#inst.b, Stack), I#inst.k), Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        40 ->
            Stack2 = array:set(I#inst.a, a_sub(array:get(I#inst.b, Stack), I#inst.k), Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        41 ->
            Stack2 = array:set(I#inst.a, a_mul(array:get(I#inst.b, Stack), I#inst.k), Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        42 ->
            Stack2 = array:set(I#inst.a, a_div(array:get(I#inst.b, Stack), I#inst.k), Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        43 ->
            Stack2 = array:set(I#inst.a, a_mod(array:get(I#inst.b, Stack), I#inst.k), Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        44 ->
            Stack2 = array:set(I#inst.a, a_pow(array:get(I#inst.b, Stack), I#inst.k), Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        82 ->
            Stack2 = array:set(I#inst.a, a_idiv(array:get(I#inst.b, Stack), I#inst.k), Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % logic AND
        45 ->
            {A, B} = {array:get(I#inst.b, Stack), array:get(I#inst.c, Stack)},
            Stack2 =
                case falsy(A) of
                    true ->
                        array:set(I#inst.a, A, Stack);
                    _ ->
                        array:set(I#inst.a, B, Stack)
                end,
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % logic OR
        46 ->
            {A, B} = {array:get(I#inst.b, Stack), array:get(I#inst.c, Stack)},
            Stack2 =
                case falsy(A) of
                    true ->
                        array:set(I#inst.a, B, Stack);
                    _ ->
                        array:set(I#inst.a, A, Stack)
                end,
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % logik AND
        47 ->
            {A, B} = {array:get(I#inst.b, Stack), I#inst.k},
            Stack2 =
                case falsy(A) of
                    true ->
                        array:set(I#inst.a, A, Stack);
                    _ ->
                        array:set(I#inst.a, B, Stack)
                end,
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % logik OR
        48 ->
            {A, B} = {array:get(I#inst.b, Stack), I#inst.k},
            Stack2 =
                case falsy(A) of
                    true ->
                        array:set(I#inst.a, B, Stack);
                    _ ->
                        array:set(I#inst.a, A, Stack)
                end,
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % NOT
        50 ->
            Stack2 = array:set(I#inst.a, falsy(array:get(I#inst.b, Stack)), Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % MINUS
        51 ->
            Stack2 = array:set(I#inst.a, a_unm(array:get(I#inst.b, Stack)), Stack),
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % NEWTABLE
        53 ->
            Stack2 = array:set(I#inst.a, #table{}, Stack),
            % adjust for aux
            execloop(Towrap, Pc + 2, Top, Code, Stack2, Co);
        % DUPTABLE
        54 ->
            Stack2 = array:set(I#inst.a, #table{}, Stack),
            % doesn't really apply here...
            execloop(Towrap, Pc + 1, Top, Code, Stack2, Co);
        % FORNLOOP
        57 ->
            Init = array:get(I#inst.a + 2, Stack),
            Limit = array:get(I#inst.a, Stack),
            Step = array:get(I#inst.a + 1, Stack),

            Init2 = Init + Step,
            Stack2 = array:set(I#inst.a + 2, Init2, Stack),

            S = Step > 0,
            Pci =
                case S andalso Init =< Limit orelse not S andalso Init >= Limit of
                    true -> I#inst.d + 1;
                    _ -> 1
                end,
            execloop(Towrap, Pc + Pci, Top, Code, Stack2, Co);
        % FASTCALL3
        60 ->
            % Skipped
            % adjust for aux
            execloop(Towrap, Pc + 2, Top, Code, Stack, Co);
        % PREPVARARGS
        65 ->
            % handled by wrapper
            execloop(Towrap, Pc + 1, Top, Code, Stack, Co);
        % LOADKX
        66 ->
            Stack2 = array:set(I#inst.a, I#inst.k, Stack),
            % adjust for aux
            execloop(Towrap, Pc + 2, Top, Code, Stack2, Co);
        % JUMPX
        67 ->
            execloop(Towrap, Pc + I#inst.a + 1, Top, Code, Stack, Co);
        % FASTCALL
        68 ->
            % Skipped
            execloop(Towrap, Pc + 1, Top, Code, Stack, Co);
        % FASTCALL1
        73 ->
            % Skipped
            execloop(Towrap, Pc + 1, Top, Code, Stack, Co);
        % FASTCALL2
        74 ->
            % Skipped
            % adjust for aux
            execloop(Towrap, Pc + 2, Top, Code, Stack, Co);
        % FASTCALL2K
        75 ->
            % Skipped
            % adjust for aux
            execloop(Towrap, Pc + 2, Top, Code, Stack, Co);
        % FORGPREP
        76 ->
            % what are we even supposed to do here, there's nothing to prepare
            execloop(Towrap, Pc + I#inst.d + 1, Top, Code, Stack, Co);
        % JUMPXEQKNIL
        77 ->
            Ra = array:default(I#inst.a, Stack),
            Pci =
                case (Ra == nil) /= I#inst.kn of
                    true -> I#inst.d + 1;
                    _ -> 2
                end,
            execloop(Towrap, Pc + Pci, Top, Code, Stack, Co);
        % JUMPXEQKB
        78 ->
            {Kv, Ra} = {I#inst.k, array:default(I#inst.a, Stack)},
            Pci =
                case (Ra == Kv) /= I#inst.kn of
                    true -> I#inst.d + 1;
                    _ -> 2
                end,
            execloop(Towrap, Pc + Pci, Top, Code, Stack, Co);
        % JUMPXEQKN
        79 ->
            {Kv, Ra} = {I#inst.k, array:default(I#inst.a, Stack)},
            Pci =
                case (Ra == Kv) /= I#inst.kn of
                    true -> I#inst.d + 1;
                    _ -> 2
                end,
            execloop(Towrap, Pc + Pci, Top, Code, Stack, Co);
        % JUMPXEQKS
        80 ->
            {Kv, Ra} = {I#inst.k, array:default(I#inst.a, Stack)},
            Pci =
                case (Ra == Kv) /= I#inst.kn of
                    true -> I#inst.d + 1;
                    _ -> 2
                end,
            execloop(Towrap, Pc + Pci, Top, Code, Stack, Co)
    end.

execute(Towrap, Stack, VargsList, Co) ->
    {P, Upvals} = {Towrap#toWrap.proto, Towrap#toWrap.upvals},

    {Code, LineInfo, Protos} = {P#proto.code, P#proto.instLineInfo, P#proto.protos},

    % Co2 = Co#coroutine{
    % 	dbg = Co#coroutine.dbg#
    % }.

    execloop(Towrap, 0, 0, Code, Stack, Co).

wrapclosure(Towrap, ExistingCo) ->
    Proto = Towrap#toWrap.proto,
    {Maxs, Np} = {Proto#proto.maxStackSize, Proto#proto.numParams},

    fn("", ExistingCo, fun(Co, Args) ->
        La = deserialise:uint8(length(Args)),

        List =
            if
                Np < La ->
                    % args from np onwards
                    Args = lists:sublist(Args, Np + 1, La);
                true ->
                    []
            end,

        % io:format("List: ~p~n", [List]),

        Stack1 = array:from_list(lists:sublist(Args, min(Np, La))),
        Stack2 = array:resize(max(Maxs, La - Np), Stack1),

        % InitDbg = Co#coroutine.dbg,

        % io:format("Starting stack: ~p~n", [array:to_list(Stack2)]),
        execute(Towrap, Stack2, List, Co)
    end).

loadmodule(P, Env, RequireCache) ->
    % Alive = true,

    Towrap = #toWrap{
        proto = P#program.deserpath#deserpath.deserialised#deserialised.mainProto,
        protoList = [],
        upvals = [],
        env = Env,
        requireCache = RequireCache
    },

    #coroutine{
        function = wrapclosure(Towrap, nil),
        env = Env,
        filepath = P#program.filepath,
        dbgpath = P#program.deserpath#deserpath.dbgpath,
        requireHistory = P#program.requireHistory,
        % yieldChan = nil,
        % resumeChan = nil,
        compiler = P#program.compiler
        % programArgs = Args
    }.

% TODO: args
load(P, Env) ->
    loadmodule(P, Env, #{}).

start() ->
    try
        C = #compiler{},
        P = compilemod:compile(C, "hello"),

        Env = #{
            "print" => fn("print", nil, fun(_, Args) ->
                lists:foreach(
                    fun(X) ->
                        io:format("~p", [X])
                    end,
                    Args
                ),
                io:format("~n"),

                array:new()
            end)
        },

        Co = load(P, Env),
        % io:format("Co: ~p~n", [Co#coroutine.function]),
        Returns = (Co#coroutine.function#function.run)(Co, []),
        io:format("Returns: ~p~n", [array:to_list(Returns)])
    catch
        error:Reason:Stack ->
            io:format("Error: ~p~nStack: ~p~n", [Reason, Stack])
    end,
    io:format("~n"),
    init:stop().
