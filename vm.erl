-module(vm).
-export([start/0]).

-record(compiler, {o = 0}).

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

-record(program, {deserpath = #deserpath{}, filepath = "", compiler = #compiler{}, requireHistory = []}).

-record(toWrap, {
    proto,
    protoList = [],
    upvals = [],
    % alive = false,
    env = #{},
    % Store the last return, as it's the only one that's relevant
    requireCache = #{}
}).

-record(coroutine, {
    function,
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

wrapclosure(Towrap, ExistingCo) ->
	Proto = Towrap#toWrap.proto,
	{Maxs, Np} = {Proto#proto.maxStackSize, Proto#proto.numParams},
	ok.


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

        Env = #{},

        Co = load(P, Env)
    catch
        error:Reason:Stack ->
            io:format("Error: ~p~nStack: ~p~n", [Reason, Stack])
    end,
    io:format("~n"),
    init:stop().
