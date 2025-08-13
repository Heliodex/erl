-module(compilemod).
-export([start/0, compile/2]).

-record(compiler, {o = 0}).

% -record(proto, {
%     dbgname = "",
%     code = [],
%     instLineInfo = [],
%     protos = [],
%     maxStackSize = 0,
%     numParams = 0,
%     nups = 0
% }).

% -record(deserialised, {mainProto = #proto{}, protoList = []}).

-record(deserpath, {deserialised, dbgpath = ""}).

-record(program, {deserpath = #deserpath{}, filepath = "", compiler = #compiler{}, requireHistory = []}).

ext() -> ".luau".

luauCompile(Path, O) ->
    os:cmd("luau-compile --binary -O" ++ integer_to_list(O) ++ " " ++ Path).

compile(C, Path) ->
    % hash path instead of bytecode
    % Hash = crypto:hash(sha3_256, Path),

    % find if file at path exists
    Pathext1 = Path ++ ext(),
    Pathext =
        case filelib:is_file(Pathext1) of
            true ->
                Pathext1;
            _ ->
                case filelib:is_dir(Path) of
                    true ->
                        Path ++ "/main" ++ ext();
                    _ ->
                        error("error finding file")
                end
        end,

    B = luauCompile(Pathext, C#compiler.o),
    % convert list to binary
    Bin = list_to_binary(B),

    % dbgpath has the extension and all
    D = deserialise:deserialise(Bin),

    Dp = #deserpath{
        deserialised = D,
        dbgpath = Pathext
    },
    % TODO: cache

    #program{
        deserpath = Dp,
        filepath = Path,
        compiler = C
    }.

start() ->
    try
        C = #compiler{},
        compile(C, "hello")
    catch
        error:Reason:Stack ->
            io:format("Error: ~p~nStack: ~p~n", [Reason, Stack])
    end,
    io:format("~n"),
    init:stop().
