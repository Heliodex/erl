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

-record(program, {
    deserpath = #deserpath{}, filepath = "", compiler = #compiler{}, requireHistory = []
}).

ext() -> ".luau".

% os:exec returns as a string (why??) and jumbles the bytecode accordingly
get_data(Port, Sofar) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, [Sofar | Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    true
            end,
            receive
                {'EXIT', Port, _} ->
                    ok
                % force context switch
            after 1 ->
                ok
            end,
            list_to_binary(lists:flatten(Sofar))
    end.

my_exec(Command) ->
    Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
    get_data(Port, []).

luauCompile(Path, O) ->
    my_exec("luau-compile --binary -O" ++ integer_to_list(O) ++ " " ++ Path).

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

    Bin = luauCompile(Pathext, C#compiler.o),
    % io:format("File content: ~p~n", [Bin]),

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
