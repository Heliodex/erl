-module(compile).
-export([start/0]).

-record(compiler, {o = 0}).

ext() -> ".luau".

read_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            Binary;
        {error, Reason} ->
            error(Reason)
    end.

luauCompile(Pathext, O) ->
	ok.

compile(Compiler, Path) ->
    % hash path instead of bytecode
    Hash = crypto:hash(sha3_256, Path),

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

    B = luauCompile(Pathext, Compiler#compiler.o),

    io:format("Compiling ~s~n", [Pathext]),
    ok.

start() ->
    try
        Compiler = #compiler{},
        compile(Compiler, "hello")
    catch
        error:Reason:Stack ->
            io:format("Error: ~p~nStack: ~p~n", [Reason, Stack])
    end,
    io:format("~n"),
    init:stop().
