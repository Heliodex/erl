-module(deserialise).
-export([start/0]).

read_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            Binary;
        {error, Reason} ->
            erlang:error(Reason)
    end.

main() ->
    io:format("Deserialisation started~n"),
    FileContent = read_file("hello.bytecode"),
    io:format("File content: ~p~n", [FileContent]).

start() ->
    try
        main()
    catch
        error:Reason:Stack ->
            io:format("Error: ~p~nStack: ~p~n", [Reason, Stack])
    end,
    io:format("~n"),
    init:stop().
