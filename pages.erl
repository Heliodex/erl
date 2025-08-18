-module(pages).
-export([run/3]).

-record(compiler, {o = 0}).

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

render(SessionID, Code, Out) ->
    mod_esi:deliver(
        SessionID,
        "" ++
            "<!doctype html>"
            "<html>"
            "<head>"
            "	<title>Erlang</title>"
            "</head>"
            "<body>"
            "	<h1>Hello, world!</h1>"
            "	<form method=\"post\">"
            "		<textarea type=\"text\" name=\"code\" placeholder=\"Enter your code\" cols=\"30\" rows=\"10\">" ++
            Code ++
            "</textarea>"
            "		<button type=\"submit\">Submit</button>"
            "	</form>"
            "	<output>"
            "		<pre>" ++ Out ++
            "</pre>"
            "	</output>"
            "</body>"
            "</html>"
    ).

runcode(Code) ->
    Hash = crypto:hash(sha3_256, Code),
    Hex = [Y || <<X:4>> <= Hash, Y <- integer_to_list(X, 16)],

    Filename = "/tmp/" ++ Hex,
    FilenameExt = Filename ++ ".luau",

    try
        ok = file:write_file(FilenameExt, list_to_binary(Code)),

        C = #compiler{},
        P = compilemod:compile(C, Filename),
        % clear file
        ok = file:write_file(FilenameExt, ""),

        Env = #{
            "print" => vm:fn("print", nil, fun(_, Args) ->
                StrArgs = lists:map(fun(A) -> io_lib:format("~p", [A]) end, Args),
                ok = file:write_file(
                    FilenameExt, lists:flatten([lists:join(" ", StrArgs), "\n"]), [append]
                ),

                array:new()
            end)
        },

        Co = vm:load(P, Env),

        Returns = (Co#coroutine.function#function.run)(Co, []),
        ReturnsList = array:to_list(Returns),

        {ok, Result} = file:read_file(FilenameExt),
        io:format("Result: ~p~n", [Result]),

        binary_to_list(Result) ++ "\nReturns: " ++ lists:join(", ", ReturnsList)
    catch
        error:Reason:Stack ->
            io_lib:format("Error: ~p~nStack: ~p~n", [Reason, Stack])
    end.

test(SessionID, Input) ->
    % io:format("SessionID: ~p~nInput: ~p~n", [SessionID, Input]),

    Q = uri_string:dissect_query(Input),
    case Q of
        [] ->
            render(SessionID, "", "");
        [{"code", Code}] ->
            % io:format("Code: ~p~n", [Code]),
            Out = runcode(Code),
            render(SessionID, Code, Out);
        _ ->
            mod_esi:deliver(
                SessionID,
                "<!doctype html>"
                "<html>"
                "<head>"
                "	<title>Error</title>"
                "</head>"
                "<body>"
                "	<h1>Invalid Input</h1>"
                "	<p>Please provide valid code.</p>"
                "</body>"
                "</html>"
            )
    end.

run(SessionID, _Env, Input) ->
    try
        test(SessionID, Input)
    catch
        error:Reason:Stack ->
            io:format("Error: ~p~nStack: ~p~n", [Reason, Stack]),
            init:stop()
    end.
