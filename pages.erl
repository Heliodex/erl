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
            % shouldn't be in the head but idc
            "	<meta charset=\"utf-8\" />"
            "	<title>Luau + Erlang</title>"
            "</head>"
            "<body>"
            "	<h1><a href=\"https://luau.org\" target=\"_blank\">Luau</a> + <a href=\"https://www.erlang.org\" target=\"_blank\">Erlang</a></h1>"
            "	<p>Welcome! This is a test of an experimental Luau VM written in Erlang. I wrote this to learn Erlang (I already knew Luau, and have previously written <a href=\"https://github.com/Heliodex/coputer/tree/main/litecode\" target=\"_blank\">a Luau VM in Go</a>) and I'm still learning, so it's rather unstable at the moment. If you see an \"Internal Server Error\" page, try reloading.</p>"
            "	<p>On this page you can write some Luau code (or try out the examples). When the Run button is pressed, it's compiled by the Luau compiler to bytecode, where it is deserialised and executed by my VM implementation, and its output and return values are shown below. At the moment the deserialiser + VM totals ~1200 lines of Erlang, and the Go version which it is based on is ~1600 lines of Go. This version doesn't include Luau's standard library, which would be another 3000-4000 lines, so the only available function is print(). Some operations aren't implemented yet, for example while loops should work but for loops don't. This will, of course, be improved in future!</p>"
            "	<p>Source code: <a href=\"https://github.com/Heliodex/erl\" target=\"_blank\">github.com/Heliodex/erl</a></p>"
            "	<h2>Try some examples</h2>"
            "	<ul>"
            "		<li><form method=\"post\">"
            "			<textarea style=\"display:none\" name=\"code\">print \"Hello, world!\"</textarea>"
            "   		<button type=\"submit\">Hello world</button>"
            "		</form></li>"
            "		<li><form method=\"post\">"
            "			<textarea style=\"display:none\" name=\"code\">print(33 + 53)\n"
            "print(33 - 53)\n"
            "print(33 * 53)\n"
            "print(33 / 53)\n"
            "print(33 % 53)\n"
            "print(33 ^ 53)\n"
            "print(33 // 53)\n"
            "print(-5)</textarea>"
            "   		<button type=\"submit\">Mathematics</button>"
            "		</form></li>"
            "		<li><form method=\"post\">"
            "			<textarea style=\"display:none\" name=\"code\">local x: number = 0\n"
            "while x < 10 do\n"
            "	x += 1\n"
            "	print(x)\n"
            "end</textarea>"
            "   		<button type=\"submit\">While loop</button>"
            "		</form></li>"
            "		<li><form method=\"post\">"
            "			<textarea style=\"display:none\" name=\"code\">local t = {}\n"
            "t.hello = \"world\"\n"
            "print(t.hello)</textarea>"
            "   		<button type=\"submit\">Table accesses</button>"
            "		</form></li>"
            % "		<li><form method=\"post\">"
            % "			<textarea style=\"display:none\" name=\"code\">return \"Goodbye!\", 1, 2, 3</textarea>"
            % "   		<button type=\"submit\">Return values</button>"
            % "		</form></li>"
            "	</ul>"
            "	<h2>Run code</h2>"
            "	<form method=\"post\">"
            "		<textarea name=\"code\" placeholder=\"Enter your code\" cols=\"30\" rows=\"10\">" ++
            Code ++
            "</textarea>"
            "		<button type=\"submit\">Run</button>"
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
        % io:format("Result: ~p~n", [Result]),

        binary_to_list(Result) ++ "\nReturns: " ++ lists:join(", ", ReturnsList)
    catch
        error:Reason:Stack ->
            io_lib:format("Error: ~p~nStack: ~p~n", [Reason, Stack])
    end.

test(SessionID, Input) ->
    % io:format("SessionID: ~p~nInput: ~p~n", [SessionID, Input]),

    case uri_string:dissect_query(Input) of
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
                "	<h1>Invalid input</h1>"
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
