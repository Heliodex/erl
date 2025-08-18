-module(server).
-export([start/0]).

run_server() ->
    inets:start(),
    {ok, _} = inets:start(httpd, [
        {port, 5555},
        {server_name, "server"},
        {server_root, "."},
        {document_root, "."},
        {modules, [
            mod_alias,
            mod_auth,
            mod_esi,
            mod_actions,
            mod_cgi,
            mod_dir,
            mod_get,
            mod_head,
            mod_log,
            mod_disk_log
        ]},
        {erl_script_alias, {"/erl", [pages]}}
    ]),
    io:format("HTTP server started on port 5555~n").

start() ->
    try
        run_server()
    catch
        error:Reason:Stack ->
            io:format("Error: ~p~nStack: ~p~n", [Reason, Stack])
    end.
