-module(bellboy_http_server).
-author('Steve Vinoski <vinoski@ieee.org>').
-export([start/1, start/2, stop/1]).

%% start/1 takes a handler function and starts the web server on port 8000.

%% start/2 takes a handler function and a port number. The handler function
%% takes two arguments: a TCP socket and request data. The request data is
%% a property list indicating the invoked HTTP method, the target URI, the
%% HTTP version of the request, and a list of HTTP headers sent with the
%% request. For requests that provide data such as PUT and POST, the
%% handler function is expected to read the body off the socket. The
%% handler function returns a 3-tuple indicating the response HTTP status
%% code, the response HTTP headers in the form of {header,value} pairs, and
%% any response body. The start/2 function waits to receive a stop atom,
%% indicating it should close the listen socket and exit.
start(Handler) ->
    {ok, spawn_link(fun() -> do_start(Handler, 8000) end)}.

start(Handler, Port) ->
    {ok, spawn_link(fun() -> do_start(Handler, Port) end)}.

do_start(Handler, Port) ->
    {ok, LS} = gen_tcp:listen(Port, [{reuseaddr,true}, binary, {backlog,1024}]),
    spawn_link(fun() -> accept(LS, Handler) end),
    receive
        {stop, StopPID} ->
            gen_tcp:close(LS),
            StopPID ! ok
    end.

stop(PID) when is_pid(PID) ->
    PID ! {stop, self()},
    receive
        ok -> ok
    after 100 -> error
    end.

%% The accept/2 function accepts a connection, spawns a new acceptor, and
%% then handles its incoming request.
accept(LS, Handler) ->
    case gen_tcp:accept(LS) of
        {ok, S} ->
            ok = inet:setopts(S, [{packet,http_bin}]),
            spawn_link(fun() -> accept(LS, Handler) end),
            serve(S, Handler, [{headers, []}]);
        {error, closed} ->
            ok
    end.

%% The serve/3 function reads the request headers, assembles the request
%% data property list, calls the handler/2 function to handle the request,
%% and assembles and returns the response.
serve(S, Handler, Req) ->
    ok = inet:setopts(S, [{active, once}]),
    HttpMsg = receive
                  {http, S, Msg} -> Msg;
                  _ -> gen_tcp:close(S)
              end,
    case HttpMsg of
        {http_request, M, {abs_path, Uri}, Vsn} ->
            NReq = [{method,M},{uri,Uri},{version,Vsn}|Req],
            serve(S, Handler, NReq);
        {http_header, _, Hdr, _, Val} ->
            {headers, Hdrs} = lists:keyfind(headers, 1, Req),
            serve(S, Handler, lists:keystore(headers, 1, Req,
                                             {headers, [{Hdr,Val}|Hdrs]}));
        http_eoh ->
            ok = inet:setopts(S, [{packet, raw}]),
            {RawStatus, Hdrs, Resp} = try Handler(S, Req) catch _:_ -> {500, [], <<>>} end,
            Status = if
                RawStatus =:= 200 -> "200 OK";
                is_list(RawStatus) -> RawStatus
            end,
            ok = gen_tcp:send(S, ["HTTP/1.1 ", Status, "\r\n",
                                  [[H, ": ", V, "\r\n"] || {H,V} <- Hdrs],
                                  "\r\n", Resp]),
            gen_tcp:close(S);
        {http_error, Error} ->
            exit(Error);
        ok -> ok
    end.
