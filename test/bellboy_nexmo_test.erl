-module(bellboy_nexmo_test).

-include_lib("eunit/include/eunit.hrl").

start_server(Kind) ->
    Parent = self(),
    bellboy_http_server:start(
        fun(S, Req) -> http_handler(Kind, Parent, S, Req) end,
        8000
    ).

http_handler(nexmo_ok, PID, _Socket, Request) ->
    PID ! Request,
    Content = <<
        "{\n    \"message-count\": \"1\",\n    "
        "\"messages\": [{\n        \"to\": \"34666666666\",\n        "
        "\"message-id\": \"14000001A4ECACC7\",\n        "
        "\"status\": \"0\",\n        \"remaining-balance\": \"1.92050000\","
        "\n        \"message-price\": \"0.07950000\","
        "\n        \"network\": \"20416\"\n    }]\n}"
    >>,
    {200, [
        {"cache-control","max-age=1"},
        {"connection","keep-alive"},
        {"date","Thu, 13 May 2021 10:32:53 GMT"},
        {"server","nginx"},
        {"content-length","258"},
        {"content-type","application/json"},
        {"x-frame-options","deny"},
        {"x-xss-protection","1; mode=block;"},
        {"strict-transport-security", "max-age=31536000; includeSubdomains"},
        {"content-disposition","attachment; filename=\"api.txt\""}, 
        {"x-nexmo-trace-id","0feb8e00eb8eb445ae7be4798fdcf9f9"}
    ], Content}.

nexmo_sms_test() ->
    {ok, PID} = start_server(nexmo_ok),
    application:set_env(bellboy, nexmo_url_msg, "http://localhost:8000"),
    {ok, #{body := RawBody, code := 200, response := {{"HTTP/1.1", 200, "OK"}, [_|_], RawBody}}} =
        bellboy:nexmo(#{
            type => send_sms,
            from => <<"Vonage APIs">>,
            to => <<"34666666666">>,
            api_key => <<"01234567">>,
            api_secret => <<"0123456789abcdef">>,
            text => <<"Your activation code is XXXX">>
        }),
    Body = jsone:decode(iolist_to_binary(RawBody)),
    ?assertEqual(
        #{
            <<"message-count">> => <<"1">>,
            <<"messages">> => [#{
                <<"message-id">> => <<"14000001A4ECACC7">>,
                <<"message-price">> => <<"0.07950000">>,
                <<"network">> => <<"20416">>,
                <<"remaining-balance">> => <<"1.92050000">>,
                <<"status">> => <<"0">>,
                <<"to">> => <<"34666666666">>}
            ]
        },
        Body
    ),
    receive
        [{method, 'POST'}|_] -> ok
    after 1000 -> throw(timeout)
    end,
    bellboy_http_server:stop(PID),
    ok.
