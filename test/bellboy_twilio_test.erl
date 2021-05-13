-module(bellboy_twilio_test).

-include_lib("eunit/include/eunit.hrl").

start_server(Kind) ->
    Parent = self(),
    bellboy_http_server:start(
        fun(S, Req) -> http_handler(Kind, Parent, S, Req) end,
        8001
    ).

http_handler(twilio_ok, PID, _Socket, Request) ->
    PID ! Request,
    Content = <<
        "{\"sid\": \"SS01234567890123456789012345678901\", \"date_created\": "
        "\"Thu, 13 May 2021 10:56:54 +0000\", \"date_updated\": "
        "\"Thu, 13 May 2021 10:56:54 +0000\", \"date_sent\": null, "
        "\"account_sid\": \"AC0123456789abcdef01234567890abcde\", "
        "\"to\": \"+34666666666\", \"from\": \"+11111111111\", "
        "\"messaging_service_sid\": null, \"body\": \"Your activation code is XXX\", "
        "\"status\": \"queued\", \"num_segments\": \"1\", "
        "\"num_media\": \"0\", \"direction\": \"outbound-api\", "
        "\"api_version\": \"2010-04-01\", \"price\": null, \"price_unit\": \"USD\", "
        "\"error_code\": null, \"error_message\": null, \"uri\": "
        "\"/2010-04-01/Accounts/AC0123456789abcdef01234567890abcde/Messages/"
        "SS01234567890123456789012345678901.json\", \"subresource_uris\": "
        "{\"media\": \"/2010-04-01/Accounts/AC0123456789abcdef01234567890abcde/Messages"
        "/SS01234567890123456789012345678901/Media.json\"}}"
    >>,
    {200, [
        {"connection","keep-alive"},
        {"date","Thu, 13 May 2021 10:56:54 GMT"},
        {"content-length","791"},
        {"content-type","application/json"},
        {"twilio-concurrent-requests","1"},
        {"twilio-request-id","RQ01234567890abcdef0123456789abcde"},
        {"twilio-request-duration","0.098"}, 
        {"access-control-allow-origin","*"},
        {"access-control-allow-headers",
            "Accept, Authorization, Content-Type, If-Match, If-Modified-Since, If-None-Match, If-Unmodified-Since"},
        {"access-control-allow-methods", "GET, POST, DELETE, OPTIONS"},
        {"access-control-expose-headers","ETag"},
        {"access-control-allow-credentials","true"},
        {"x-powered-by","AT-5000"},
        {"x-shenanigans","none"},
        {"x-home-region","us1"},
        {"x-api-domain","api.twilio.com"},
        {"strict-transport-security","max-age=31536000"}
    ], Content}.

twilio_sms_test() ->
    {ok, PID} = start_server(twilio_ok),
    application:set_env(bellboy, twilio_url_msg, "http://localhost:8001/~s"),
    {ok, #{body := RawBody, code := 200, response := {{"HTTP/1.1", 200, "OK"}, [_|_], RawBody}}} =
        bellboy:twilio(#{
            type => send_message,
            account_sid => "0123456789abcdef0123456789abcdef01",
            auth_token => "0123456789abcdef0123456789abcdef",
            from => "11111111111",
            to => "31666666666",
            body => "Your activation code is XXXX"
        }),
    Body = jsone:decode(iolist_to_binary(RawBody)),
    ?assertMatch(
        #{
            <<"account_sid">> := <<"AC0123456789abcdef01234567890abcde">>,
            <<"api_version">> := <<"2010-04-01">>,
            <<"body">> := <<"Your activation code is XXX">>,
            <<"date_created">> := <<"Thu, 13 May 2021 10:56:54 +0000">>,
            <<"date_sent">> := null,
            <<"date_updated">> := <<"Thu, 13 May 2021 10:56:54 +0000">>,
            <<"direction">> := <<"outbound-api">>,
            <<"error_code">> := null,
            <<"error_message">> := null,
            <<"from">> := <<"+11111111111">>,
            <<"messaging_service_sid">> := null,
            <<"num_media">> := <<"0">>,
            <<"num_segments">> := <<"1">>,
            <<"price">> := null,
            <<"price_unit">> := <<"USD">>,
            <<"sid">> := <<"SS01234567890123456789012345678901">>,
            <<"status">> := <<"queued">>,
            <<"subresource_uris">> := #{<<"media">> := <<"/2010-04-01/", _/binary>>},
            <<"to">> := <<"+34666666666">>,
            <<"uri">> := <<"/2010-04-01/Accounts/AC0123456789abcdef01234567890abcde/Messages/SS01234567890123456789012345678901.json">>
        },
        Body
    ),
    receive
        [{method, 'POST'}|_] -> ok;
        Other -> throw({no_match, Other})
    after 1000 -> throw(timeout)
    end,
    bellboy_http_server:stop(PID),
    ok.
