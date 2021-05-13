%% MIT License

%% Copyright (c) 2021 Viacheslav Katsuba <v.katsuba.dev@gmail.com>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(bellboy_nexmo).

-export([message/1]).

%% @private
%% @doc
%% Plivo handler
%% @end

message(#{type := send_sms} = Data) ->
    send_sms(maps:without([type], Data));
message(#{type := send_pin} = Data) ->
    send_pin(maps:without([type], Data));
message(#{type := cancel_pin} = Data) ->
    cancel_pin(maps:without([type], Data));
message(#{type := check_pin} = Data) ->
    check_pin(maps:without([type], Data));
message(_) ->
    {error, bad_arg}.

%% @private
%% @doc
%% Send simple SMS message
%% @end
-spec send_sms(Params :: maps:map()) ->
    {ok, Result :: maps:map()} |
    {error, Reason :: tuple() | bad_arg}.

send_sms(#{from := F, text := TXT, to := T, api_key := AK, api_secret := AC} = B)
        when is_binary(F)
        andalso is_binary(TXT)
        andalso is_binary(T)
        andalso is_binary(AK)
        andalso is_binary(AC) ->
    RD = #{
        m => post,
        u => nexmo_url_msg(),
        ct => "application/json",
        b => jsone:encode(B)
    },
    case bellboy_http:httpc_request(RD) of
        {ok, Resp} ->
            {ok, #{
                code => bellboy_http:get_code(Resp),
                body => bellboy_http:gen_body(bellboy_http:get_body(Resp)),
                response => Resp
            }};

        Error ->
            Error
    end;

send_sms(_) ->
    {error, bad_arg}.

%% @private
%% @doc
%% Send PIN SMS
%% @end
-spec send_pin(Params :: maps:map()) ->
    {ok, Result :: maps:map()} |
    {error, Reason :: tuple() | bad_arg}.

send_pin(#{api_key := AK, api_secret := AC, number := N, brand := B, code_length := CL})
        when is_list(AK)
        andalso is_list(AC)
        andalso is_list(N)
        andalso is_list(B)
        andalso is_list(CL) ->
    URI = uri_string:compose_query([
        {"api_key", AK},
        {"api_secret", AC},
        {"number", N},
        {"brand", B},
        {"code_length", CL}
    ]),
    RD = #{
        m => get,
        u => nexmo_url_verify() ++ URI
    },
    case bellboy_http:httpc_request(RD) of
        {ok, Resp} ->
            {ok, #{
                code => bellboy_http:get_code(Resp),
                body => bellboy_http:gen_body(bellboy_http:get_body(Resp)),
                response => Resp
            }};

        Error ->
            Error
    end;

send_pin(_) ->
    {error, bad_arg}.

%% @private
%% @doc
%% Cancel PIN SMS
%% @end
-spec cancel_pin(Params :: maps:map()) ->
    {ok, Result :: maps:map()} |
    {error, Reason :: tuple() | bad_arg}.

cancel_pin(#{api_key := AK, api_secret := AC, request_id := RID}) when is_list(AK), is_list(AC), is_list(RID) ->
    URI = uri_string:compose_query([
        {"api_key", AK},
        {"api_secret", AC},
        {"cmd", "cancel"},
        {"request_id", RID}
    ]),
    RD = #{
        m => get,
        u => nexmo_url_control() ++ URI
    },
    case bellboy_http:httpc_request(RD) of
        {ok, Resp} ->
            {ok, #{
                code => bellboy_http:get_code(Resp),
                body => bellboy_http:gen_body(bellboy_http:get_body(Resp)),
                response => Resp
            }};

        Error ->
            Error
    end;

cancel_pin(_) ->
    {error, bad_arg}.

%% @private
%% @doc
%% Check PIN SMS
%% @end
-spec check_pin(Params :: maps:map()) ->
    {ok, Result :: maps:map()} |
    {error, Reason :: tuple() | bad_arg}.

check_pin(#{api_key := AK, api_secret := AC, request_id := RID, code := C})
        when is_list(AK)
        andalso is_list(AC)
        andalso is_list(RID)
        andalso is_list(C) ->
    URI = uri_string:compose_query([
        {"api_key", AK},
        {"api_secret", AC},
        {"request_id", RID},
        {"code", C}
    ]),
    RD = #{
        m => get,
        u => nexmo_url_check() ++ URI
    },
    case bellboy_http:httpc_request(RD) of
        {ok, Resp} ->
            {ok, #{
                code => bellboy_http:get_code(Resp),
                body => bellboy_http:gen_body(bellboy_http:get_body(Resp)),
                response => Resp
            }};

        Error ->
            Error
    end;

check_pin(_) ->
    {error, bad_arg}.


nexmo_url_msg() ->
    application:get_env(bellboy, nexmo_url_msg).

nexmo_url_verify() ->
    application:get_env(bellboy, nexmo_url_verify).

nexmo_url_control() ->
    application:get_env(bellboy, nexmo_url_control).

nexmo_url_check() ->
    application:get_env(bellboy, nexmo_url_check).
