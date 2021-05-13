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

-module(bellboy_plivo).

-export([message/1]).

%% @private
%% @doc
%% Plivo handler
%% @end

message(#{type := send_message} = Data) ->
    send_message(Data);

message(#{type := get_message} = Data) ->
    get_message(Data);

message(#{type := get_messages} = Data) ->
    get_messages(Data);

message(_) ->
    {error, bad_arg}.

%% @private
%% @doc
%% Send simple SMS message
%% @end
-spec send_message(Params :: maps:map()) ->
    {ok, Result :: maps:map()} |
    {error, Reason :: tuple() | bad_arg}.

send_message(#{auth_id := AID, auth_token := AT} = Data)
        when is_list(AID)
        andalso is_list(AT) ->
    P = maps:without([type, auth_id, payload], Data),
    RD = #{
        m => post,
        u => plivo_url_msg(AID),
        h => #{"Authorization" => bellboy_http:basic_auth(AID, AT)},
        b => jsone:encode(P),
        ct => "application/json"
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

send_message(_) ->
    {error, bad_arg}.

%% @private
%% @doc
%% Get SMS message
%% @end
-spec get_message(Params :: maps:map()) ->
    {ok, Result :: maps:map()} |
    {error, Reason :: tuple() | bad_arg}.

get_message(#{auth_id := AID, auth_token := AT, message_uuid := MUUID})
        when is_list(AID)
        andalso is_list(AT)
        andalso is_list(MUUID) ->
    RD = #{
        m => get,
        u => plivo_url_msg(AID) ++ MUUID,
        h => #{"Authorization" => bellboy_http:basic_auth(AID, AT)}
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

get_message(_) ->
    {error, bad_arg}.

%% @private
%% @doc
%% Get SMS messages
%% @end
-spec get_messages(Params :: maps:map()) ->
    {ok, Result :: maps:map()} |
    {error, Reason :: tuple() | bad_arg}.

get_messages(#{auth_id := AID, auth_token := AT})
        when is_list(AID)
        andalso is_list(AT) ->
    RD = #{
        m => get,
        u => plivo_url_msg(AID),
        h => #{"Authorization" => bellboy_http:basic_auth(AID, AT)}
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

get_messages(_) ->
    {error, bad_arg}.

plivo_url_msg(AuthID) ->
    Format = application:get_env(bellboy, plivo_url_msg, undefined),
    io_lib:format(Format, [AuthID]).
