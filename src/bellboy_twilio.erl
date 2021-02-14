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

-module(bellboy_twilio).

%%% ==================================================================
%%% API
%%% ==================================================================

-export([message/1]).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include("bellboy.hrl").

%%% ==================================================================
%%% API functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Twilio handler
%% @end
%% -------------------------------------------------------------------

message(#{type := send_message} = Data) ->
  send_message(Data);

message(#{type := get_message} = Data) ->
  get_message(Data);

message(#{type := get_messages} = Data) ->
  get_messages(Data);

message(_) ->
  ?BAD_ARG.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Send simple SMS message
%% @end
%% -------------------------------------------------------------------
-spec send_message(Params :: maps:map()) -> {ok, Result :: maps:map()} | {error, Reason :: tuple() | bad_arg}.

send_message(#{account_sid := AID, auth_token := AT, body := B, from := F, to := T}) ->
  case bellboy_utils:is_valid([is_list(AID), is_list(AT), is_list(B), is_list(F), is_list(T)]) of
    true ->
      BURI = "Body=" ++ http_uri:encode(B) ++ "&From=" ++ http_uri:encode(F) ++ "&To=" ++ http_uri:encode(T),
      RD = #{m => post, u => ?TWILIO_URL_MSG(AID), h => #{"Authorization" => ?BASIC_AUTH(AID, AT)}, ct => "application/x-www-form-urlencoded", b => BURI},
      case bellboy_utils:httpc_request(RD) of
        {ok, Resp} ->
          {ok, #{code => bellboy_utils:get_code(Resp), body => bellboy_utils:gen_body(bellboy_utils:get_body(Resp)), response => Resp}};
        Error ->
          Error
      end;
    false ->
      ?BAD_ARG
  end;

send_message(_) ->
  ?BAD_ARG.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Get specific SMS message
%% @end
%% -------------------------------------------------------------------
-spec get_message(Params :: maps:map()) -> {ok, Result :: maps:map()} | {error, Reason :: tuple() | bad_arg}.

get_message(#{account_sid := AID, auth_token := AT, sid := SID}) when is_list(AID), is_list(AT), is_list(SID) ->
  RD = #{m => get, u => ?TWILIO_URL_SPEC_MSG(AID, SID), h => #{"Authorization" => ?BASIC_AUTH(AID, AT)}},
  case bellboy_utils:httpc_request(RD) of
    {ok, Resp} ->
      {ok, #{code => bellboy_utils:get_code(Resp), body => bellboy_utils:gen_body(bellboy_utils:get_body(Resp)), response => Resp}};
    Error ->
      Error
  end;

get_message(_) ->
  ?BAD_ARG.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Get all SMS messages
%% @end
%% -------------------------------------------------------------------
-spec get_messages(Params :: maps:map()) -> {ok, Result :: maps:map()} | {error, Reason :: tuple() | bad_arg}.

get_messages(#{account_sid := AID, auth_token := AT}) when is_list(AID), is_list(AT) ->
  RD = #{m => get, u => ?TWILIO_URL_MSG(AID), h => #{"Authorization" => ?BASIC_AUTH(AID, AT)}},
  case bellboy_utils:httpc_request(RD) of
    {ok, Resp} ->
      {ok, #{code => bellboy_utils:get_code(Resp), body => bellboy_utils:gen_body(bellboy_utils:get_body(Resp)), response => Resp}};
    Error ->
      Error
  end;

get_messages(_) ->
  ?BAD_ARG.
