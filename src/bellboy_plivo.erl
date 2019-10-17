-module(bellboy_plivo).

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
%% Plivo handler
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

%%% ==================================================================
%%% Internal/Private functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Send simple SMS message
%% @end
%% -------------------------------------------------------------------
-spec send_message(Params :: maps:map()) -> {ok, Result :: maps:map()} | {error, Reason :: tuple() | bad_arg}.

send_message(#{auth_id := AID, auth_token := AT} = Data) when is_list(AID), is_list(AT) ->
  P = maps:without([type, auth_id, payload], Data),
  RD = #{m => post, u => ?PLIVO_URL_MSG(AID), h => #{"Authorization" => ?BASIC_AUTH(AID, AT)}, b => jiffy:encode(P), ct => "application/json"},
  case bellboy_utils:httpc_request(RD) of
    {ok, Resp} ->
      {ok, #{code => bellboy_utils:get_code(Resp), body => bellboy_utils:gen_body(bellboy_utils:get_body(Resp)), response => Resp}};
    Error ->
      Error
  end;

send_message(_) ->
  ?BAD_ARG.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Get SMS message
%% @end
%% -------------------------------------------------------------------
-spec get_message(Params :: maps:map()) -> {ok, Result :: maps:map()} | {error, Reason :: tuple() | bad_arg}.

get_message(#{auth_id := AID, auth_token := AT, message_uuid := MUUID}) when is_list(AID), is_list(AT), is_list(MUUID) ->
  RD = #{m => get, u => ?PLIVO_URL_MSG(AID) ++ MUUID, h => #{"Authorization" => ?BASIC_AUTH(AID, AT)}},
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
%% Get SMS messages
%% @end
%% -------------------------------------------------------------------
-spec get_messages(Params :: maps:map()) -> {ok, Result :: maps:map()} | {error, Reason :: tuple() | bad_arg}.

get_messages(#{auth_id := AID, auth_token := AT}) when is_list(AID), is_list(AT) ->
  RD = #{m => get, u => ?PLIVO_URL_MSG(AID), h => #{"Authorization" => ?BASIC_AUTH(AID, AT)}},
  case bellboy_utils:httpc_request(RD) of
    {ok, Resp} ->
      {ok, #{code => bellboy_utils:get_code(Resp), body => bellboy_utils:gen_body(bellboy_utils:get_body(Resp)), response => Resp}};
    Error ->
      Error
  end;

get_messages(_) ->
  ?BAD_ARG.
