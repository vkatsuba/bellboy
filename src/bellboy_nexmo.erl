-module(bellboy_nexmo).

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

message(#{type := send_sms} = Data) ->
  send_sms(maps:without([type], Data));

message(#{type := send_pin} = Data) ->
  send_pin(maps:without([type], Data));

message(#{type := cancel_pin} = Data) ->
  cancel_pin(maps:without([type], Data));

message(#{type := check_pin} = Data) ->
  check_pin(maps:without([type], Data));

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
-spec send_sms(Params :: maps:map()) -> {ok, Result :: maps:map()} | {error, Reason :: tuple() | bad_arg}.

send_sms(#{from := F, text := TXT, to := T, api_key := AK, api_secret := AC} = B) ->
  case bellboy_utils:is_valid([is_binary(F), is_binary(TXT), is_binary(T), is_binary(AK), is_binary(AC)]) of
    true ->
      RD = #{m => post, u => ?NEXMO_URL_MSG, ct => "application/json", b => jiffy:encode(B)},
      case bellboy_utils:httpc_request(RD) of
        {ok, Resp} ->
          {ok, #{code => bellboy_utils:get_code(Resp), body => bellboy_utils:gen_body(bellboy_utils:get_body(Resp)), response => Resp}};
        Error ->
          Error
      end;
    false ->
      ?BAD_ARG
  end;

send_sms(_) ->
  ?BAD_ARG.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Send PIN SMS
%% @end
%% -------------------------------------------------------------------
-spec send_pin(Params :: maps:map()) -> {ok, Result :: maps:map()} | {error, Reason :: tuple() | bad_arg}.

send_pin(#{api_key := AK, api_secret := AC, number := N, brand := B, code_length := CL}) ->
  case bellboy_utils:is_valid([is_list(AK), is_list(AC), is_list(N), is_list(B), is_list(CL)]) of
    true ->
      URI = "?api_key=" ++ http_uri:encode(AK) ++ "&api_secret=" ++ http_uri:encode(AC) ++ "&number=" ++ http_uri:encode(N) ++ "&brand=" ++ http_uri:encode(B) ++ "&code_length=" ++ http_uri:encode(CL),
      RD = #{m => get, u => ?NEXMO_URL_VERIFY ++ URI},
      case bellboy_utils:httpc_request(RD) of
        {ok, Resp} ->
          {ok, #{code => bellboy_utils:get_code(Resp), body => bellboy_utils:gen_body(bellboy_utils:get_body(Resp)), response => Resp}};
        Error ->
          Error
      end;
    false ->
      ?BAD_ARG
  end;

send_pin(_) ->
  ?BAD_ARG.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Cancel PIN SMS
%% @end
%% -------------------------------------------------------------------
-spec cancel_pin(Params :: maps:map()) -> {ok, Result :: maps:map()} | {error, Reason :: tuple() | bad_arg}.

cancel_pin(#{api_key := AK, api_secret := AC, request_id := RID}) when is_list(AK), is_list(AC), is_list(RID) ->
  URI = "?api_key=" ++ http_uri:encode(AK) ++ "&api_secret=" ++ http_uri:encode(AC) ++ "&cmd=cancel" ++ "&request_id=" ++ http_uri:encode(RID),
  RD = #{m => get, u => ?NEXMO_URL_CONTROL ++ URI},
  case bellboy_utils:httpc_request(RD) of
    {ok, Resp} ->
      {ok, #{code => bellboy_utils:get_code(Resp), body => bellboy_utils:gen_body(bellboy_utils:get_body(Resp)), response => Resp}};
    Error ->
      Error
  end;

cancel_pin(_) ->
  ?BAD_ARG.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Check PIN SMS
%% @end
%% -------------------------------------------------------------------
-spec check_pin(Params :: maps:map()) -> {ok, Result :: maps:map()} | {error, Reason :: tuple() | bad_arg}.

check_pin(#{api_key := AK, api_secret := AC, request_id := RID, code := C}) when is_list(AK), is_list(AC), is_list(RID), is_list(C) ->
  URI = "?api_key=" ++ http_uri:encode(AK) ++ "&api_secret=" ++ http_uri:encode(AC) ++ "&request_id=" ++ http_uri:encode(RID) ++ "&code=" ++ http_uri:encode(C),
  RD = #{m => get, u => ?NEXMO_URL_CHECK ++ URI},
  case bellboy_utils:httpc_request(RD) of
    {ok, Resp} ->
      {ok, #{code => bellboy_utils:get_code(Resp), body => bellboy_utils:gen_body(bellboy_utils:get_body(Resp)), response => Resp}};
    Error ->
      Error
  end;

check_pin(_) ->
  ?BAD_ARG.
